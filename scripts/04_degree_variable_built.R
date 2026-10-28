#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 04_degree_variable
#' -----------------

library(haven)
library(dplyr)
library(readr)
library(tidyr)

w09 <- read_dta("data/raw/issp/2009/ZA5400_v4-0-0.dta") 
w19 <- read_dta("data/raw/issp/2019/ZA7600_v3-0-0.dta")
names(w09) <- tolower(names(w09))
names(w19) <- tolower(names(w19))

tab_one <- function(d, iso, wave) {
  nat  <- paste0(tolower(iso), "_degr")
  sub  <- d[startsWith(as.character(d$c_alphan), iso), ]
  labs <- attr(d[[nat]], "labels")                
  sub %>%
    transmute(nat_code        = as.numeric(.data[[nat]]),
              degree_original = as.numeric(degree),
              educyrs         = as.numeric(educyrs)) %>%
    group_by(nat_code, degree_original) %>%
    summarise(n = n(),
              median_educyrs = median(educyrs[educyrs >= 0 & educyrs <= 30]),
              .groups = "drop") %>%
    mutate(wave = wave, iso = iso,
           nat_label = names(labs)[match(nat_code, labs)])
}

tab_one(w19, "DK", 2019)    


isos_2009 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","HU","LV","PL","SK","ES")
isos_2019 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","AU","DE","JP","NZ","US")

cw09 <- bind_rows(lapply(isos_2009, function(i) tab_one(w09, i, 2009)))
cw19 <- bind_rows(lapply(isos_2019, function(i) tab_one(w19, i, 2019)))

crosswalk <- bind_rows(cw09, cw19) %>%
  mutate(degree_cumulation = if_else(wave == 2019 & degree_original == 6, 5, degree_original),
         harmonized_tertiary = NA_real_,
         note = "")

write_csv(crosswalk, "data/codebooks/degree_crosswalk_blank.csv")

crosswalk %>% group_by(wave) %>% summarise(sum(n))

#' -----------
#' HARMONIZING
#' -----------

w09 <- read_dta("data/raw/issp/2009/ZA5400_v4-0-0.dta")
w19 <- read_dta("data/raw/issp/2019/ZA7600_v3-0-0.dta")
names(w09) <- tolower(names(w09))
names(w19) <- tolower(names(w19))
cw <- read_csv("data/codebooks/degree_crosswalk_final.csv", show_col_types = FALSE)

#" Country codes covered in each wave (same as in the crosswalk)
isos_2009 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","HU","LV","PL","SK","ES")
isos_2019 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","AU","DE","JP","NZ","US")

#' Respondent table: id is v3 in 2009 and caseid in 2019
resp_one <- function(d, iso, wave, idvar) {
  nat <- paste0(tolower(iso), "_degr")
  d[startsWith(as.character(d$c_alphan), iso), ] %>%
    transmute(wave = wave, iso = iso, id = as.numeric(.data[[idvar]]),
              nat_code = as.numeric(.data[[nat]]),
              degree_original = as.numeric(degree))
}
#' Above code keeps their ID, their national degree code and their DEGREE. 
#' The two lapply calls just run it over every country and stack the results. 
#' It is a loop written in short form.

resp <- bind_rows(
  lapply(isos_2009, function(i) resp_one(w09, i, 2009, "v3")),
  lapply(isos_2019, function(i) resp_one(w19, i, 2019, "caseid"))
)


#'  Join the crosswalk, with a guard that no rows were added or lost
n_before <- nrow(resp)
resp <- resp %>%
  left_join(cw %>% select(wave, iso, nat_code, degree_original, harmonized_tertiary), # attaches 0/1 decision to each respondent
            by = c("wave", "iso", "nat_code", "degree_original"))
stopifnot(nrow(resp) == n_before) 
resp <- resp %>%
  mutate(harmonized_tertiary = if_else(degree_original %in% 0:6, harmonized_tertiary, NA_real_))

resp %>% filter(degree_original %in% 0:6, is.na(harmonized_tertiary)) %>% count(wave, iso)  # empty
anyDuplicated(resp[c("wave", "iso", "id")]) 

#' Validate the ID against the cumulation: the tertiary split must agree exactly
issp_raw <- readRDS("data/interim/issp_raw.rds")
b <- issp_raw %>%
  filter(year %in% c(2009, 2019)) %>%
  transmute(wave = year,
            iso = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id = as.numeric(caseid),
            degree_cum = as.numeric(degree)) %>%
  zap_labels()
j <- inner_join(resp, b, by = c("wave", "iso", "id"), relationship = "one-to-one")
nrow(j)                                                         # 53,213
ok <- j$degree_original %in% 0:6 & j$degree_cum %in% 0:5
mean((if_else(j$degree_original == 6, 5, j$degree_original)[ok] == 5) == (j$degree_cum[ok] == 5))  # 1


#' Compare old (DEGREE >= 5) and new (harmonized) share by country and wave
comparison <- resp %>%
  group_by(iso, wave) %>%
  summarise(old = mean(degree_original[degree_original %in% 0:6] >= 5),
            new = mean(harmonized_tertiary, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = wave, values_from = c(old, new))
print(comparison, n = 25)
write_csv(comparison, "output/tables/credential_old_vs_new.csv")

#' Save the lookup for 01_build_data.R
harm <- resp %>% select(wave, iso, id, harmonized_tertiary)
stopifnot(nrow(harm) == 53213, !anyDuplicated(harm[c("wave", "iso", "id")]))
saveRDS(harm, "data/interim/harmonized_tertiary.rds")

