#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 02_degree_variable_built
#' ------------------------

#' ONE-OFF. Already run on 2026-10-28; the filled file is degree_crosswalk_final.csv.
#' Writes only to degree_crosswalk_blank.csv. Never write to the final file.

library(haven)
library(dplyr)
library(readr)

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

write_csv(crosswalk, "data/docs/degree_crosswalk_blank.csv")

crosswalk %>% group_by(wave) %>% summarise(sum(n))

