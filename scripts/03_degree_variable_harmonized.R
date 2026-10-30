#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 03_degree_variable_harmonized
#' -----------------------------
library(haven)
library(dplyr)
library(readr)

w09 <- read_dta("data/raw/issp/2009/ZA5400_v4-0-0.dta")
w19 <- read_dta("data/raw/issp/2019/ZA7600_v3-0-0.dta")
names(w09) <- tolower(names(w09))
names(w19) <- tolower(names(w19))
cw <- read_csv("data/docs/degree_crosswalk_final.csv", show_col_types = FALSE)

#' Country codes covered in each wave (same as in the crosswalk)
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
resp <- bind_rows(
  lapply(isos_2009, function(i) resp_one(w09, i, 2009, "v3")),
  lapply(isos_2019, function(i) resp_one(w19, i, 2019, "caseid"))
)

#' Above code keeps their ID, their national degree code and their DEGREE. 
#' The two lapply calls just run it over every country and stack the results. 
#' It is a loop written in short form.

#'  Join the crosswalk, with a guard that no rows were added or lost
n_before <- nrow(resp)
resp <- resp %>%
  left_join(cw %>% select(wave, iso, nat_code, degree_original, harmonized_tertiary),
            by = c("wave", "iso", "nat_code", "degree_original"))
stopifnot(nrow(resp) == n_before)
resp <- resp %>%
  mutate(harmonized_tertiary = if_else(degree_original %in% 0:6, harmonized_tertiary, NA_real_))
stopifnot(!anyDuplicated(resp[c("wave", "iso", "id")]))

#' Attach to the merged data (one-to-one on wave, iso, id) and rename
merged <- readRDS("data/interim/issp_merged.rds")
harm <- resp %>% select(wave, iso, id, harmonized_tertiary)
analysis <- merged %>%
  left_join(harm, by = c("wave", "iso", "id"), relationship = "many-to-one")

stopifnot(nrow(analysis) == nrow(merged))
stopifnot(sum(!is.na(analysis$harmonized_tertiary)) == sum(!is.na(harm$harmonized_tertiary)))

analysis <- analysis %>%
  rename(credential_degree5 = credential, credential = harmonized_tertiary)
saveRDS(analysis, "data/interim/issp_analysis.rds")