
pres_results <- read.csv("raw_data/countypres_2000-2016.csv") %>%
  clean_names()

saveRDS(object = pres_results, file = "final_project/rds/pres_results.RDS")

acs <- read.csv("raw_data/acs_5ye.csv") %>%
  clean_names()

saveRDS(object = acs, file = "final_project/rds/acs.RDS")

health <- read.csv("raw_data/health_data_2016.csv") %>%
  clean_names() %>%
  select(! 3:6)

saveRDS(object = health, file = "final_project/rds/health.RDS")

education <- read.csv("raw_data/state_education_data_2018.csv") %>%
  clean_names() %>%
  select(! 3:7)

saveRDS(object = education, file = "final_project/rds/education.RDS")

age_income_2016_n <- read_excel("raw_data/family_voting_registration_by_age_and_income_2016.xlsx", skip = 5, col_types = "numeric") %>%
  clean_names() %>%
  select(! c(x1, x2))

age_income_2016_c <- read_excel("raw_data/family_voting_registration_by_age_and_income_2016.xlsx", skip = 5) %>%
  clean_names() %>%
  select(c(x1, x2))

age_income_2016 <- cbind(age_income_2016_c, age_income_2016_n)

age_income_2016 <- age_income_2016 %>%
  rename(
    "age_group" = x1,
    "total_income" = x2,
    "total_population" = x3,
    "reported_registered" = reported_registered_5,
    "reported_registered_percent" = x6,
    "reported_not_registered_percent" = x8,
    "no_response_to_registration" = no_response_to_registration1,
    "no_response_to_registration_percent" = x10,
    "reported_voted" = reported_voted_11,
    "reported_voted_percent" = x12,
    "reported_not_voted_percent" = x14,
    "no_response_to_voting" = no_response_to_voting2,
    "no_response_to_voting_percent" = x16,
    "registered_total_percent" = reported_registered_17,
    "voted_total_percent" = reported_voted_18
  )

saveRDS(object = age_income_2016, file = "final_project/rds/age_income_2016.RDS")
