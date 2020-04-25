
pres_results <- read.csv("raw_data/countypres_2000-2016.csv") %>%
  clean_names() %>%
  select(! version)

pres_results <- pres_results %>%
  mutate(abb_county = paste0(county, ", ", state_po))

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

age_income_2016_18_over <- age_income_2016 %>%
  head(13) %>%
  select(! age_group) %>%
  drop_na()

age_income_2016_18_24 <- age_income_2016[14:25,] %>%
  select(! age_group) %>%
  drop_na()

age_income_2016_25_44 <- age_income_2016[26:37,] %>%
  select(! age_group) %>%
  drop_na()

age_income_2016_45_64 <- age_income_2016[38:49,] %>%
  select(! age_group) %>%
  drop_na()

age_income_2016_65_74 <- age_income_2016[50:61,] %>%
  select(! age_group) %>%
  drop_na()

age_income_2016_75_over <- age_income_2016[62:73,] %>%
  select(! age_group) %>%
  drop_na()

saveRDS(object = age_income_2016, file = "final_project/rds/age_income_2016.RDS")

saveRDS(object = age_income_2016_18_over, file = "final_project/rds/age_income_2016_18_over.RDS")

saveRDS(object = age_income_2016_18_24, file = "final_project/rds/age_income_2016_18_24.RDS")

saveRDS(object = age_income_2016_25_44, file = "final_project/rds/age_income_2016_25_44.RDS")

saveRDS(object = age_income_2016_45_64, file = "final_project/rds/age_income_2016_45_64.RDS")

saveRDS(object = age_income_2016_65_74, file = "final_project/rds/age_income_2016_65_74.RDS")

saveRDS(object = age_income_2016_75_over, file = "final_project/rds/age_income_2016_75_over.RDS")

income_county <- read_excel("raw_data/per_capita_income_by_county.xlsx") %>%
  clean_names()

income_county <- income_county %>%
  rename(
    "county" = table_1_per_capita_personal_income_by_county_2016_2018,
    "personal_income_2016" = x2,
    "personal_income_2017" = x3,
    "personal_income_2018" = x4,
    "personal_income_rank_state_2018" = x5,
    "pct_change_2017" = x6,
    "pct_change_2018" = x7,
    "rank_in_state_pct_change" = x8
  )

income_county <- income_county[4:3223, 1:5]

saveRDS(object = income_county, file = "final_project/rds/income_county.RDS")












