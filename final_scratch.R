
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

grad_ed <- read_excel("raw_data/grad_rates_school_district.xlsx") %>%
  clean_names()

grad_ed <- grad_ed[12:3344,]

grad_ed <- grad_ed %>%
  dplyr::rename(
    "percent" = data,
    "school_district" = location,
    "year" = time_frame
  ) %>%
  select(c(school_district, year, percent))

saveRDS(object = grad_ed, file = "final_project/rds/grad_ed.RDS")

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

income_county <- income_county %>%
  mutate(ID = seq.int(nrow(income_county)))

income_county <- income_county %>%
  mutate(state_po = ifelse(income_county$ID <= 68, "AL", 
                           ifelse(between(ID, 70, 99), "AK", 
                                  ifelse(between(ID, 102, 116), "AZ",
                                         ifelse(between(ID, 120, 193), "AR",
                                                ifelse(between(ID, 196, 253), "CA",
                                                       ifelse(between(ID, 256, 319), "CO",
                                                              ifelse(between(ID, 322, 329), "CT",
                                                                     ifelse(between(ID, 332, 334), "DE",
                                                                            ifelse(between(ID, 336, 336), "DC",
                                                                                   ifelse(between(ID, 339, 405), "FL",
                                                                                          ifelse(between(ID, 408, 556), "GA",
                                                                                                 ifelse(between(ID, 570, 572), "HI",
                                                                                                        ifelse(between(ID, 575, 618), "ID",
                                                                                                               ifelse(between(ID, 621, 722), "IL",
                                                                                                                      ifelse(between(ID, 725, 816), "IN",
                                                                                                                             ifelse(between(ID, 819, 917), "IA",
                                                                                                                                    ifelse(between(ID, 920, 1024), "KS",
                                                                                                                                           ifelse(between(ID, 1027, 1146), "KY",
                                                                                                                                                  ifelse(between(ID, 1149, 1212), "LA",
                                                                                                                                                         ifelse(between(ID, 1215, 1230), "MD",
                                                                                                                                                                ifelse(between(ID, 1233, 1256), "MA",
                                                                                                                                                                       ifelse(between(ID, 1275, 1357), "MI",
                                                                                                                                                                              ifelse(between(ID, 1360, 1446), "MN",
                                                                                                                                                                                     ifelse(between(ID, 1449, 1530), "MS",
                                                                                                                                                                                            ifelse(between(ID, 1533, 1647), "MO",
                                                                                                                                                                                                   ifelse(between(ID, 1650, 1705), "MT",
                                                                                                                                                                                                          ifelse(between(ID, 1708, 1800), "NE",
                                                                                                                                                                                                                 ifelse(between(ID, 1803, 1819), "NV",
                                                                                                                                                                                                                        ifelse(between(ID, 1822, 1831), "NH",
                                                                                                                                                                                                                               ifelse(between(ID, 1834, 1854), "NJ",
                                                                                                                                                                                                                                      ifelse(between(ID, 1857, 1889), "NM",
                                                                                                                                                                                                                                             ifelse(between(ID, 1892, 1953), "NY",
                                                                                                                                                                                                                                                    ifelse(between(ID, 1956, 2055), "NC",
                                                                                                                                                                                                                                                           ifelse(between(ID, 2058, 2110), "ND",
                                                                                                                                                                                                                                                                  ifelse(between(ID, 2113, 2200), "OH",
                                                                                                                                                                                                                                                                         ifelse(between(ID, 2203, 2279), "OK",
                                                                                                                                                                                                                                                                                ifelse(between(ID, 2282, 2317), "OR",
                                                                                                                                                                                                                                                                                       ifelse(between(ID, 2320, 2386), "PA",
                                                                                                                                                                                                                                                                                              ifelse(between(ID, 2389, 2393), "RI",
                                                                                                                                                                                                                                                                                                     ifelse(between(ID, 2396, 2441), "SC",
                                                                                                                                                                                                                                                                                                            ifelse(between(ID, 2444, 2509), "SD",
                                                                                                                                                                                                                                                                                                                   ifelse(between(ID, 2512, 2606), "TN",
                                                                                                                                                                                                                                                                                                                          ifelse(between(ID, 2609, 2862), "TX",
                                                                                                                                                                                                                                                                                                                                 ifelse(between(ID, 2865, 2893), "UT",
                                                                                                                                                                                                                                                                                                                                        ifelse(between(ID, 2896, 2909), "VT",
                                                                                                                                                                                                                                                                                                                                               ifelse(between(ID, 2912, 2994), "VA",
                                                                                                                                                                                                                                                                                                                                                      ifelse(between(ID, 3021, 3059), "WA",
                                                                                                                                                                                                                                                                                                                                                             ifelse(between(ID, 3062, 3116), "WV", "NA")))))))))))))))))))))))))))))))))))))))))))))))))

income_county$state_po[3119:3190] <- "WI"

income_county$state_po[3193:3215] <- "WY"

income_county_ns <- income_county %>%
  drop_na() %>%
  mutate(abb_county = paste0(county, ", ", state_po))

ic_2016 <- income_county_ns %>%
  select(county, personal_income_2016) %>%
  rename(
    "y" = personal_income_2016
  )

ic_2017 <- income_county_ns %>%
  select(county, personal_income_2017) %>%
  rename(
    "y" = personal_income_2017
  )

ic_2018 <- income_county_ns %>%
  select(county, personal_income_2018) %>%
  rename(
    "y" = personal_income_2018
  )

ic_rank_2018 <- income_county_ns %>%
  select(county, personal_income_rank_state_2018) %>%
  rename(
    "y" = personal_income_rank_state_2018
  )

saveRDS(object = income_county_ns, file = "final_project/rds/income_county_ns.RDS")

saveRDS(object = ic_2016, file = "final_project/rds/ic_2016.RDS")

saveRDS(object = ic_2017, file = "final_project/rds/ic_2017.RDS")

saveRDS(object = ic_2018, file = "final_project/rds/ic_2018.RDS")

saveRDS(object = ic_rank_2018, file = "final_project/rds/ic_rank_2018.RDS")






