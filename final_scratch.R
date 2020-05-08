
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

grad_ed$percent <- as.numeric(grad_ed$percent)

grad_ed$year <- as.numeric(grad_ed$year)

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

income_county_ns$personal_income_2016 <- as.numeric(income_county_ns$personal_income_2016)

income_county_ns$personal_income_rank_state_2018 <- as.numeric(income_county_ns$personal_income_rank_state_2018)

ic_2016 <- income_county_ns %>%
  select(abb_county, personal_income_2016) %>%
  rename(
    "y" = personal_income_2016
  )

ic_2017 <- income_county_ns %>%
  select(abb_county, personal_income_2017) %>%
  rename(
    "y" = personal_income_2017
  )

ic_2018 <- income_county_ns %>%
  select(abb_county, personal_income_2018) %>%
  rename(
    "y" = personal_income_2018
  )

ic_rank_2018 <- income_county_ns %>%
  select(abb_county, personal_income_rank_state_2018) %>%
  rename(
    "y" = personal_income_rank_state_2018
  )

saveRDS(object = income_county_ns, file = "final_project/rds/income_county_ns.RDS")

saveRDS(object = ic_2016, file = "final_project/rds/ic_2016.RDS")

saveRDS(object = ic_2017, file = "final_project/rds/ic_2017.RDS")

saveRDS(object = ic_2018, file = "final_project/rds/ic_2018.RDS")

saveRDS(object = ic_rank_2018, file = "final_project/rds/ic_rank_2018.RDS")

pres_income <- full_join(pres_results_reg, income_county_ns)

pres_income <- pres_income %>%
  mutate(county_c = paste0(county, " County"))

saveRDS(object = pres_income, file = "final_project/rds/pres_income.RDS")

county_pop <- read_csv("raw_data/estimated_county_population.csv") %>%
  clean_names()

county_pop <- county_pop %>%
  select(c(stname, ctyname, popestimate2016))

county_pop <- county_pop %>%
  dplyr::rename("county_c" = ctyname,
                "state" = stname)

saveRDS(object = county_pop, file = "final_project/rds/county_pop.RDS")

regression <- full_join(county_pop, pres_income, by = c("county_c", "state")) 

regression <- regression %>%
  drop_na() %>%
  mutate(percent_votes = totalvotes/popestimate2016) %>%
  mutate(county_state = paste0(county_c, ", ", state)) %>%
  filter(! county_state == "Richmond County, Virginia")

saveRDS(object = regression, file = "final_project/rds/regression.RDS")

regression_log <- regression %>%
  mutate(log_totalvotes = log(totalvotes), log_personal_income_2016 = log(personal_income_2016))

saveRDS(object = regression_log, file = "final_project/rds/regression_log.RDS")

health <- read.csv("raw_data/health_data_2016.csv") %>%
  clean_names() %>%
  select(! 3:6) %>%
  filter(! qualifying_name == "Geo_QNAME") %>%
  rename("county_state" = qualifying_name,
         "county_c" = name_of_area)

health[,3:29] <- sapply(health[,3:29],as.numeric)

health[,1:2] <- sapply(health[,1:2], as.character)

saveRDS(object = health, file = "final_project/rds/health.RDS")

health_18_punhealthy <- health %>% select(c(county_state, physically_unhealthy_days_per_month_persons_18_years_and_over)) %>%
  rename("y" = physically_unhealthy_days_per_month_persons_18_years_and_over)

saveRDS(object = health_18_punhealthy, file = "final_project/rds/health_18_punhealthy.RDS")

health_18_munhealthy <- health %>% select(c(county_state, mentally_unhealthy_days_per_month_persons_18_years_and_over)) %>%
  rename("y" = mentally_unhealthy_days_per_month_persons_18_years_and_over)

saveRDS(object = health_18_munhealthy, file = "final_project/rds/health_18_munhealthy.RDS")

health_18_pctfairpoor <- health %>% select(c(county_state, percent_of_adults_that_report_fair_or_poor_health_persons_18_years_and_over)) %>%
  rename("y" = percent_of_adults_that_report_fair_or_poor_health_persons_18_years_and_over)

saveRDS(object = health_18_pctfairpoor, file = "final_project/rds/helath_18_pctfairpoor.RDS")

health_primary_phys <- health %>% select(c(county_state, primary_care_physicians_pcp)) %>%
  rename("y" = primary_care_physicians_pcp)

saveRDS(object = health_primary_phys, file = "final_project/rds/health_primary_phys.RDS")

health_primary_phys_rate <- health %>% select(c(county_state, primary_care_physicians_pcp_rate_per_100_000_population)) %>%
  rename("y" = primary_care_physicians_pcp_rate_per_100_000_population)

saveRDS(object = health_primary_phys_rate, file = "final_project/rds/health_primary_phys_rate.RDS")

health_dentists <- health %>% select(c(county_state, dentists)) %>%
  rename("y" = dentists)

saveRDS(object = health_dentists, file = "final_project/rds/health_dentists.RDS")

health_dentists_rate <- health %>% select(c(county_state, dentists_rate_per_100_000_population)) %>%
  rename("y" = dentists_rate_per_100_000_population)

saveRDS(object = health_food_environmental_index, file = "final_project/rds/health_dentists_rate.RDS")

health_costs_adjusted_medicare <- health %>% select(c(county_state, health_care_costs_price_adjusted_medicare_reimbursements)) %>%
  rename("y" = health_care_costs_price_adjusted_medicare_reimbursements)

saveRDS(object = health_costs_adjusted_medicare, file = "final_project/rds/health_costs_adjusted_medicare.RDS")

health_percent_woinsurance_under19 <- health %>% select(c(county_state, percent_of_persons_without_insurance_population_under_19_years_2013_est)) %>%
  rename("y" = percent_of_persons_without_insurance_population_under_19_years_2013_est)

saveRDS(object = health_percent_woinsurance_under19, file = "final_project/rds/health_percent_woinsurance_under19.RDS")

health_percent_woinsurance_18_64 <- health %>% select(c(county_state, percent_of_persons_without_insurance_population_18_to_64_years_2013_est)) %>%
  rename("y" = percent_of_persons_without_insurance_population_18_to_64_years_2013_est)

saveRDS(object = health_percent_woinsurance_18_64, file = "final_project/rds/health_percent_woinsurance_18_64.RDS")

health_infant_mortality <- health %>% select(c(county_state, infant_mortality_death_counts)) %>%
  rename("y" = infant_mortality_death_counts)

saveRDS(object = health_infant_mortality, file = "final_project/rds/health_infant_mortality.RDS")

health_child_mortality <- health %>% select(c(county_state, child_mortality_death_counts)) %>%
  rename("y" = child_mortality_death_counts)

saveRDS(object = health_child_mortality, file = "final_project/rds/health_child_mortality.RDS")

health_pct_smokers <- health %>% select(c(county_state, percent_current_smokers_persons_18_years_and_over)) %>%
  rename("y" = percent_current_smokers_persons_18_years_and_over)

saveRDS(object = health_pct_smokers, file = "final_project/rds/health_pct_smokers.RDS")

health_pct_drinking <- health %>% select(c(county_state, percent_drinking_adults_persons_18_years_and_over)) %>%
  rename("y" = percent_drinking_adults_persons_18_years_and_over)

saveRDS(object = health_pct_drinking, file = "final_project/rds/health_pct_drinking.RDS")

health_food_environmental_index <- health %>% select(c(county_state, food_environment_index)) %>%
  rename("y" = food_environment_index)

saveRDS(object = health_food_environmental_index, file = "final_project/rds/health_food_environmental_index.RDS")


