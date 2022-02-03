library(tidyverse) # this belongs somewhere else but for now it's okay

get_data <- function(){
  fy2020 <- read_csv("data/raw/FY2020_IPEDS.csv")
  fy2019 <- read_csv("data/raw/FY2019_IPEDS.csv")
  fy2018 <- read_csv("data/raw/FY2018_IPEDS.csv")
  fy2017 <- read_csv("data/raw/FY2017_IPEDS.csv")
  fy2016 <- read_csv("data/raw/FY2016_IPEDS.csv")
  
  fy2020 <- fy2020[1:ncol(fy2020)-1]
  fy2019 <- fy2019[1:ncol(fy2019)-1]
  fy2018 <- fy2018[1:ncol(fy2018)-1]
  fy2017 <- fy2017[1:ncol(fy2017)-1]
  fy2016 <- fy2016[1:ncol(fy2016)-1]

  #add year
  fy2020$fyear <- 2020
  fy2019$fyear <- 2019
  fy2018$fyear <- 2018
  fy2017$fyear <- 2017
  fy2016$fyear <- 2016

  #clean column names
  fy2020 <- clean_column_names(fy2020)
  fy2019 <- clean_column_names(fy2019)
  fy2018 <- clean_column_names(fy2018)
  fy2017 <- clean_column_names(fy2017)
  fy2016 <- clean_column_names(fy2016)

  fy2020 <- janitor::clean_names(fy2020)
  fy2019 <- janitor::clean_names(fy2019)
  fy2018 <- janitor::clean_names(fy2018)
  fy2017 <- janitor::clean_names(fy2017)
  fy2016 <- janitor::clean_names(fy2016)

  fy2016 <- fy2016 %>% 
    rename(has_an_academic_library = 
           has_academic_library_access_to_library_collection_and_or_has_library_expenses)

  fy2019$degree_granting_status <- NULL
  fy2018$degree_granting_status <- NULL
  fy2017$degree_granting_status <- NULL
  fy2016$degree_granting_status <- NULL
  
  fy2019$were_annual_total_library_expenses_greater_than_or_equal_to_100_000 <- NULL
  fy2020$were_annual_total_library_expenses_greater_than_or_equal_to_100_000 <- NULL
  
  dat <- bind_rows(fy2020, fy2019, fy2018, fy2017, fy2016)
  dat
}

clean_column_names <- function(dat) {
  oldnames <- colnames(dat) 
  newnames <- str_replace_all(oldnames, "\\([A-Za-z0-9_]*\\)", "")
  newnames <- str_replace_all(newnames, " [0-9]{4}-[0-9]{2}", "")
  newnames <- str_trim(newnames)
  newnames <- as.list(newnames)
  colnames(dat) <- newnames
  dat
}

recode_variables <- function(dat) {
  dat$bureau_of_economic_analysis_regions <- as.factor(case_when(
    dat$bureau_of_economic_analysis_regions == 0 ~ "U.S. Service schools",
    dat$bureau_of_economic_analysis_regions == 1 ~ "New England (CT, ME, MA, NH, RI, VT)",
    dat$bureau_of_economic_analysis_regions == 2 ~ "Mid East (DE, DC, MD, NJ, NY, PA)",
    dat$bureau_of_economic_analysis_regions == 3 ~ "Great Lakes (IL, IN, MI, OH, WI)",
    dat$bureau_of_economic_analysis_regions == 4 ~ "Plains (IA, KS, MN, MO, NE, ND, SD)",
    dat$bureau_of_economic_analysis_regions == 5 ~ "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
    dat$bureau_of_economic_analysis_regions == 6 ~ "Southwest (AZ, NM, OK, TX)",
    dat$bureau_of_economic_analysis_regions == 7 ~ "Rocky Mountains (CO, ID, MT, UT, WY)",
    dat$bureau_of_economic_analysis_regions == 8 ~ "Far West (AK, CA, HI, NV, OR, WA)",
    dat$bureau_of_economic_analysis_regions == 9 ~ "Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, VI)"
  ))
  
  dat$sector_of_institution <- as.factor(case_when(
    dat$sector_of_institution == 0 ~ "Administrative Unit",
    dat$sector_of_institution == 1 ~ "Public, 4-year or above",
    dat$sector_of_institution == 2 ~ "Private not-for-profit, 4-year or above",
    dat$sector_of_institution == 3 ~ "Private for-profit, 4-year or above",
    dat$sector_of_institution == 4 ~ "Public, 2-year",
    dat$sector_of_institution == 5 ~ "Private not-for-profit, 2-year",
    dat$sector_of_institution == 6 ~ "Private for-profit, 2-year",
    dat$sector_of_institution == 7 ~ "Public, less-than 2-year",
    dat$sector_of_institution == 8 ~ "Private not-for-profit, less-than 2-year",
    dat$sector_of_institution == 9 ~ "Private for-profit, less-than 2-year",
    dat$sector_of_institution == 99 ~ "Sector unknown (not active)"
  ))
  
  dat$level_of_institution <- as.factor(case_when(
    dat$level_of_institution == 1 ~ "Four or more years",
    dat$level_of_institution == 2 ~ "At least 2 but less than 4 years",
    dat$level_of_institution == 3 ~ "Less than 2 years (below associate)",
    dat$level_of_institution == -3 ~ "Not available"
  ))
  
  dat$control_of_institution <- as.factor(case_when(
    dat$control_of_institution == 1 ~ "Public",
    dat$control_of_institution == 2 ~ "Private not-for-profit",
    dat$control_of_institution == 3 ~ "Private for-proft",
    dat$control_of_institution == -3 ~ "Not available"
  ))
  
  dat$degree_of_urbanization_urban_centric_locale <- as.factor(case_when(
    dat$degree_of_urbanization_urban_centric_locale == 11 ~ "City: Large",
    dat$degree_of_urbanization_urban_centric_locale == 12 ~ "City: Midsize",
    dat$degree_of_urbanization_urban_centric_locale == 13 ~ "City: Small",
    dat$degree_of_urbanization_urban_centric_locale == 21 ~ "Suburb: Large",
    dat$degree_of_urbanization_urban_centric_locale == 22 ~ "Suburb: Midsize",
    dat$degree_of_urbanization_urban_centric_locale == 23 ~ "Suburb: Small",
    dat$degree_of_urbanization_urban_centric_locale == 31 ~ "Town: Fringe",
    dat$degree_of_urbanization_urban_centric_locale == 32 ~ "Town: Distant",
    dat$degree_of_urbanization_urban_centric_locale == 33 ~ "Town: Remote",
    dat$degree_of_urbanization_urban_centric_locale == 41 ~ "Rural: Fringe",
    dat$degree_of_urbanization_urban_centric_locale == 42 ~ "Rural: Distant",
    dat$degree_of_urbanization_urban_centric_locale == 43 ~ "Rural: Remote",
    dat$degree_of_urbanization_urban_centric_locale == -3 ~ "Not available"
  ))
  
  dat$institutional_category <- as.factor(case_when(
    dat$institutional_category == 1 ~ "Degree-granting, graduate with no undergraduate degrees",
    dat$institutional_category == 2 ~ "Degree-granting, primarily baccalaureate or above",
    dat$institutional_category == 3 ~ "Degree-granting, not primarily baccalaureate or above",
    dat$institutional_category == 4 ~ "Degree-granting, associate's and certificates",
    dat$institutional_category == 5 ~ "Nondegree-granting, above teh baccalaureate",
    dat$institutional_category == 6 ~ "Nondegree-granting, sub-baccalaureate",
    dat$institutional_category == -1 ~ "Not reported",
    dat$institutional_category == -2 ~ "Not applicable"
  ))
  
  dat$does_institution_have_a_tenure_system <- as.factor(case_when(
    dat$does_institution_have_a_tenure_system == 1 ~ "Has tenure system",
    dat$does_institution_have_a_tenure_system == 2 ~ "No tenure system",
    dat$does_institution_have_a_tenure_system == -1 ~ "Not reported",
    dat$does_institution_have_a_tenure_system == -2 ~ "Not applicable"
  ))
  
  dat$institution_size_category <- as.factor(case_when(
    dat$institution_size_category == 1 ~ "Under 1,000",
    dat$institution_size_category == 2 ~ "1,000 - 4,999",
    dat$institution_size_category == 3 ~ "5,000 - 9,999",
    dat$institution_size_category == 4 ~ "10,000 - 19,999",
    dat$institution_size_category == 5 ~ "20,000 and above",
    dat$institution_size_category == -1 ~ "Not reported",
    dat$institution_size_category == -2 ~ "Not applicable"
  ))
  
  dat$is_the_library_collection_entirely_electronic <- as.factor(case_when(
    dat$is_the_library_collection_entirely_electronic == 1 ~ "Yes",
    dat$is_the_library_collection_entirely_electronic == 2 ~ "No",
    dat$is_the_library_collection_entirely_electronic == -2 ~ "Not applicable",
    dat$is_the_library_collection_entirely_electronic == -1 ~ "Not reported"
  ))
  
  dat$are_staff_fringe_benefits_paid_out_of_the_library_budget <- as.factor(case_when(
    dat$are_staff_fringe_benefits_paid_out_of_the_library_budget == 1 ~ "Yes",
    dat$are_staff_fringe_benefits_paid_out_of_the_library_budget == 2 ~ "No",
    dat$are_staff_fringe_benefits_paid_out_of_the_library_budget == -2 ~ "Not applicable",
    dat$are_staff_fringe_benefits_paid_out_of_the_library_budget == -1 ~ "Not reported"
  ))
  
  dat$does_institution_have_interlibrary_loan_services <- as.factor(case_when(
    dat$does_institution_have_interlibrary_loan_services == 1 ~ "Yes",
    dat$does_institution_have_interlibrary_loan_services == 2 ~ "No",
    dat$does_institution_have_interlibrary_loan_services == -2 ~ "Not applicable",
    dat$does_institution_have_interlibrary_loan_services == -1 ~ "Not reported"
  ))
  
  dat$has_an_academic_library <- as.factor(case_when(
    dat$has_an_academic_library == 1 ~ "Yes, have access to library collections and have library expenses",
    dat$has_an_academic_library == 2 ~ "Yes, have access to library collections, but no library expenses",
    dat$has_an_academic_library == 3 ~ "No access to library collections, but have library expenses",
    dat$has_an_academic_library == 0 ~ "No access to library collections and no library expenses",
    dat$has_an_academic_library == -2 ~ "Not applicable",
    dat$has_an_academic_library == -1 ~ "Not reported"
  ))
  
  dat$highest_degree_offered <- as.factor(case_when(
    dat$highest_degree_offered == 11 ~ "Doctor's degree - research/scholarship and professional practice",
    dat$highest_degree_offered == 12 ~ "Doctor's degree - research/scholarship",
    dat$highest_degree_offered == 13 ~ "Doctor's degree - professional practice",
    dat$highest_degree_offered == 14 ~ "Doctor's degree - other",
    dat$highest_degree_offered == 20 ~ "Master's degree",
    dat$highest_degree_offered == 30 ~ "Bachelor's degree",
    dat$highest_degree_offered == 40 ~ "Associate's degree",
    dat$highest_degree_offered == 0 ~ "Non-degree granting",
    dat$highest_degree_offered == -3 ~ "Not available"
  ))
  
  dat$institution_has_hospital <- as.factor(case_when(
    dat$institution_has_hospital == 1 ~ "Yes",
    dat$institution_has_hospital == 2 ~ "No",
    dat$institution_has_hospital == -1 ~ "Not reported",
    dat$institution_has_hospital == -2 ~ "Not applicable"
  ))
  
  dat$institution_grants_a_medical_degree <- as.factor(case_when(
    dat$institution_grants_a_medical_degree == 1 ~ "Yes",
    dat$institution_grants_a_medical_degree == 2 ~ "No",
    dat$institution_grants_a_medical_degree == -1 ~ "Not reported",
    dat$institution_grants_a_medical_degree == -2 ~ "Not applicable"
  ))
  
  dat
}

dat <- get_data()
dat <- recode_variables(dat)

dat <- dat %>% 
  filter(has_an_academic_library == "Yes, have access to library collections and have library expenses" |
           has_an_academic_library == "Yes, have access to library collections, but no library expenses")

write_csv(dat, "data/processed/IPEDS.csv")