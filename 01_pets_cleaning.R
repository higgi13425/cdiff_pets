#note that data are downloaded from SurveyMonkey
#  Using Analyze Results, Exports, All individual responses
# XLS+, Original View
# then open Excel folder, Clostridium difficile Pets Survey.xlsx

#load libraries
library(rio)
library(janitor) #note using github version
library(dplyr)
library(visdat)
library(naniar)
library(plotly)
pets_raw <- import('cdiff_pets.xlsx')

#remove unneeded rows and columns
pets <- pets_raw %>%
  clean_names %>%
  remove_empty_rows() %>% 
  remove_empty_cols() %>% 
  select(-respondent_id, -collector_id, -end_date, -ip_address) %>% 
  filter(i_would_like_to_continue_and_complete_this_22_question_survey == "Yes")  %>% 
  filter(!is.na(study_id_number)) 
  
#rename ugly columns
pets <- pets %>%  
  rename(consent = i_would_like_to_continue_and_complete_this_22_question_survey) %>% 
  rename(study_id = study_id_number) %>% 
  rename(restaurant3 = do_you_eat_at_restaurants_more_than_3_times_per_week) %>% 
  rename(dessert3 = do_you_eat_dessert_more_than_3_times_per_week) %>% 
  rename(meat3 = do_you_eat_meat_more_than_3_times_per_week) %>% 
  rename(salad3 = do_you_eat_salads_more_than_3_times_per_week) %>% 
  rename(redwine3 = do_you_drink_red_wine_more_than_3_times_per_week) %>% 
  rename(dairy = how_many_servings_of_dairy_do_you_consume_each_week_1_serving_8_oz_milk_this_includes_cheeses_and_yogurt_but_not_butter) %>% 
  rename(vitamin = do_you_take_either_a_multivitamin_daily_or_vitamin_d_supplements_daily) %>% 
  rename(probiotic = do_you_take_probiotics_daily) %>% 
  rename(antibiotic_exp = did_you_use_any_antibiotics_in_the_three_months_before_or_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter_if_no_enter_no_if_yes_please_enter_yes_followed_by_the_name_of_the_antibiotic_i_e_yes_keflex_or_if_unknown_yes_unknown) %>% 
  rename(acid_blocker = did_you_use_any_of_the_following_acid_blocking_medications_in_the_following_4_weeks_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(cdi = have_you_ever_in_your_life_tested_positive_for_an_intestinal_infection_called_clostridium_difficile_c_diff_that_required_antibiotics) %>% 
  rename(health_care = does_anyone_who_lives_in_your_household_work_in_a_health_care_setting_where_there_are_patients_being_treated_for_illnesses_hospital_clinic_etc) %>% 
  rename(hospital = were_you_admitted_to_a_hospital_in_the_three_months_prior_to_at_time_of_c_diff_test_test_date_in_letter) %>% 
  rename(hc_facility = did_you_live_in_a_health_care_facility_nursing_home_rehabilitation_center_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(adl = prior_to_your_c_diff_test_on_supply_date_if_not_known_did_you_have_difficulty_or_require_assistance_in_daily_activity_such_as) %>% 
  rename(dog_allerg = are_you_allergic_to_dogs) %>% 
  rename(cat_allerg = are_you_allergic_to_cats) %>% 
  rename(dog = do_you_have_a_dog_that_sleeps_in_your_house_each_night) %>% 
  rename(dog_outside = does_your_dog_go_outside_each_day) %>% 
  rename(cat = do_you_have_a_cat_that_sleeps_in_your_house_each_night) %>% 
  rename(cat_outside = does_your_cat_go_outside_each_day)  
  
  
# fix acid_blocker variable
sum(is.na(pets$acid_blocker)) #59 missing
#fix up one with multiple acid blockers
pets$x_7[pets$study_id == "106"] <- NA #eliminates famotidine, keeps PPI

#now start filling in acid_blocker
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_1[is.na(pets$acid_blocker)] #now 57
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_2[is.na(pets$acid_blocker)] #now 56
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_3[is.na(pets$acid_blocker)]
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_4[is.na(pets$acid_blocker)] #now 55
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_5[is.na(pets$acid_blocker)] #now 54
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_6[is.na(pets$acid_blocker)] #now 49
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_7[is.na(pets$acid_blocker)] #now 46
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_8[is.na(pets$acid_blocker)]
pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_9[is.na(pets$acid_blocker)]
pets$acid_blocker[is.na(pets$acid_blocker)] <- "None"
#now none missing
tabyl(pets$acid_blocker)
#now drop x_1 to x_9
pets <- pets %>% select(-(x_1:x_10))

# fix antibiotic exposure
#ideally edit each entry to include a clear (and correct) Yes or No,
# edit out any inadvertent "no" or "not"
# note that some yes = entocort, zofran
pets$abx[grepl("yes", pets$antibiotic_exp, ignore.case = TRUE)] <- "Yes"
pets$abx[grepl("no", pets$antibiotic_exp, ignore.case = TRUE)] <- "No"
pets$abx[c(19,21,28, 33,36,47,52, 62, 63,65,66,72,73,76)] <- "Yes"
pets$abx[c(16,37, 46,74,75)] <- "No"


# fix dairy entries
pets$dairy[c(21)] <- "6.5"
pets$dairy[c(22)] <- "5.5"
pets$dairy[c(23)] <- "17.5"
pets$dairy[c(24)] <- "10.5"
pets$dairy[c(25)] <- "8.5"
pets$dairy[c(46)] <- "0"
pets$dairy[c(47)] <- "2.5"
pets$dairy[c(54)] <- "8"
pets$dairy[c(63)] <- "3"
pets$dairy[c(64)] <- "3"
pets$dairy[c(67)] <- "5" # for not sure
pets$dairy[c(71)] <- "25" # for >20
pets$dairy[c(72)] <- "8"
pets$dairy[c(73)] <- "9"
pets$dairy[c(75)] <- "2"
pets$dairy[c(76)] <- "24"
pets$dairy[c(77)] <- "11" #for 10+

pets$dairy <- as.integer(pets$dairy)

# check, fix cdi Yes/no









# fix adl variables - sum up
pets <- pets %>%  
  rename(adl_feed = adl) %>% 
  rename(adl_walk = x_11) %>% 
  rename(adl_transfer = x_12) %>% 
  rename(adl_dress = x_13) %>% 
  rename(adl_groom = x_14) %>% 
  rename(adl_bathroom = x_15)

#fix one case with missing values
pets$adl_feed[pets$study_id == 260] <- "Some Assistance"
pets$adl_walk[pets$study_id == 260] <- "Some Assistance"
pets$adl_transfer[pets$study_id == 260] <- "Some Assistance"
pets$adl_groom[pets$study_id == 260] <- "Some Assistance"
pets$adl_bathroom[pets$study_id == 260] <- "Some Assistance"


#create adl summary score
pets$adl_score <- 0
pets$adl_score[pets$adl_feed == "Independent"] <- pets$adl_score[pets$adl_feed == "Independent"]+2
pets$adl_score[pets$adl_feed == "Some Assistance"] <- pets$adl_score[pets$adl_feed == "Some Assistance"]+1

pets$adl_score[pets$adl_walk == "Independent"] <- pets$adl_score[pets$adl_walk == "Independent"]+2
pets$adl_score[pets$adl_walk == "Some Assistance"] <- pets$adl_score[pets$adl_walk == "Some Assistance"]+1

pets$adl_score[pets$adl_transfer == "Independent"] <- pets$adl_score[pets$adl_transfer == "Independent"]+2
pets$adl_score[pets$adl_transfer == "Some Assistance"] <- pets$adl_score[pets$adl_transfer == "Some Assistance"]+1

pets$adl_score[pets$adl_dress == "Independent"] <- pets$adl_score[pets$adl_dress == "Independent"]+2
pets$adl_score[pets$adl_dress == "Some Assistance"] <- pets$adl_score[pets$adl_dress == "Some Assistance"]+1

pets$adl_score[pets$adl_groom == "Independent"] <- pets$adl_score[pets$adl_groom == "Independent"]+2
pets$adl_score[pets$adl_groom == "Some Assistance"] <- pets$adl_score[pets$adl_groom == "Some Assistance"]+1

pets$adl_score[pets$adl_bathroom == "Independent"] <- pets$adl_score[pets$adl_bathroom == "Independent"]+2
pets$adl_score[pets$adl_bathroom == "Some Assistance"] <- pets$adl_score[pets$adl_bathroom == "Some Assistance"]+1


#fix some missing animal data
#fix dog_outside not applicable in 20, 36, 67, 77
pets$dog_outside[c(20,36,67,77)] <- "Not Applicable"
#fix cat_outside not applicable in 20
pets$cat_outside[20] <- "Not Applicable"

# identify, fix missing data esp at end of survey
vis_dat(pets)
vis_miss(pets)
pets %>% select(starts_with("x")) %>% vis_miss() %>% ggplotly()
vis_miss(pets) %>% ggplotly() #interactive - a bit slow - to be replaced by vis_miss_ly
gg_miss_var(pets)
miss_case_table(pets)
miss_case_summary(pets)
miss_var_summary(pets)

# still missing- all pet Q for obs 30, study_id 169
# still missing cat Q for obs 77, study_id 108


#convert a lot of character variables to factors
# convert some to integers study_id

