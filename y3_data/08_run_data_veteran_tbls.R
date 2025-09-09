################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 08_run_data_veteran_tbls.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 1/21/25 >
##
################################################################################

#Goal: Runs Analyses and Stores them in Pivot Tables by veteran status and 
# demographics.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)
library(purrr)
library(future)
library(furrr)
library(parallel)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

## ---------------------------
## file directories
## ---------------------------

code_file_dir<-file.path("C:/Users/yjeff/Desktop/Community Schools/Teacher Retention Data",
                         "LA","Analyses", "cs_teach_rent2", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")

data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

#error function
safe_function_df<-function(expr, version){
  result<- tryCatch(
    {
      expr
      },
    error = function(e){
      
      return(data.frame(percent = NA))
      
    }
    
  )
  return(result)
}

safe_function<-function(expr, version){
  result<- tryCatch(
    {
      expr
    },
    error = function(e){
      
      return(NA)
      
    }
    
  )
  return(result)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "hr_color_by_sch.RData"))
source(file.path(code_file_dir, "00_school_lists.R"))

## -----------------------------------------------------------------------------
## Part 0 - Preparation
## -----------------------------------------------------------------------------

#determine all the race/ethncity categories

all_data<-hr_by_sch %>% bind_rows(.id = "school")

ethnicity_string<-all_data$hr_ethnicity %>% unique()

#create ethnicity binary variable function
add_toc_var<-function(df){
  
  df_update<-df %>% mutate(
    toc = case_when(
      hr_ethnicity %in% c("Hispanic/Latino", "Black or African American",
                          "Two or More", "Pacific Islander", "Asian",
                          "Filipino", "American Indian or Alaskan Native") ~ 1,
      TRUE ~ 0
    )
  )
  return(df_update)
}

#test<-add_toc_var(hr_by_sch[["107th Street Elementary"]])

#detect cores
num_cores<-detectCores()

plan(multisession, workers = num_cores-2)

## -----------------------------------------------------------------------------
## Part 1 - Add Functions
## -----------------------------------------------------------------------------

#these veteran tables show:
#among all/BIPOC teachers, what percent are veteran teachers 

create_veteran_tbl<-function(df, sch_name, outcome, type = "all"){
  
  test<-df
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-5 years","6-10 years",
                                         "11-15 years","15+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-5 years","6-10 years",
                                         "11-15 years","15+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-5 years","6-10 years",
                                         "11-15 years","15+ years", NA))
    }
    }
  
  vet_tbl<-data.frame(vet_status = c("0-3 years","4-5 years","6-10 years",
                                     "11-15 years","15+ years", NA))
  
  vet_data<-left_join(vet_tbl,data19, by = c("vet_status" = "teacher_years_18_19_cv2"))
  vet_data<-left_join(vet_data,data20, by = c("vet_status" = "teacher_years_19_20_cv2"))
  vet_data<-left_join(vet_data,data21, by = c("vet_status" = "teacher_years_20_21_cv2"))
  vet_data<-left_join(vet_data,data22, by = c("vet_status" = "teacher_years_21_22_cv2"))
  vet_data<-left_join(vet_data,data23, by = c("vet_status" = "teacher_years_22_23_cv2"))
  vet_data<-left_join(vet_data,data24, by = c("vet_status" = "teacher_years_23_24_cv2"))


  colnames(vet_data)<-c("vet_status", "yr_18_19", "yr_19_20",
                         "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  
  #filter out rows
  vet_data<-vet_data %>% filter(!is.na(vet_status))
  
  return(vet_data)
}

create_veteran_tbl2<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_18_19_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_19_20_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_20_21_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_21_22_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_22_23_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years","4-5 years")~"0-5 years",
      teacher_years_23_24_cv2 %in% c("6-10 years","11-15 years","15+ years")~"5+ years",
      TRUE ~ NA)
  )
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-5 years","5+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-5 years","5+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-5 years","5+ years", NA))
    }
  }
  
  vet_tbl<-data.frame(vet_status = c("0-5 years","5+ years", NA))
  
  vet_data<-left_join(vet_tbl,data19, by = c("vet_status" = "teacher_years_18_19_cv2"))
  vet_data<-left_join(vet_data,data20, by = c("vet_status" = "teacher_years_19_20_cv2"))
  vet_data<-left_join(vet_data,data21, by = c("vet_status" = "teacher_years_20_21_cv2"))
  vet_data<-left_join(vet_data,data22, by = c("vet_status" = "teacher_years_21_22_cv2"))
  vet_data<-left_join(vet_data,data23, by = c("vet_status" = "teacher_years_22_23_cv2"))
  vet_data<-left_join(vet_data,data24, by = c("vet_status" = "teacher_years_23_24_cv2"))
  
  
  colnames(vet_data)<-c("vet_status", "yr_18_19", "yr_19_20",
                        "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  
  #filter out rows
  vet_data<-vet_data %>% filter(!is.na(vet_status))
  
  return(vet_data)
}

create_veteran_tbl3<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_18_19_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_18_19_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_19_20_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_19_20_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_20_21_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_20_21_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_21_22_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_21_22_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_22_23_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_22_23_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_23_24_cv2 %in% c("4-5 years","6-10 years")~"4-10 years",
      teacher_years_23_24_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA)
  )
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years",
                                         "10+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)", "teacher (out)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
  }
  
  vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
  
  vet_data<-left_join(vet_tbl,data19, by = c("vet_status" = "teacher_years_18_19_cv2"))
  vet_data<-left_join(vet_data,data20, by = c("vet_status" = "teacher_years_19_20_cv2"))
  vet_data<-left_join(vet_data,data21, by = c("vet_status" = "teacher_years_20_21_cv2"))
  vet_data<-left_join(vet_data,data22, by = c("vet_status" = "teacher_years_21_22_cv2"))
  vet_data<-left_join(vet_data,data23, by = c("vet_status" = "teacher_years_22_23_cv2"))
  vet_data<-left_join(vet_data,data24, by = c("vet_status" = "teacher_years_23_24_cv2"))
  
  
  colnames(vet_data)<-c("vet_status", "yr_18_19", "yr_19_20",
                        "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  
  #filter out rows
  vet_data<-vet_data %>% filter(!is.na(vet_status))
  
  return(vet_data)
}

#only in-classroom teachers
create_veteran_tbl4<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_18_19_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_18_19_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_19_20_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_19_20_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_20_21_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_20_21_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_21_22_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_21_22_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_22_23_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_22_23_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_23_24_cv2 %in% c("4-5 years","6-10 years")~"4-10 years",
      teacher_years_23_24_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA)
  )
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years",
                                         "10+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        group_by(teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        group_by(teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        group_by(teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        group_by(teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        group_by(teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        group_by(teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test24) * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
  }
  
  vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
  
  vet_data<-left_join(vet_tbl,data19, by = c("vet_status" = "teacher_years_18_19_cv2"))
  vet_data<-left_join(vet_data,data20, by = c("vet_status" = "teacher_years_19_20_cv2"))
  vet_data<-left_join(vet_data,data21, by = c("vet_status" = "teacher_years_20_21_cv2"))
  vet_data<-left_join(vet_data,data22, by = c("vet_status" = "teacher_years_21_22_cv2"))
  vet_data<-left_join(vet_data,data23, by = c("vet_status" = "teacher_years_22_23_cv2"))
  vet_data<-left_join(vet_data,data24, by = c("vet_status" = "teacher_years_23_24_cv2"))
  
  
  colnames(vet_data)<-c("vet_status", "yr_18_19", "yr_19_20",
                        "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  
  #filter out rows
  vet_data<-vet_data %>% filter(!is.na(vet_status))
  
  return(vet_data)
}

#CREATE DEMOGRAPHIC FUNCTION
create_demo_veteran_tbl<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_18_19_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_18_19_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_19_20_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_19_20_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_20_21_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_20_21_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_21_22_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_21_22_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_22_23_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_22_23_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_23_24_cv2 %in% c("4-5 years","6-10 years")~"4-10 years",
      teacher_years_23_24_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA)
  )
  
  #add demographic variables
  test<-add_toc_var(test) 
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(toc, teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(toc, teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(toc, teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(toc, teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(toc, teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(toc, teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years",
                                         "10+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables (Percent of TOC in the school)
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((sum(toc)/n() * 100),2))
      
      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((sum(toc)/n() * 100),2))
      
      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((sum(toc)/n() * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((sum(toc)/n() * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((sum(toc)/n() * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent24 = round((sum(toc)/n() * 100),2))      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((sum(toc)/n() * 100),2))

      data20<-test20 %>% 
        group_by(teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((sum(toc)/n() * 100),2))

      data21<-test21 %>% 
        group_by(teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((sum(toc)/n() * 100),2))
      
      data22<-test22 %>% 
        group_by(teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((sum(toc)/n() * 100),2))
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((sum(toc)/n() * 100),2))
      
      data24<-test24 %>% 
        group_by(teacher_years_23_24_cv2) %>% 
        summarize(percent24 = round((sum(toc)/n() * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
  }
  
  
  if (outcome == "percent"){
    
    vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    
    vet_data<-left_join(vet_tbl,data19, by = c("vet_status" = "teacher_years_18_19_cv2"))
    vet_data<-left_join(vet_data,data20, by = c("vet_status" = "teacher_years_19_20_cv2"))
    vet_data<-left_join(vet_data,data21, by = c("vet_status" = "teacher_years_20_21_cv2"))
    vet_data<-left_join(vet_data,data22, by = c("vet_status" = "teacher_years_21_22_cv2"))
    vet_data<-left_join(vet_data,data23, by = c("vet_status" = "teacher_years_22_23_cv2"))
    vet_data<-left_join(vet_data,data24, by = c("vet_status" = "teacher_years_23_24_cv2"))
    
    
    colnames(vet_data)<-c("vet_status", "yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
    
    
  }

  if (outcome == "count"){
    
    vet_tbl<-data.frame(vet_status = c("0-3 years","0-3 years",
                                       "4-10 years","4-10 years",
                                       "10+ years","10+ years",
                                       NA, NA),
                        toc = rep(c(0,1),4))
    
    vet_data<-left_join(vet_tbl,data19, by = c("toc", "vet_status" = "teacher_years_18_19_cv2"))
    vet_data<-left_join(vet_data,data20, by = c("toc","vet_status" = "teacher_years_19_20_cv2"))
    vet_data<-left_join(vet_data,data21, by = c("toc","vet_status" = "teacher_years_20_21_cv2"))
    vet_data<-left_join(vet_data,data22, by = c("toc","vet_status" = "teacher_years_21_22_cv2"))
    vet_data<-left_join(vet_data,data23, by = c("toc","vet_status" = "teacher_years_22_23_cv2"))
    vet_data<-left_join(vet_data,data24, by = c("toc","vet_status" = "teacher_years_23_24_cv2"))
    
    
    colnames(vet_data)<-c("vet_status","toc",  "yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
    
    
  }
  
  #filter out NA veteran status  and keep only TOC
  vet_data<-vet_data %>% filter(!is.na(vet_status))
 # vet_data<-vet_data %>% filter(toc ==1)
  
  return(vet_data)
}

#updates the percentages to reflect whole school
create_demo_veteran_tbl2<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_18_19_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_18_19_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_19_20_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_19_20_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_20_21_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_20_21_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_21_22_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_21_22_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_22_23_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_22_23_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_23_24_cv2 %in% c("4-5 years","6-10 years")~"4-10 years",
      teacher_years_23_24_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA)
  )
  
  #add demographic variables
  test<-add_toc_var(test) 
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(toc, teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(toc, teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(toc, teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(toc, teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(toc, teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(toc, teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years",
                                         "10+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #create total count for the school overall by counting row numbers
      
      #make pivot tables (Percent of TOC in the school)
      data19<-test19 %>% 
        group_by(toc,teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(toc, teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(toc, teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(toc, teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(toc, teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(toc, teacher_years_23_24_cv2) %>% 
        summarize(percent24 = round((n()/nrow(test24) * 100),2))      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_18_19_cv2) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_19_20_cv2) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_20_21_cv2) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_21_22_cv2) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_22_23_cv2) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        group_by(toc, teacher_years_23_24_cv2) %>% summarize(count24 = n())
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets

      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      

      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        group_by(toc, teacher_years_18_19_cv2) %>%
        summarize(percent19 = round((n()/nrow(test19) * 100),2))
      
      data20<-test20 %>% 
        group_by(toc, teacher_years_19_20_cv2) %>% 
        summarize(percent20 = round((n()/nrow(test20) * 100),2))
      
      data21<-test21 %>% 
        group_by(toc, teacher_years_20_21_cv2) %>% 
        summarize(percent21 = round((n()/nrow(test21) * 100),2))
      
      data22<-test22 %>% 
        group_by(toc, teacher_years_21_22_cv2) %>% 
        summarize(percent22 = round((n()/nrow(test22) * 100),2))
      
      data23<-test23 %>% 
        group_by(toc, teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((n()/nrow(test23) * 100),2))
      
      data24<-test24 %>% 
        group_by(toc, teacher_years_23_24_cv2) %>% 
        summarize(percent24 = round((n()/nrow(test24) * 100),2))
      
      vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
  }
  
  
  if (outcome == "percent"){
    
    vet_tbl<-data.frame(vet_status = c("0-3 years","0-3 years",
                                       "4-10 years","4-10 years",
                                       "10+ years","10+ years",
                                       NA, NA),
                        toc = rep(c(0,1),4))
    
    vet_data<-left_join(vet_tbl,data19, by = c("toc", "vet_status" = "teacher_years_18_19_cv2"))
    vet_data<-left_join(vet_data,data20, by = c("toc","vet_status" = "teacher_years_19_20_cv2"))
    vet_data<-left_join(vet_data,data21, by = c("toc", "vet_status" = "teacher_years_20_21_cv2"))
    vet_data<-left_join(vet_data,data22, by = c("toc", "vet_status" = "teacher_years_21_22_cv2"))
    vet_data<-left_join(vet_data,data23, by = c("toc", "vet_status" = "teacher_years_22_23_cv2"))
    vet_data<-left_join(vet_data,data24, by = c("toc", "vet_status" = "teacher_years_23_24_cv2"))
     
    colnames(vet_data)<-c("vet_status","toc",  "yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  }
  
  if (outcome == "count"){
    
    vet_tbl<-data.frame(vet_status = c("0-3 years","0-3 years",
                                       "4-10 years","4-10 years",
                                       "10+ years","10+ years",
                                       NA, NA),
                        toc = rep(c(0,1),4))
    
    vet_data<-left_join(vet_tbl,data19, by = c("toc", "vet_status" = "teacher_years_18_19_cv2"))
    vet_data<-left_join(vet_data,data20, by = c("toc","vet_status" = "teacher_years_19_20_cv2"))
    vet_data<-left_join(vet_data,data21, by = c("toc","vet_status" = "teacher_years_20_21_cv2"))
    vet_data<-left_join(vet_data,data22, by = c("toc","vet_status" = "teacher_years_21_22_cv2"))
    vet_data<-left_join(vet_data,data23, by = c("toc","vet_status" = "teacher_years_22_23_cv2"))
    vet_data<-left_join(vet_data,data24, by = c("toc","vet_status" = "teacher_years_23_24_cv2"))
    
    
    colnames(vet_data)<-c("vet_status","toc",  "yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
    
    
  }
  
  #filter out NA veteran status  and keep only TOC
  vet_data<-vet_data %>% filter(!is.na(vet_status))
  # vet_data<-vet_data %>% filter(toc ==1)
  
  return(vet_data)
  
}

#PLAN:
#With Existing Demo Dataset, use the Count table to summ up the complete 
#number of teachers, then divide that by the number of teachers in each category
#to get the right number -DONE

create_demo_veteran_tbl_overall<-function(df, sch_name, outcome, type = "all"){
  
  test<-df %>% mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_18_19_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_18_19_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_19_20_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_19_20_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_20_21_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_20_21_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_21_22_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_21_22_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_22_23_cv2 %in% c("4-5 years", "6-10 years")~"4-10 years",
      teacher_years_22_23_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24_cv2 %in% c("0-3 years")~"0-3 years",
      teacher_years_23_24_cv2 %in% c("4-5 years","6-10 years")~"4-10 years",
      teacher_years_23_24_cv2 %in% c("11-15 years","15+ years")~"10+ years",
      TRUE ~ NA)
  )
  
  #add demographic variables
  test<-add_toc_var(test) 
  
  if (type == "all"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        group_by(toc) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        group_by(toc) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        group_by(toc) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        group_by(toc) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        group_by(toc) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        group_by(toc) %>% summarize(count24 = n())
      
      # vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years",
      #                                    "10+ years", NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>% 
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>% 
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        summarize(percent19 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data20<-test20 %>% 
        summarize(percent20 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data21<-test21 %>% 
        summarize(percent21 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data22<-test22 %>% 
        summarize(percent22 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data23<-test23 %>% 
        summarize(percent23 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data24<-test24 %>% 
        summarize(percent24 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()      
    }
    
  }
  
  if (type == "teachers"){
    
    if (outcome == "count"){
      
      data19<-test %>% 
        filter(`school_name_clean_year_2018-19` == sch_name) %>% 
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count19 = n())
      
      data20<-test %>% 
        filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count20 = n())
      
      data21<-test %>% 
        filter(`school_name_clean_year_2020-21` == sch_name) %>% 
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count21 = n())
      
      data22<-test %>% 
        filter(`school_name_clean_year_2021-22` == sch_name) %>%
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count22 = n())
      
      data23<-test %>% 
        filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count23 = n())
      
      data24<-test %>% 
        filter(`school_name_clean_year_2023-24` == sch_name) %>%
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        group_by(toc) %>% summarize(count24 = n())
      
      # vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
    
    if (outcome == "percent"){
      
      #make filtered datasets
      test19<-test %>% filter(`school_name_clean_year_2018-19` == sch_name) %>%
        filter(`job_type_year_2018-19` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_18_19_cv2))
      
      test20<-test %>% filter(`school_name_clean_year_2019-20` == sch_name) %>%
        filter(`job_type_year_2019-20` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_19_20_cv2))
      
      test21<-test %>% filter(`school_name_clean_year_2020-21` == sch_name) %>%
        filter(`job_type_year_2020-21` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_20_21_cv2))
      
      test22<-test %>% filter(`school_name_clean_year_2021-22` == sch_name) %>% 
        filter(`job_type_year_2021-22` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_21_22_cv2))
      
      test23<-test %>% filter(`school_name_clean_year_2022-23` == sch_name) %>%
        filter(`job_type_year_2022-23` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_22_23_cv2))
      
      test24<-test %>% filter(`school_name_clean_year_2023-24` == sch_name) %>% 
        filter(`job_type_year_2023-24` %in% c("teacher (in)")) %>%
        filter(!is.na(teacher_years_23_24_cv2))
      
      #make pivot tables
      data19<-test19 %>% 
        summarize(percent19 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data20<-test20 %>% 
        summarize(percent20 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data21<-test21 %>% 
        summarize(percent21 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data22<-test22 %>% 
        summarize(percent22 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data23<-test23 %>% 
        group_by(teacher_years_22_23_cv2) %>% 
        summarize(percent23 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      data24<-test24 %>% 
        summarize(percent24 = round((sum(toc)/n() * 100),2)) %>% safe_function_df()
      
      # vet_tbl<-data.frame(vet_status = c("0-3 years","4-10 years","10+ years",NA))
    }
  }
  
  
  if (outcome == "percent"){
    
    vet_data<-cbind(data19,data20,data21,data22,data23,data24)
    
    colnames(vet_data)<-c("yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
  }
  
  if (outcome == "count"){
    
    vet_tbl<-data.frame(toc = c(0,1))
    
    vet_data<-left_join(vet_tbl,data19, by = c("toc"))
    vet_data<-left_join(vet_data,data20, by = c("toc"))
    vet_data<-left_join(vet_data,data21, by = c("toc"))
    vet_data<-left_join(vet_data,data22, by = c("toc"))
    vet_data<-left_join(vet_data,data23, by = c("toc"))
    vet_data<-left_join(vet_data,data24, by = c("toc"))
    
    colnames(vet_data)<-c("toc",  "yr_18_19", "yr_19_20",
                          "yr_20_21", "yr_21_22", "yr_22_23", "yr_23_24")
    
    
  }

  return(vet_data)
}

#check
test_c2<-create_demo_veteran_tbl2(hr_by_sch[["107th Street Elementary"]],
                                  "107th Street Elementary",
                                  "count", type = "teachers")
test_p2<-create_demo_veteran_tbl2(hr_by_sch[["107th Street Elementary"]],
                                  "107th Street Elementary",
                                  "percent", type = "teachers")

#combined table
create_combined_tbl<-function(df_cs, df_ts, df_combined){
  
  #created combined overall table
  combined<-df[['overall']] %>% filter(rent == "Overall Retention Rate") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate" ~ "Combined Overall Retention Rate")) 
  
  cs<-df[['cs']] %>% filter(rent == "Overall Retention Rate") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate" ~ "CS Overall Retention Rate")) 
  
  ts<-df[['ts']] %>% filter(rent == "Overall Retention Rate") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate" ~ "TS Overall Retention Rate")) 
  
  df[['combined_overall']]<-rbind(cs, ts, combined)
  
  
}

## -----------------------------------------------------------------------------
## Part 2.1 - Create Retention Tables
## -----------------------------------------------------------------------------

update_vet_tbls<-function(all_list, toc_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 2)
    names(x)<-c("all", "color")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_veteran_tbl(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  vet_tbls[["staff"]][["color"]]<-future_map(table_type_string, 
                                      function(tbl_type){
                                        map2(toc_list,names(toc_list),
                                             function(x,y){create_veteran_tbl(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["color"]])<- table_type_string
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_veteran_tbl(x,y, tbl_type,
                                                                            type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  
  
  vet_tbls[["teachers"]][["color"]]<-future_map(table_type_string, 
                                      function(tbl_type){
                                        map2(toc_list,names(toc_list),
                                             function(x,y){create_veteran_tbl(x,y, tbl_type,
                                                                              type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["color"]])<- table_type_string
  
  return(vet_tbls)
}

update_vet_tbls2<-function(all_list, toc_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 2)
    names(x)<-c("all", "color")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_veteran_tbl2(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  vet_tbls[["staff"]][["color"]]<-future_map(table_type_string, 
                                      function(tbl_type){
                                        map2(toc_list,names(toc_list),
                                             function(x,y){create_veteran_tbl2(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["color"]])<- table_type_string
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                       function(tbl_type){
                                         map2(all_list,names(all_list),
                                              function(x,y){create_veteran_tbl2(x,y, tbl_type,
                                                                               type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  
  
  vet_tbls[["teachers"]][["color"]]<-future_map(table_type_string, 
                                         function(tbl_type){
                                           map2(toc_list,names(toc_list),
                                                function(x,y){create_veteran_tbl2(x,y, tbl_type,
                                                                                 type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["color"]])<- table_type_string
  
  return(vet_tbls)
}

update_vet_tbls3<-function(all_list, toc_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 2)
    names(x)<-c("all", "color")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_veteran_tbl3(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  vet_tbls[["staff"]][["color"]]<-future_map(table_type_string, 
                                      function(tbl_type){
                                        map2(toc_list,names(toc_list),
                                             function(x,y){create_veteran_tbl3(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["color"]])<- table_type_string
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                       function(tbl_type){
                                         map2(all_list,names(all_list),
                                              function(x,y){create_veteran_tbl3(x,y, tbl_type,
                                                                                type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  
  
  vet_tbls[["teachers"]][["color"]]<-future_map(table_type_string, 
                                         function(tbl_type){
                                           map2(toc_list,names(toc_list),
                                                function(x,y){create_veteran_tbl3(x,y, tbl_type,
                                                                                  type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["color"]])<- table_type_string
  
  return(vet_tbls)
}

update_vet_tbls4<-function(all_list, toc_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 2)
    names(x)<-c("all", "color")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_veteran_tbl4(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  vet_tbls[["staff"]][["color"]]<-future_map(table_type_string, 
                                      function(tbl_type){
                                        map2(toc_list,names(toc_list),
                                             function(x,y){create_veteran_tbl4(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["color"]])<- table_type_string
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                       function(tbl_type){
                                         map2(all_list,names(all_list),
                                              function(x,y){create_veteran_tbl4(x,y, tbl_type,
                                                                                type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  
  
  vet_tbls[["teachers"]][["color"]]<-future_map(table_type_string, 
                                         function(tbl_type){
                                           map2(toc_list,names(toc_list),
                                                function(x,y){create_veteran_tbl4(x,y, tbl_type,
                                                                                  type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["color"]])<- table_type_string
  
  return(vet_tbls)
}

vet_tbls<-vector("list",4)
names(vet_tbls)<-c(str_c("version", 1:4))
  
vet_tbls[["version1"]]<-update_vet_tbls(hr_by_sch, hr_by_sch_color)
vet_tbls[["version2"]]<-update_vet_tbls2(hr_by_sch, hr_by_sch_color)
vet_tbls[["version3"]]<-update_vet_tbls3(hr_by_sch, hr_by_sch_color)
vet_tbls[["version4"]]<-update_vet_tbls4(hr_by_sch, hr_by_sch_color)

#demographics
update_demo_tbls<-function(all_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 1)
    names(x)<-c("all")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_demo_veteran_tbl(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                       function(tbl_type){
                                         map2(all_list,names(all_list),
                                              function(x,y){create_demo_veteran_tbl(x,y, tbl_type,
                                                                                type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  return(vet_tbls)
}

#function that creates demo tables where percentages are calculated by the 
#number of staff/teachers for the whole school
update_demo_tbls2<-function(all_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 1)
    names(x)<-c("all")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                           function(tbl_type){
                                             map2(all_list,names(all_list),
                                                  function(x,y){create_demo_veteran_tbl2(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                              function(tbl_type){
                                                map2(all_list,names(all_list),
                                                     function(x,y){create_demo_veteran_tbl2(x,y, tbl_type,
                                                                                           type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  return(vet_tbls)
}

update_demo_tbls_overall<-function(all_list){
  
  vet_tbls<-vector("list", 2)
  names(vet_tbls)<-c("staff", "teachers")
  
  vet_tbls<-map(vet_tbls, function(x){
    x<-vector("list", 1)
    names(x)<-c("all")
    return(x)
  })
  
  
  table_type_string<-c("count", "percent")
  
  #all staff
  vet_tbls[["staff"]][["all"]]<-future_map(table_type_string, 
                                    function(tbl_type){
                                      map2(all_list,names(all_list),
                                           function(x,y){create_demo_veteran_tbl_overall(x,y, tbl_type)})})
  
  names(vet_tbls[["staff"]][["all"]])<- table_type_string
  
  
  #all teachers
  vet_tbls[["teachers"]][["all"]]<-future_map(table_type_string, 
                                       function(tbl_type){
                                         map2(all_list,names(all_list),
                                              function(x,y){create_demo_veteran_tbl_overall(x,y, tbl_type,
                                                                                    type = "teachers")})})
  
  names(vet_tbls[["teachers"]][["all"]])<- table_type_string
  
  return(vet_tbls)
}

## -----------------------------------------------------------------------------
## Part 2.2 - Add School Strings
## -----------------------------------------------------------------------------

all_sch_string<-names(vet_tbls[["version1"]][["teachers"]][["color"]][["percent"]])
ts_sch_string<-all_sch_string[!all_sch_string %in% cs_string]

#create elementary and middle/high school comparison school lists
ts_rent_elem<-ts_sch_string[grepl("Elementary", ts_sch_string)]
  
ts_elem_string<-c(ts_rent_elem,
                  "Carmen Lomas Garza Primary Center",
                  "Caroldale Learning Community",
                  "Rosa Parks Learning Center",
                  "Young Empowered Scholars Academy")

ts_ms_hs_string<-c("Abraham Lincoln Senior High",
                   "Arleta Senior High",
                   "Cal Burke High School",
                   "George Washington Carver Middle School",
                   "Glenn Hammond Curtiss Middle School",
                   "Harold McAlister High School CYESIS",
                   "Los Angeles Academy Middle School",
                   "Mark Twain Middle School",
                   "Theodore Roosevelt Senior High",
                   "Miguel Contreras Learning Complex - Business and Tourism",
                   "Miguel Contreras Learning Complex - School of Social Justice",
                   "Orchard Academies 2B",
                   "Orchard Academies 2C")

ts_span_string<-c("Elizabeth Learning Center",
                  "Judith F Baca Arts Academy")

#create school list string vector
school_string_list<-vector("list", 2)
names(school_string_list)<-c("cs", "ts")

#CS
school_string_list[["cs"]]<-list(cs_string, cs_elem, cs_mid_hi)
names(school_string_list[["cs"]])<-c("all", "elem", "mid_hi")

#TS
school_string_list[["ts"]]<-list(ts_sch_string, ts_elem_string, ts_ms_hs_string,
                                 ts_span_string)
names(school_string_list[["ts"]])<-c("all", "elem", "mid_hi", "span")

## -----------------------------------------------------------------------------
## Part 2.2 - Create Veteran Percentage Tables - Overall
## -----------------------------------------------------------------------------

add_names_to_tables<-function(df, sch_name){
  
  df_update<-df %>% mutate(school = sch_name)
  df_update<-df_update %>% select(school, everything())
  
  return(df_update)
}

create_overall_vet_tbl<-function(df, school_string,status_vet){
  
  df_update<-df %>% keep(names(df) %in% school_string)
  
  #Create Overall Retention Rate
  
  rent_overall<-bind_rows(df_update, .id = 'school') %>%
    filter(vet_status == status_vet)
  
  rent_overall<-rent_overall %>%
    mutate(
      yr_18_19 = as.numeric(yr_18_19),
      yr_19_20 = as.numeric(yr_19_20),
      yr_20_21 = as.numeric(yr_20_21),
      yr_21_22 = as.numeric(yr_21_22),
      yr_22_23 = as.numeric(yr_22_23),
      yr_23_24 = as.numeric(yr_23_24),
    )

  pivot_rent<-rent_overall %>%
    summarize(
      yr_18_19 = sum(yr_18_19, na.rm = T)/n() %>% round(),
      yr_19_20 = sum(yr_19_20, na.rm = T)/n() %>% round(),
      yr_20_21 = sum(yr_20_21, na.rm = T)/n() %>% round(),
      yr_21_22 = sum(yr_21_22, na.rm = T)/n() %>% round(),
      yr_22_23 = sum(yr_22_23, na.rm = T)/n() %>% round(),
      yr_23_24 = sum(yr_23_24, na.rm = T)/n() %>% round(),

    )

  pivot_rent$school<- "Percent of Teachers"

  pivot_rent$vet_status<- "Percent of Teachers"

  pivot_rent<-pivot_rent %>% select(school, vet_status, everything())

  #if (tbl_type == "Retention Rate %"){
    rent_overall<-rbind(rent_overall, pivot_rent)

    #round rental numbers to whole numbers

    rent_overall<-rent_overall %>%
      mutate(yr_18_19 = round(yr_18_19),
             yr_19_20 = round(yr_19_20),
             yr_20_21 = round(yr_20_21),
             yr_21_22 = round(yr_21_22),
             yr_22_23 = round(yr_22_23),
             yr_23_24 = round(yr_23_24)
             )

  
  return(rent_overall)
}

vet_status_string<-c("0-3 years","4-5 years","6-10 years",
                     "11-15 years","15+ years")

#overall rent tables
create_vet_tbls_overall_list<-function(df_tbls, schools, status_string){
  
  df_list<-map(df_tbls, function(teach_staff){
    
    map(teach_staff,
        function(teach_type){
          map(teach_type,
              function(count_percent_type){
                df_update<-map(status_string, function(status){
                  create_overall_vet_tbl(count_percent_type,
                                         schools,status)
                })
                names(df_update)<-status_string
                return(df_update)
              })  
        }) 
  })
  
  return(df_list)
  
}

#create veteran combined tables function
create_vet_combined_tbls_list<-function(vet_list, status_string){
  
  vet_tbls_overall_list<-map(school_string_list,
                             function(school_list){
                               map(school_list,
                                   function(string_sch){
                                     create_vet_tbls_overall_list(vet_list,
                                                                  string_sch,
                                                                  status_string) %>% 
                                       safe_function()
                                   })})
  
  #by neighborhood
  vet_tbls_neighborhood<-vector("list", 3)
  names(vet_tbls_neighborhood)<-c("overall", "cs", "ts")
  
  vet_tbls_neighborhood[["overall"]]<- map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch,status_string)%>% safe_function()})
  
  vet_tbls_neighborhood[["cs"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch[string_sch %in% cs_string],
                                 status_string) %>% safe_function()}) 
  
  vet_tbls_neighborhood[["ts"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch[string_sch %in%
                                                       ts_sch_string],
                                 status_string) %>% safe_function()}) 
  
  #By Cohort
  vet_tbls_cohort<-vector("list", 3)
  names(vet_tbls_cohort)<-c(str_c("cohort", c(1:3)))
  
  vet_tbls_cohort[["cohort1"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort1_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort2"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort2_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort3"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort2_3_string,
                                                             status_string) %>% safe_function()
  
  #combine all the lists together into one
  
  vet_combined_tbls<-vector("list", 3)
  names(vet_combined_tbls)<-c("overall", "neighborhood", "cohort")
  
  vet_combined_tbls[["overall"]]<-vet_tbls_overall_list
  vet_combined_tbls[["neighborhood"]]<-vet_tbls_neighborhood
  vet_combined_tbls[["cohort"]]<-vet_tbls_cohort
  
  return(vet_combined_tbls)
}

#create combined veteran tables
vet_combined_tbls<-future_map2(vet_tbls,
                        list(version1 = vet_status_string,
                             version2 = c("0-5 years", "5+ years"),
                             version3 = c("0-3 years", "4-10 years","10+ years"),
                             version4 = c("0-3 years", "4-10 years","10+ years")),
                        function(x,y){
                          create_vet_combined_tbls_list(x,y)})

#To read, among all teachers (all teachers of color), x percentage of them
# have taught 10+ years. 

## -----------------------------------------------------------------------------
## Part 2.3 - Create Veteran Percentage Tables - By cohort
## -----------------------------------------------------------------------------

#add string
cs_cohort3<- cs_cohort2_3_string[!c(cs_cohort2_3_string %in% cs_cohort2_string)] 

#update function
create_vet_combined_tbls_list2<-function(vet_list, status_string){
  
  vet_tbls_overall_list<-map(school_string_list,
                             function(school_list){
                               map(school_list,
                                   function(string_sch){
                                     create_vet_tbls_overall_list(vet_list,
                                                                  string_sch,
                                                                  status_string) %>% 
                                       safe_function()
                                   })})
  
  #by neighborhood
  vet_tbls_neighborhood<-vector("list", 3)
  names(vet_tbls_neighborhood)<-c("overall", "cs", "ts")
  
  vet_tbls_neighborhood[["overall"]]<- map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch,status_string)%>% safe_function()})
  
  vet_tbls_neighborhood[["cs"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch[string_sch %in% cs_string],
                                 status_string) %>% safe_function()}) 
  
  vet_tbls_neighborhood[["ts"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list(vet_list,string_sch[string_sch %in%
                                                       ts_sch_string],
                                 status_string) %>% safe_function()}) 
  
  #By Cohort
  vet_tbls_cohort<-vector("list", 3)
  names(vet_tbls_cohort)<-c(str_c("cohort", c(1:3)))
  
  vet_tbls_cohort[["cohort1"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort1_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort2"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort2_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort3"]]<-create_vet_tbls_overall_list(vet_list,
                                                             cs_cohort3,
                                                             status_string) %>% safe_function()
  
  #combine all the lists together into one
  
  vet_combined_tbls<-vector("list", 3)
  names(vet_combined_tbls)<-c("overall", "neighborhood", "cohort")
  
  vet_combined_tbls[["overall"]]<-vet_tbls_overall_list
  vet_combined_tbls[["neighborhood"]]<-vet_tbls_neighborhood
  vet_combined_tbls[["cohort"]]<-vet_tbls_cohort
  
  return(vet_combined_tbls)
}

#update cohort values
update_cohort_values<-function(df){
  
  test<-df
  
  for (version in c("version1", "version2", "version3", "version4")){
    
    for (ppl_type in c("staff", "teachers")){
      
      for (sample in c("all","color")){
        
        for (type in c("count", "percent")){
          
          #clean cohort 2
          for (school in names(test[[version]][[ppl_type]][[sample]][[type]])) {
            if (school %in% cs_cohort2_string) {
              test[[version]][[ppl_type]][[sample]][[type]][[school]] <-
                test[[version]][[ppl_type]][[sample]][[type]][[school]] %>% 
                mutate(yr_18_19 = NA)
            }
          }
          
          #clean cohort 3
          cs_cohort3<- cs_cohort2_3_string[!c(cs_cohort2_3_string %in% cs_cohort2_string)] 
          
          for (school in names(test[[version]][[ppl_type]][[sample]][[type]])) {
            if (school %in% cs_cohort3) {
              test[[version]][[ppl_type]][[sample]][[type]][[school]]<-
                test[[version]][[ppl_type]][[sample]][[type]][[school]] %>% 
                mutate(yr_18_19 = NA, yr_19_20 = NA)
              
            }}}}}}
  
  return(test)
  }

vet_tbls_cohort<-update_cohort_values(vet_tbls)

vet_combined_tbls_cohort<-future_map2(vet_tbls_cohort,
                               list(version1 = vet_status_string,
                                    version2 = c("0-5 years", "5+ years"),
                                    version3 = c("0-3 years", "4-10 years","10+ years"),
                                    version4 = c("0-3 years", "4-10 years","10+ years")),
                               function(x,y){
                                 create_vet_combined_tbls_list2(x,y)})

## -----------------------------------------------------------------------------
## Part 2.4 - Create Teacher Demographic Tables - Overall
## -----------------------------------------------------------------------------
#see TOC here

#create demo overall
create_overall_vet_tbl_demo<-function(df, school_string,status_vet){
  
  df_update<-df %>% keep(names(df) %in% school_string)
  
  #Create Overall Retention Rate

  rent_overall<-bind_rows(df_update, .id = 'school') %>%
    filter(vet_status == status_vet)

  if ("toc" %in% colnames(rent_overall)){
    rent_overall<-rent_overall %>% filter(toc == 1)
  }
  
  rent_overall <- rent_overall %>%
    group_by(school, vet_status) %>%
    summarise(yr_18_19 = sum(yr_18_19, na.rm = T),
              yr_19_20 = sum(yr_19_20, na.rm = T),
              yr_20_21 = sum(yr_20_21, na.rm = T),
              yr_21_22 = sum(yr_21_22, na.rm = T),
              yr_22_23 = sum(yr_22_23, na.rm = T),
              yr_23_24 = sum(yr_23_24, na.rm = T)) %>% 
    ungroup()
  
  
  rent_overall<-rent_overall %>%
    mutate(
      yr_18_19 = as.numeric(yr_18_19),
      yr_19_20 = as.numeric(yr_19_20),
      yr_20_21 = as.numeric(yr_20_21),
      yr_21_22 = as.numeric(yr_21_22),
      yr_22_23 = as.numeric(yr_22_23),
      yr_23_24 = as.numeric(yr_23_24),
    )

  pivot_rent<-rent_overall %>%
    summarize(
      yr_18_19 = sum(yr_18_19, na.rm = T)/n() %>% round(),
      yr_19_20 = sum(yr_19_20, na.rm = T)/n() %>% round(),
      yr_20_21 = sum(yr_20_21, na.rm = T)/n() %>% round(),
      yr_21_22 = sum(yr_21_22, na.rm = T)/n() %>% round(),
      yr_22_23 = sum(yr_22_23, na.rm = T)/n() %>% round(),
      yr_23_24 = sum(yr_23_24, na.rm = T)/n() %>% round(),

    )

  pivot_rent$school<- "Percent of TOC Teachers"

  pivot_rent$vet_status<- "Percent of TOC Teachers"
  
  pivot_rent<-pivot_rent %>% select(school, vet_status, everything())

  #if (tbl_type == "Retention Rate %"){
  rent_overall<-rbind(rent_overall, pivot_rent)

  #round rental numbers to whole numbers

  rent_overall<-rent_overall %>%
    mutate(yr_18_19 = round(yr_18_19),
           yr_19_20 = round(yr_19_20),
           yr_20_21 = round(yr_20_21),
           yr_21_22 = round(yr_21_22),
           yr_22_23 = round(yr_22_23),
           yr_23_24 = round(yr_23_24)
    )


  return(rent_overall)
}

#check
test<-create_overall_vet_tbl_demo(demo_tbl_2[["teachers"]][["all"]][["percent"]],
                                  cs_string, "10+ years")

#overall demo tables
create_vet_tbls_overall_list_demo<-function(df_tbls, schools, status_string){
  
  df_list<-map(df_tbls, function(teach_staff){
    
    map(teach_staff,
        function(teach_type){
          map(teach_type,
              function(count_percent_type){
                df_update<-map(status_string, function(status){
                  create_overall_vet_tbl_demo(count_percent_type,
                                         schools,status)
                })
                names(df_update)<-status_string
                return(df_update)
              })  
        }) 
  })
  
  return(df_list)
  
}

#create demo combined tables function
create_vet_combined_tbls_list_demo<-function(vet_list, status_string){
  
  vet_tbls_overall_list<-map(school_string_list,
                             function(school_list){
                               map(school_list,
                                   function(string_sch){
                                     create_vet_tbls_overall_list_demo(vet_list,
                                                                  string_sch,
                                                                  status_string) %>% 
                                       safe_function()
                                   })})
  
  #by neighborhood
  vet_tbls_neighborhood<-vector("list", 3)
  names(vet_tbls_neighborhood)<-c("overall", "cs", "ts")
  
  vet_tbls_neighborhood[["overall"]]<- map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list_demo(vet_list,string_sch,status_string)%>% safe_function()})
  
  vet_tbls_neighborhood[["cs"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list_demo(vet_list,string_sch[string_sch %in% cs_string],
                                 status_string) %>% safe_function()}) 
  
  vet_tbls_neighborhood[["ts"]]<-map(neighborhood_string_list,function(string_sch){
    create_vet_tbls_overall_list_demo(vet_list,string_sch[string_sch %in%
                                                       ts_sch_string],
                                 status_string) %>% safe_function()}) 
  
  #By Cohort
  vet_tbls_cohort<-vector("list", 3)
  names(vet_tbls_cohort)<-c(str_c("cohort", c(1:3)))
  
  vet_tbls_cohort[["cohort1"]]<-create_vet_tbls_overall_list_demo(vet_list,
                                                             cs_cohort1_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort2"]]<-create_vet_tbls_overall_list_demo(vet_list,
                                                             cs_cohort2_string,
                                                             status_string) %>% 
    safe_function()
  
  vet_tbls_cohort[["cohort3"]]<-create_vet_tbls_overall_list_demo(vet_list,
                                                                  cs_cohort3,
                                                             status_string) %>% safe_function()
  
  #combine all the lists together into one
  
  vet_combined_tbls<-vector("list", 3)
  names(vet_combined_tbls)<-c("overall", "neighborhood", "cohort")
  
  vet_combined_tbls[["overall"]]<-vet_tbls_overall_list
  vet_combined_tbls[["neighborhood"]]<-vet_tbls_neighborhood
  vet_combined_tbls[["cohort"]]<-vet_tbls_cohort
  
  return(vet_combined_tbls)
}

#percentages reflect the percentage of BIPOC teachers per veteran status
#Among veteran teachers, what is the percentage of BIPOC teachers
demo_tbl<-update_demo_tbls(hr_by_sch)

#update percentages to reflect whole school
#What is the percentage of BIPOC teachers who are veteran teachers in the school?
demo_tbl2<-update_demo_tbls2(hr_by_sch)

#demo combined table
demo_combined_tbls<-create_vet_combined_tbls_list_demo(demo_tbl,
                        c("0-3 years", "4-10 years","10+ years"))

#combined table - version 2 reflects whole school
demo_combined_tbls2<-create_vet_combined_tbls_list_demo(demo_tbl2,
                            c("0-3 years", "4-10 years","10+ years"))

## -----------------------------------------------------------------------------
## Part 2.5 - Create Teacher Demographic Tables - By Cohort
## -----------------------------------------------------------------------------
#see TOC here
#update cohort values

#NOTE: version 2 reflects TOC percentage based on the whole school

update_cohort_values_demo<-function(df){
  
  test<-df
  
    for (ppl_type in c("staff", "teachers")){
      
      for (sample in c("all")){
        
        for (type in c("count", "percent")){
          
          #clean cohort 2
          for (school in names(test[[ppl_type]][[sample]][[type]])) {
            if (school %in% cs_cohort2_string) {
              test[[ppl_type]][[sample]][[type]][[school]] <-
                test[[ppl_type]][[sample]][[type]][[school]] %>% 
                mutate(yr_18_19 = NA)
            }
          }
          
          #clean cohort 3
          cs_cohort3<- cs_cohort2_3_string[!c(cs_cohort2_3_string %in% cs_cohort2_string)] 
          
          for (school in names(test[[ppl_type]][[sample]][[type]])) {
            if (school %in% cs_cohort3) {
              test[[ppl_type]][[sample]][[type]][[school]]<-
                test[[ppl_type]][[sample]][[type]][[school]] %>% 
                mutate(yr_18_19 = NA, yr_19_20 = NA)
              
            }}}}}
  
  return(test)
}


#update cohort values
demo_tbl_cohort<-update_cohort_values_demo(demo_tbl)


demo_tbl_cohort2<-update_cohort_values_demo(demo_tbl2)

#demo combined table
demo_combined_tbls_cohort<-create_vet_combined_tbls_list_demo(demo_tbl_cohort,
                                      c("0-3 years", "4-10 years","10+ years"))

demo_combined_tbls_cohort2<-create_vet_combined_tbls_list_demo(demo_tbl_cohort2,
                                      c("0-3 years", "4-10 years","10+ years"))

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(vet_combined_tbls,vet_tbls,vet_combined_tbls_cohort,
     file = file.path(code_file_dir, "vet_combined_tbls.RData"))

save(demo_tbl, demo_tbl2, demo_combined_tbls,demo_combined_tbls2,
     demo_tbl_cohort, demo_tbl_cohort2,
     demo_combined_tbls_cohort, demo_combined_tbls_cohort2,
     file = file.path(code_file_dir, "demo_combined_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------