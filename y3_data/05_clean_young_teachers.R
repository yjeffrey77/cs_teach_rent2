################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 05_clean_young_teachers_data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 10/6/24 >
##
################################################################################

#Goal: Conducts additional cleaning of Y3 Data by doing the following:

#1. Run Descriptives Having only 0-3 year-aged teachers
#2. Run Analyses without tenured teachers in the 0-5 year-aged category
#3. Run Analyses without tenured teachers in the 0-3 year-aged category

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()



## ---------------------------
## file directories
## ---------------------------

code_file_dir<-file.path("C:/Users/yjeff/Desktop/Community Schools/Teacher Retention Data",
                         "Analyses","cs_teach_rent2","y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")


data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

source(file.path(code_file_dir,"05_clean_young_teachers_functions.R"))

## ---------------------------
## load & inspect data
## ---------------------------

hr_data<-read_excel(file.path(data_file_dir,
                              "CoS_Employee_Retention_HR_Data_20240523.xlsx"))

hr_data_norm<-read_excel(file.path(data_file_dir,
                                   "CoS_Full_Certificated_Employee_Norm_Day_List_20240523.xlsx"))

#read in updated job_description file
job_desc<-read.csv(file.path(data_file_dir_yr2,"UCLA_Cos_Teacher_Retention",
                             "job_description_updated.csv")) %>%
  select(-c(X, notes)) %>% 
  select(-c(job_type, job_type2, decision, josh_notes)) #Use this code to only keep the most updated job_type

## -----------------------------------------------------------------------------
## Part 1.1 - Clean Data: Names and Job Titles
## -----------------------------------------------------------------------------

hr_data_norm_c<-clean_names(hr_data_norm)

#add classroom, outside classroom, and admin positions in this data
hr_data_job_desc<-hr_data_norm_c %>% select(job_text) %>% distinct()

hr_data_job_desc<-hr_data_job_desc %>%
  left_join(job_desc, by = c("job_text" = "job_desc"))

#check
hr_data_job_desc %>% filter(is.na(job_type3))

#SIG School Coord,Temp Adv - considered staff?
#PRINCIPAL, SCHOOL PREGNT - considered admin?

#update job_text
hr_data_job_desc<-hr_data_job_desc %>%
  mutate(job_type = case_when(
    job_text %in% c("TEACHER LIBRARIAN") ~ "staff",
    job_text %in% c("SIG School Coord,Temp Adv") ~ "staff",
    job_text %in% c("PRINCIPAL, SCHOOL PREGNT") ~ "admin",
    TRUE ~ job_type3
  ))

#left join into hr_data_norm_c
hr_data_norm_c<-hr_data_norm_c %>%
  left_join(hr_data_job_desc, by = "job_text") 

#remove job_type3
hr_data_norm_c<-hr_data_norm_c %>%
  select(-c(job_type3))

## -----------------------------------------------------------------------------
## Part 1.2 - Clean Hire Dates 
## -----------------------------------------------------------------------------

#Data
#x0000_start_date,x0000_end_date, contract_date, tenure_date
#Which date should I consider? Only hire date is the stable one.

#Clean Hire Dates
hr_data_norm_c<-hr_data_norm_c %>% 
  mutate(
    hire_date = as.character(hire_date)  
  )
  
hr_data_norm_c<-hr_data_norm_c %>% 
  mutate(
    hire_date = case_when(
      generated_pseudo_id %in% c("Generated02_2701") ~ "2017-10-31",
      generated_pseudo_id %in% c("Generated02_0972") ~ "2016-04-20",
      generated_pseudo_id %in% c("Generated02_1252") ~ "2016-09-21",
      generated_pseudo_id %in% c("Generated02_1948") ~ "2009-01-22",
      generated_pseudo_id %in% c("Generated02_2224") ~ "2016-10-24",
      generated_pseudo_id %in% c("Generated02_3151") ~ "2014-09-09",
      generated_pseudo_id %in% c("Generated02_3460") ~ "2018-11-15",
      generated_pseudo_id %in% c("Generated02_3684") ~ "2017-08-21",
      generated_pseudo_id %in% c("Generated02_4220") ~ "2019-09-30",
      generated_pseudo_id %in% c("Generated02_4448") ~ "2018-08-13",
      generated_pseudo_id %in% c("Generated02_5830") ~ "1996-04-09",
      generated_pseudo_id %in% c("Generated02_6102") ~ "2016-08-17",
      generated_pseudo_id %in% c("Generated02_7030") ~ "2019-11-05",
      generated_pseudo_id %in% c("Generated02_7658") ~ "1990-05-29",
      TRUE ~ hire_date
    ),
    gender_text = case_when(
      generated_pseudo_id %in% c("Generated02_6354") ~ "Female",
      TRUE ~ gender_text
    )
  )

## -----------------------------------------------------------------------------
## Part 1.3 - Update school names
## -----------------------------------------------------------------------------

#Subset data by school
school_names_tbl<-hr_data_norm_c %>% count(parent_school_name)

#make a string version
school_names_string<-school_names_tbl$parent_school_name

#write out school name
#write.csv(school_names_tbl, file.path(code_file_dir,"school_names.csv"))

#read in updated school names (cleaned version of school_names.csv)
school_names_tbl_update<-read.csv(file.path(code_file_dir,"school_names_update.csv"))

school_names_tbl_update<-school_names_tbl_update %>% select(-c(X))

#join updated names to table

hr_data_norm_c<-hr_data_norm_c %>%
  left_join(school_names_tbl_update, by = "parent_school_name")

## -----------------------------------------------------------------------------
## Part 1.4 - Transform to Wide Format
## -----------------------------------------------------------------------------

#Change HR Data to Wide Format
hr_data_wide<-hr_data_norm_c %>%
  select(-c(x0000_start_date,x0000_end_date, contract_date,
            parent_school_name))


hr_data_wide<-hr_data_wide %>% 
  pivot_wider(names_from = school_year,
              values_from = c(employment_status_text,
                              employee_subgroup_text,
                              school_name_clean,
                              position,
                              position_text,
                              job, job_text,
                              job_type),
              names_prefix = "year_")
#check
test<-hr_data_wide %>% count(generated_pseudo_id)

#hr_ethnicity,generated_pseudo_id,
#x0000_start_date,hire_date, contract_date,
#tenure_date, x0000_end_date,

## -----------------------------------------------------------------------------
## Part 1.5 - Add Dates
## -----------------------------------------------------------------------------

#add age years (numeric)
hr_data_wide<-hr_data_wide %>%
  mutate(
    #teacher years
    teacher_years_18_19 = 2019 - year(hire_date),
    teacher_years_19_20 = 2020 - year(hire_date),
    teacher_years_20_21 = 2021 - year(hire_date),
    teacher_years_21_22 = 2022 - year(hire_date),
    teacher_years_22_23 = 2023 - year(hire_date),
    teacher_years_23_24 = 2024 - year(hire_date),
    #tenure years
    tenure_years_18_19 = 2019 - year(tenure_date),
    tenure_years_19_20 = 2020 - year(tenure_date),
    tenure_years_20_21 = 2021 - year(tenure_date),
    tenure_years_21_22 = 2022 - year(tenure_date),
    tenure_years_22_23 = 2023 - year(tenure_date),
    tenure_years_23_24 = 2024 - year(tenure_date),
  )

#add difference in years
hr_data_wide<-hr_data_wide %>%
  mutate(
    #teacher years
    diff_years_18_19 = teacher_years_18_19 - tenure_years_18_19,
    diff_years_19_20 = teacher_years_19_20 - tenure_years_19_20,
    diff_years_20_21 = teacher_years_20_21 - tenure_years_20_21,
    diff_years_21_22 = teacher_years_21_22 - tenure_years_21_22,
    diff_years_22_23 = teacher_years_22_23 - tenure_years_22_23,
    diff_years_23_24 = teacher_years_23_24 - tenure_years_23_24,
  )

#check
test<-hr_data_wide %>% count(diff_years_18_19)
test<-hr_data_wide %>% filter(diff_years_23_24 == 0)

#When looking at the breakdown, we see that there are the following:
# 1 teacher that had a tenure date before the hire date
# 2,904 teachers who had the same hire and tenure year
# 614 teachers who had a tenure date 1 year after hire date
# 411 teacher who had a tenure date 2 years after hire date

#However, I notice the hire years and tenure years 
#can vary drastically. This makes me question the hire years: 

#I wonder if individuals who are first hired, they may not be considered as
# a teacher. They may have been in the system for years as a staff.

#Given that tenure in CA if the teacher has taught for 2 teachers, I will 
#provide a more "conservative" of teacher years, which is adding 2 to the 
#difference between the hire date and the academic year. 

#teacher years (version 2)
hr_data_wide<-hr_data_wide %>%
  mutate(
    #teacher years
    teacher_years_18_19v2 = tenure_years_18_19 + 2,
    teacher_years_19_20v2 = tenure_years_19_20 + 2,
    teacher_years_20_21v2 = tenure_years_20_21 + 2,
    teacher_years_21_22v2 = tenure_years_21_22 + 2,
    teacher_years_22_23v2 = tenure_years_22_23 + 2,
    teacher_years_23_24v2 = tenure_years_23_24 + 2
  )

test<-hr_data_wide %>% count(teacher_years_23_24v2)
test<-hr_data_wide %>% filter(teacher_years_23_24v2<0)

# Note only one teacher that was considered tenured before being hired

#As a result, kept the individual in the data. With this data, it is 
#not possible to determine possible teachers entered the 
  
#add age years (categorical)
hr_data_wide<-hr_data_wide %>%
  mutate(
    teacher_years_18_19_c = case_when(
      teacher_years_18_19 <= 5 ~ "0-5 years",
      (teacher_years_18_19 > 5 & teacher_years_18_19 <= 10) ~ "6-10 years",
      (teacher_years_18_19 > 10 & teacher_years_18_19 <= 15) ~ "11-15 years",
      (teacher_years_18_19 > 15 ) ~ "15+ years",
    ),
    teacher_years_19_20_c = case_when(
      teacher_years_19_20 <= 5 ~ "0-5 years",
      (teacher_years_19_20 > 5 & teacher_years_19_20 <= 10) ~ "6-10 years",
      (teacher_years_19_20 > 10 & teacher_years_19_20 <= 15) ~ "11-15 years",
      (teacher_years_19_20 > 15 ) ~ "15+ years",
    ),
    teacher_years_20_21_c = case_when(
      teacher_years_20_21 <= 5 ~ "0-5 years",
      (teacher_years_20_21 > 5 & teacher_years_20_21 <= 10) ~ "6-10 years",
      (teacher_years_20_21 > 10 & teacher_years_20_21 <= 15) ~ "11-15 years",
      (teacher_years_20_21 > 15 ) ~ "15+ years",
    ),
    teacher_years_21_22_c = case_when(
      teacher_years_21_22 <= 5 ~ "0-5 years",
      (teacher_years_21_22 > 5 & teacher_years_21_22 <= 10) ~ "6-10 years",
      (teacher_years_21_22 > 10 & teacher_years_21_22 <= 15) ~ "11-15 years",
      (teacher_years_21_22 > 15 ) ~ "15+ years",
    ),
    teacher_years_22_23_c = case_when(
      teacher_years_22_23 <= 5 ~ "0-5 years",
      (teacher_years_22_23 > 5 & teacher_years_22_23 <= 10) ~ "6-10 years",
      (teacher_years_22_23 > 10 & teacher_years_22_23 <= 15) ~ "11-15 years",
      (teacher_years_22_23 > 15 ) ~ "15+ years",
    ),
    teacher_years_23_24_c = case_when(
      teacher_years_23_24 <= 5 ~ "0-5 years",
      (teacher_years_23_24 > 5 & teacher_years_23_24 <= 10) ~ "6-10 years",
      (teacher_years_23_24 > 10 & teacher_years_23_24 <= 15) ~ "11-15 years",
      (teacher_years_23_24 > 15 ) ~ "15+ years",
    ))

check<-hr_data_wide %>% select(hire_date,
                               teacher_years_18_19, teacher_years_18_19_c)
hr_data_wide %>% count(teacher_years_18_19_c)


#add age years (categorical) - 0-3 years version
hr_data_wide<-hr_data_wide %>%
  mutate(
    teacher_years_18_19_cv2 = case_when(
      teacher_years_18_19 <= 3 ~ "0-3 years",
      (teacher_years_18_19 > 4 & teacher_years_18_19 <= 5) ~ "4-5 years",
      (teacher_years_18_19 > 5 & teacher_years_18_19 <= 10) ~ "6-10 years",
      (teacher_years_18_19 > 10 & teacher_years_18_19 <= 15) ~ "11-15 years",
      (teacher_years_18_19 > 15 ) ~ "15+ years",
    ),
    teacher_years_19_20_cv2 = case_when(
      teacher_years_19_20 <= 3 ~ "0-3 years",
      (teacher_years_19_20 > 4 & teacher_years_19_20 <= 5) ~ "4-5 years",
      (teacher_years_19_20 > 5 & teacher_years_19_20 <= 10) ~ "6-10 years",
      (teacher_years_19_20 > 10 & teacher_years_19_20 <= 15) ~ "11-15 years",
      (teacher_years_19_20 > 15 ) ~ "15+ years",
    ),
    teacher_years_20_21_cv2 = case_when(
      teacher_years_20_21 <= 3 ~ "0-3 years",
      (teacher_years_20_21 > 4 & teacher_years_20_21 <= 5) ~ "4-5 years",
      (teacher_years_20_21 > 5 & teacher_years_20_21 <= 10) ~ "6-10 years",
      (teacher_years_20_21 > 10 & teacher_years_20_21 <= 15) ~ "11-15 years",
      (teacher_years_20_21 > 15 ) ~ "15+ years",
    ),
    teacher_years_21_22_cv2 = case_when(
      teacher_years_21_22 <= 3 ~ "0-3 years",
      (teacher_years_21_22 > 4 & teacher_years_21_22 <= 5) ~ "4-5 years",
      (teacher_years_21_22 > 5 & teacher_years_21_22 <= 10) ~ "6-10 years",
      (teacher_years_21_22 > 10 & teacher_years_21_22 <= 15) ~ "11-15 years",
      (teacher_years_21_22 > 15 ) ~ "15+ years",
    ),
    teacher_years_22_23_cv2 = case_when(
      teacher_years_22_23 <= 3 ~ "0-3 years",
      (teacher_years_22_23 > 4 & teacher_years_22_23 <= 5) ~ "4-5 years",
      (teacher_years_22_23 > 5 & teacher_years_22_23 <= 10) ~ "6-10 years",
      (teacher_years_22_23 > 10 & teacher_years_22_23 <= 15) ~ "11-15 years",
      (teacher_years_22_23 > 15 ) ~ "15+ years",
    ),
    teacher_years_23_24_cv2 = case_when(
      teacher_years_23_24 <= 3 ~ "0-3 years",
      (teacher_years_23_24 > 4 & teacher_years_23_24 <= 5) ~ "4-5 years",
      (teacher_years_23_24 > 5 & teacher_years_23_24 <= 10) ~ "6-10 years",
      (teacher_years_23_24 > 10 & teacher_years_23_24 <= 15) ~ "11-15 years",
      (teacher_years_23_24 > 15 ) ~ "15+ years",
    ))

hr_data_wide %>% count(teacher_years_18_19_cv2)


##add age years (categorical) - tenure version
hr_data_wide<-hr_data_wide %>%
  mutate(
    teacher_years_18_19_t = case_when(
      teacher_years_18_19v2 <= 5 ~ "0-5 years",
      (teacher_years_18_19v2 > 5 & teacher_years_18_19v2 <= 10) ~ "6-10 years",
      (teacher_years_18_19v2 > 10 & teacher_years_18_19v2 <= 15) ~ "11-15 years",
      (teacher_years_18_19v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_19_20_t = case_when(
      teacher_years_19_20v2 <= 5 ~ "0-5 years",
      (teacher_years_19_20v2 > 5 & teacher_years_19_20v2 <= 10) ~ "6-10 years",
      (teacher_years_19_20v2 > 10 & teacher_years_19_20v2 <= 15) ~ "11-15 years",
      (teacher_years_19_20v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_20_21_t = case_when(
      teacher_years_20_21v2 <= 5 ~ "0-5 years",
      (teacher_years_20_21v2 > 5 & teacher_years_20_21v2 <= 10) ~ "6-10 years",
      (teacher_years_20_21v2 > 10 & teacher_years_20_21v2 <= 15) ~ "11-15 years",
      (teacher_years_20_21v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_21_22_t = case_when(
      teacher_years_21_22v2 <= 5 ~ "0-5 years",
      (teacher_years_21_22v2 > 5 & teacher_years_21_22v2 <= 10) ~ "6-10 years",
      (teacher_years_21_22v2 > 10 & teacher_years_21_22v2 <= 15) ~ "11-15 years",
      (teacher_years_21_22v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_22_23_t = case_when(
      teacher_years_22_23v2 <= 5 ~ "0-5 years",
      (teacher_years_22_23v2 > 5 & teacher_years_22_23v2 <= 10) ~ "6-10 years",
      (teacher_years_22_23v2 > 10 & teacher_years_22_23v2 <= 15) ~ "11-15 years",
      (teacher_years_22_23v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_23_24_t = case_when(
      teacher_years_23_24v2 <= 5 ~ "0-5 years",
      (teacher_years_23_24v2 > 5 & teacher_years_23_24v2 <= 10) ~ "6-10 years",
      (teacher_years_23_24v2 > 10 & teacher_years_23_24v2 <= 15) ~ "11-15 years",
      (teacher_years_23_24v2 > 15 ) ~ "15+ years",
    ))

##add age years (categorical) - tenure version
hr_data_wide<-hr_data_wide %>%
  mutate(
    teacher_years_18_19_tv2 = case_when(
      teacher_years_18_19v2 <= 3 ~ "0-3 years",
      (teacher_years_18_19v2 > 4 & teacher_years_18_19v2 <= 5) ~ "4-5 years",
      (teacher_years_18_19v2 > 5 & teacher_years_18_19v2 <= 10) ~ "6-10 years",
      (teacher_years_18_19v2 > 10 & teacher_years_18_19v2 <= 15) ~ "11-15 years",
      (teacher_years_18_19v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_19_20_tv2 = case_when(
      teacher_years_19_20v2 <= 3 ~ "0-3 years",
      (teacher_years_19_20v2 > 4 & teacher_years_19_20v2 <= 5) ~ "4-5 years",
      (teacher_years_19_20v2 > 5 & teacher_years_19_20v2 <= 10) ~ "6-10 years",
      (teacher_years_19_20v2 > 10 & teacher_years_19_20v2 <= 15) ~ "11-15 years",
      (teacher_years_19_20v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_20_21_tv2 = case_when(
      teacher_years_20_21v2 <= 3 ~ "0-3 years",
      (teacher_years_20_21v2 > 4 & teacher_years_20_21v2 <= 5) ~ "4-5 years",
      (teacher_years_20_21v2 > 5 & teacher_years_20_21v2 <= 10) ~ "6-10 years",
      (teacher_years_20_21v2 > 10 & teacher_years_20_21v2 <= 15) ~ "11-15 years",
      (teacher_years_20_21v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_21_22_tv2 = case_when(
      teacher_years_21_22v2 <= 3 ~ "0-3 years",
      (teacher_years_21_22v2 > 4 & teacher_years_21_22v2 <= 5) ~ "4-5 years",
      (teacher_years_21_22v2 > 5 & teacher_years_21_22v2 <= 10) ~ "6-10 years",
      (teacher_years_21_22v2 > 10 & teacher_years_21_22v2 <= 15) ~ "11-15 years",
      (teacher_years_21_22v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_22_23_tv2 = case_when(
      teacher_years_22_23v2 <= 3 ~ "0-3 years",
      (teacher_years_22_23v2 > 4 & teacher_years_22_23v2 <= 5) ~ "4-5 years",
      (teacher_years_22_23v2 > 5 & teacher_years_22_23v2 <= 10) ~ "6-10 years",
      (teacher_years_22_23v2 > 10 & teacher_years_22_23v2 <= 15) ~ "11-15 years",
      (teacher_years_22_23v2 > 15 ) ~ "15+ years",
    ),
    teacher_years_23_24_tv2 = case_when(
      teacher_years_23_24v2 <= 3 ~ "0-3 years",
      (teacher_years_23_24v2 > 4 & teacher_years_23_24v2 <= 5) ~ "4-5 years",
      (teacher_years_23_24v2 > 5 & teacher_years_23_24v2 <= 10) ~ "6-10 years",
      (teacher_years_23_24v2 > 10 & teacher_years_23_24v2 <= 15) ~ "11-15 years",
      (teacher_years_23_24v2 > 15 ) ~ "15+ years",
    ))


## -----------------------------------------------------------------------------
## Part 1.6 - Create Subsamples
## -----------------------------------------------------------------------------

hr_data_wide %>% count(hr_ethnicity)

#only teachers of color
hr_data_color<-hr_data_wide %>%
  filter(!(hr_ethnicity %in% c("Two or More", "Undeclared",
                               "White")))

hr_data_color %>% count(hr_ethnicity)


hr_data_color_list<-vector("list", 4)
names(hr_data_color_list)<-c("original_4", "original_5",
                             "tenure_4", "tenure_5")

#add names
year_category_4<-c("0-5 years","6-10 years","11-15 years","15+ years")
year_category_5<-c("0-3 years","4-5 years",
                   "6-10 years","11-15 years","15+ years")

#original_4
hr_data_color_list[["original_4"]]<-vector("list", 4)

names(hr_data_color_list[["original_4"]])<-c(year_category_4)

#hr_data_color_list[["original_4"]][["overall"]]<-hr_data_color

for(teach_year in year_category_4){
  hr_data_color_list[["original_4"]][[teach_year]]<-
    filter_age_years(hr_data_color, teach_year)
}

#original_5
hr_data_color_list[["original_5"]]<-vector("list", 5)

names(hr_data_color_list[["original_5"]])<-year_category_5

for(teach_year in year_category_5){
  hr_data_color_list[["original_5"]][[teach_year]]<-
    filter_age_years_v2(hr_data_color, teach_year)
}

#tenure_4
hr_data_color_list[["tenure_4"]]<-vector("list", 4)

names(hr_data_color_list[["tenure_4"]])<-year_category_4

for(teach_year in year_category_4){
  hr_data_color_list[["tenure_4"]][[teach_year]]<-
    filter_age_years_t(hr_data_color, teach_year)
}

#tenure_5
hr_data_color_list[["tenure_5"]]<-vector("list", 5)

names(hr_data_color_list[["tenure_5"]])<-year_category_5

for(teach_year in year_category_5){
  hr_data_color_list[["tenure_5"]][[teach_year]]<-
    filter_age_years_tv2(hr_data_color, teach_year)
}


## -----------------------------------------------------------------------------
## Part 2 - Subset Sample by School
## -----------------------------------------------------------------------------

#make a string version
school_names_string2<-school_names_tbl_update$school_name_clean %>% unique()

#create list (overall)
hr_by_sch<-vector("list", length(school_names_string2))
names(hr_by_sch)<-school_names_string2

#all teachers
hr_by_sch<-map(school_names_string2,
               function(x){filter_school(hr_data_wide,x)})

names(hr_by_sch)<-school_names_string2

#teachers of color
hr_by_sch_color<-map(school_names_string2,
                     function(x){filter_school(hr_data_color,x)})

names(hr_by_sch_color)<-school_names_string2

#Create list (TOC and by age)
hr_color_by_sch<-map(hr_data_color_list,
          function(x) map(x,create_sch_list))


# test<-map(hr_data_color_list,
#                      function(x) map(x,create_sch_list))  
# 
# 
# 
# hr_color_by_sch<-map(hr_data_color_list,
#                      function(x) map(x,
#                                      function(x) map(school_names_string2,
#                                            function(x){filter_school(hr_data_wide,x)})))

## -----------------------------------------------------------------------------
## Part 4 - Add Retention Type Variables
## -----------------------------------------------------------------------------

hr_by_sch<-map2(hr_by_sch,names(hr_by_sch),
               function(x,y){update_rent_variables(x,y)})


#By school 
hr_by_sch_color<-map2(hr_by_sch_color,names(hr_by_sch_color),
                      function(x,y){update_rent_variables(x,y)})


#by veteran status
hr_color_by_sch<-map(hr_color_by_sch,
          function(sample) 
            {map(sample,
                 function(teach_year){
                   map(teach_year,
                       function(a) {map2(a, names(a),
                                  function(x,y){update_rent_variables(x,y) %>%
                                      safe_function()})})})})

## -----------------------------------------------------------------------------
## Part 5 - Save Data
## -----------------------------------------------------------------------------

save(hr_color_by_sch, hr_by_sch, hr_by_sch_color,
     file = file.path(code_file_dir,'hr_color_by_sch.RData'))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------