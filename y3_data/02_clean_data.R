################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 02_clean_data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/4/24 >
##
################################################################################

#Goal: Cleans the Y3 Data to prepare for analysis.

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
                         "Analyses", "cs_teach_rent2", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")


data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

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
  select(-c(x0000_start_date,x0000_end_date, contract_date, tenure_date,
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
    teacher_years_18_19 = 2019 - year(hire_date),
    teacher_years_19_20 = 2020 - year(hire_date),
    teacher_years_20_21 = 2021 - year(hire_date),
    teacher_years_21_22 = 2022 - year(hire_date),
    teacher_years_22_23 = 2023 - year(hire_date),
    teacher_years_23_24 = 2024 - year(hire_date),
  )
  
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
    ),
  )

check<-hr_data_wide %>% select(hire_date,
                               teacher_years_18_19, teacher_years_18_19_c)

hr_data_wide %>% count(teacher_years_18_19_c)


## -----------------------------------------------------------------------------
## Part 1.6 - Create Subsamples
## -----------------------------------------------------------------------------

hr_data_wide %>% count(hr_ethnicity)

#only teachers of color
hr_data_color<-hr_data_wide %>%
  filter(!(hr_ethnicity %in% c("Two or More", "Undeclared",
                               "White")))

hr_data_color %>% count(hr_ethnicity)

#add function
filter_age_years<-function(df, years){
  
  df_list<-vector("list",6)
  names(df_list)<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  
  df_list[["18_19"]]<-df %>% filter(teacher_years_18_19_c == years)
  df_list[["19_20"]]<-df %>% filter(teacher_years_19_20_c == years)
  df_list[["20_21"]]<-df %>% filter(teacher_years_20_21_c == years)
  df_list[["21_22"]]<-df %>% filter(teacher_years_21_22_c == years)
  df_list[["22_23"]]<-df %>% filter(teacher_years_22_23_c == years)
  df_list[["23_24"]]<-df %>% filter(teacher_years_23_24_c == years)
  
  return(df_list)
}

#add hr_color_list
hr_color_list<-vector("list", 5)
names(hr_color_list)<-c("overall","0-5 years","6-10 years",
                        "11-15 years","15+ years")

hr_color_list[["overall"]]<-hr_data_color


hr_data_color %>% count(teacher_years_18_19_c)
hr_data_color %>% count(teacher_years_19_20_c)
hr_data_color %>% count(teacher_years_20_21_c)
hr_data_color %>% count(teacher_years_21_22_c)
hr_data_color %>% count(teacher_years_22_23_c)
hr_data_color %>% count(teacher_years_23_24_c)


for(teach_year in c("0-5 years","6-10 years","11-15 years","15+ years")){
  hr_color_list[[teach_year]]<-filter_age_years(hr_data_color, teach_year)
}



## -----------------------------------------------------------------------------
## Part 2 - Subset Sample by School
## -----------------------------------------------------------------------------

#add filter_school function
filter_school<-function(df, sch_name){
  df_update<-df %>%
    filter(`school_name_clean_year_2018-19` == sch_name |
             `school_name_clean_year_2019-20` == sch_name |
             `school_name_clean_year_2020-21` == sch_name |
             `school_name_clean_year_2021-22` == sch_name |
             `school_name_clean_year_2022-23` == sch_name |
             `school_name_clean_year_2023-24` == sch_name)
  
  return(df_update)
}

#make a string version
school_names_string2<-school_names_tbl_update$school_name_clean %>% unique()

#create list (overall)
hr_by_sch<-vector("list", length(school_names_string2))
names(hr_by_sch)<-school_names_string2

hr_by_sch<-map(school_names_string2,
               function(x){filter_school(hr_data_wide,x)})

names(hr_by_sch)<-school_names_string2

#Create list (TOC and by age)
toc_hr_by_sch_list<-vector('list', 5)
names(toc_hr_by_sch_list)<-names(hr_color_list)

toc_hr_by_sch_list[["overall"]]<-map(school_names_string2,
                                     function(x){filter_school(hr_color_list[["overall"]],x)})

names(toc_hr_by_sch_list[["overall"]])<-school_names_string2


toc_hr_by_sch_list[["0-5 years"]]<-map(hr_color_list[["0-5 years"]],
                                       function(y){
                                         map(school_names_string2,
                                             function(x){filter_school(y,x)})})

toc_hr_by_sch_list[["6-10 years"]]<-map(hr_color_list[["6-10 years"]],
                                       function(y){
                                         map(school_names_string2,
                                             function(x){filter_school(y,x)})})

toc_hr_by_sch_list[["11-15 years"]]<-map(hr_color_list[["11-15 years"]],
                                       function(y){
                                         map(school_names_string2,
                                             function(x){filter_school(y,x)})})

toc_hr_by_sch_list[["15+ years"]]<-map(hr_color_list[["15+ years"]],
                                       function(y){
                                         map(school_names_string2,
                                             function(x){filter_school(y,x)})})

#update names

sch_year_string<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
teach_string<-c("0-5 years","6-10 years","11-15 years","15+ years")

for (teach_year in teach_string){
  for (sch_year in sch_year_string){
    names(toc_hr_by_sch_list[[teach_year]][[sch_year]])<-school_names_string2
  }
}


## -----------------------------------------------------------------------------
## Part 3 - Create Rent Type Functions
## -----------------------------------------------------------------------------

#helper function
safe_function_desc_tbl<-function(expr, version){
  result<- tryCatch(
    {
      expr
    },
    error = function(e){
      
      # Add a row of NA values
      vec <- colnames(toc_hr_by_sch_list[["overall"]][["107th Street Elementary"]])
      # Transform the vector into a data frame with each element as a column
      df <- as.data.frame(t(vec))
      
      colnames(df)<-colnames(toc_hr_by_sch_list[["overall"]][["107th Street Elementary"]])
      
      # Add a row of NA values
      df[nrow(df),] <- NA
      
      return(df)
      
    }
    
  )
  return(result)
}

#Add variables
add_variables<-function(df, sch_name){
  
  df_update<-df %>% mutate(
    in_sch_18_19 = if_else(`school_name_clean_year_2018-19` == sch_name,1, 0, missing = 0),
    in_sch_19_20 = if_else(`school_name_clean_year_2019-20` == sch_name,1, 0, missing = 0),
    in_sch_20_21 = if_else(`school_name_clean_year_2020-21` == sch_name,1, 0, missing = 0),
    in_sch_21_22 = if_else(`school_name_clean_year_2021-22` == sch_name,1, 0, missing = 0),
    in_sch_22_23 = if_else(`school_name_clean_year_2022-23` == sch_name,1, 0, missing = 0),
    in_sch_23_24 = if_else(`school_name_clean_year_2023-24` == sch_name,1, 0, missing = 0),
    job_type_18_19 = `job_type_year_2018-19`,
    job_type_19_20 = `job_type_year_2019-20`,
    job_type_20_21 = `job_type_year_2020-21`,
    job_type_21_22 = `job_type_year_2021-22`,
    job_type_22_23 = `job_type_year_2022-23`,
    job_type_23_24 = `job_type_year_2023-24`,
  )
  
  return(df_update)
}

#add rent type 
add_rent_type <- function(df, year1, year2) {
  
  df_update<-df %>%
    mutate(
      !!str_c("rent_type_", year2) := case_when(
        # new teachers
        #missing to teacher
        get(paste0("in_sch_", year1)) == 0 &
          get(paste0("in_sch_", year2)) == 1 &
          (get(paste0("job_type_", year2)) %in%
             c("teacher (in)","teacher (out)")) ~ "new",
        
        #person moves from staff to teacher in current school
        get(paste0("in_sch_", year1)) == 1 &
          (get(paste0("job_type_", year1)) %in%
             c("staff")) &
          get(paste0("in_sch_", year2)) == 1 &
          (get(paste0("job_type_", year2)) %in%
             c("teacher (in)","teacher (out)")) ~ "new",
        
        # leavers
        #person leaves as a teacher to another school (regardless of position)
        get(paste0("in_sch_", year1)) == 1 &
          (get(paste0("job_type_", year1)) %in%
             c("teacher (in)", "teacher (out)")) &
          get(paste0("in_sch_", year2)) == 0 ~ "leaver",
        
        # stayer
        #stayed as a teacher in the classroom
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) == "teacher (in)" &
             get(paste0("job_type_", year2)) == "teacher (in)") ~ "stayer",
        
        #stayed as a teacher outside the classroom
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) == "teacher (out)" &
             get(paste0("job_type_", year2)) == "teacher (out)") ~ "stayer",
        
        # shifter
        #person moves from admin to teacher in current school
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) %in%
             c("admin")) &
          (get(paste0("job_type_", year2)) %in%
             c("teacher (in)", "teacher (out)")) ~ "shifter",  
        
        #shifted from teacher to Admin/Staff  
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) %in%
             c("teacher (in)", "teacher (out)")) &
          (get(paste0("job_type_", year2)) %in%
             c("staff", "admin")) ~ "shifter",
        
        #shifted from teacher (in) to teacher (out)  
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) %in% c("teacher (in)")) &
          (get(paste0("job_type_", year2)) %in% c("teacher (out)")) ~ "shifter",
        
        #shifted from teacher (out) to teacher (in)  
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year1)) %in% c("teacher (out)")) &
          (get(paste0("job_type_", year2)) %in% c("teacher (in)")) ~ "shifter",     
        
        # other - NA
        (get(paste0("in_sch_", year1)) == 1 &
           get(paste0("in_sch_", year2)) == 1) &
          (get(paste0("job_type_", year2)) == "staff" | get(paste0("job_type_", year2)) == "admin") &
          (get(paste0("job_type_", year1)) == "staff" | get(paste0("job_type_", year1)) == "admin") ~ NA
      )
    )
  return(df_update)
}

#Update Rent Variables
update_rent_variables<-function(df, sch_name){
  
  df_update<-add_variables(df, sch_name)
  df_update<-add_rent_type(df_update, "18_19", "19_20")
  df_update<-add_rent_type(df_update, "19_20", "20_21")
  df_update<-add_rent_type(df_update, "20_21", "21_22")
  df_update<-add_rent_type(df_update, "21_22", "22_23")
  df_update<-add_rent_type(df_update, "22_23", "23_24")
  
  return(df_update)
}

#Update Rent Variables
update_rent_variables2<-function(df, sch_name){
  
  df_update<-update_rent_variables(df, sch_name) %>% safe_function_desc_tbl()
  
  return(df_update)
}




## -----------------------------------------------------------------------------
## Part 4 - Add Retention Type Variables
## -----------------------------------------------------------------------------

hr_by_sch<-map2(hr_by_sch,names(hr_by_sch),
               function(x,y){update_rent_variables(x,y)})

#toc version
toc_hr_by_sch_list[["overall"]]<-map2(toc_hr_by_sch_list[["overall"]],
                                      names(hr_by_sch),
                function(x,y){update_rent_variables(x,y)})


for (teach_year in teach_string){
  for (sch_year in sch_year_string){
    toc_hr_by_sch_list[[teach_year]][[sch_year]]<-
      map2(toc_hr_by_sch_list[[teach_year]][[sch_year]],
           names(hr_by_sch),
           function(x,y){update_rent_variables2(x,y)})
  }
}

# toc_hr_by_sch_list[["0-5 years"]][["19_20"]]<-
#   map2(toc_hr_by_sch_list[["0-5 years"]][["19_20"]],
#        names(hr_by_sch),
#        function(x,y){update_rent_variables2(x,y)})

## -----------------------------------------------------------------------------
## Part 5 - Save Data
## -----------------------------------------------------------------------------

save(hr_by_sch,toc_hr_by_sch_list,hr_color_list,
     file = file.path(code_file_dir,'hr_by_sch.RData'))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------