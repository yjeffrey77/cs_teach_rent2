################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 02_clean_data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/4/24 >
##
################################################################################

#Goal: Store functions of 05_clean_young_teachers.R

################################################################################

## ---------------------------
## libraries
## ---------------------------


## ---------------------------
## directory paths
## ---------------------------

#see current directory



## ---------------------------
## file directories
## ---------------------------


## ---------------------------
## helper functions
## ---------------------------

## ---------------------------
## load & inspect data
## ---------------------------

## -----------------------------------------------------------------------------
## Part 1 - Filter Functions
## -----------------------------------------------------------------------------

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

filter_age_years_v2<-function(df, years){
  
  df_list<-vector("list",6)
  names(df_list)<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  
  df_list[["18_19"]]<-df %>% filter(teacher_years_18_19_cv2 == years)
  df_list[["19_20"]]<-df %>% filter(teacher_years_19_20_cv2 == years)
  df_list[["20_21"]]<-df %>% filter(teacher_years_20_21_cv2 == years)
  df_list[["21_22"]]<-df %>% filter(teacher_years_21_22_cv2 == years)
  df_list[["22_23"]]<-df %>% filter(teacher_years_22_23_cv2 == years)
  df_list[["23_24"]]<-df %>% filter(teacher_years_23_24_cv2 == years)
  
  return(df_list)
}

filter_age_years_t<-function(df, years){
  
  df_list<-vector("list",6)
  names(df_list)<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  
  df_list[["18_19"]]<-df %>% filter(teacher_years_18_19_t == years)
  df_list[["19_20"]]<-df %>% filter(teacher_years_19_20_t == years)
  df_list[["20_21"]]<-df %>% filter(teacher_years_20_21_t == years)
  df_list[["21_22"]]<-df %>% filter(teacher_years_21_22_t == years)
  df_list[["22_23"]]<-df %>% filter(teacher_years_22_23_t == years)
  df_list[["23_24"]]<-df %>% filter(teacher_years_23_24_t == years)
  
  return(df_list)
}

filter_age_years_tv2<-function(df, years){
  
  df_list<-vector("list",6)
  names(df_list)<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  
  df_list[["18_19"]]<-df %>% filter(teacher_years_18_19_tv2 == years)
  df_list[["19_20"]]<-df %>% filter(teacher_years_19_20_tv2 == years)
  df_list[["20_21"]]<-df %>% filter(teacher_years_20_21_tv2 == years)
  df_list[["21_22"]]<-df %>% filter(teacher_years_21_22_tv2 == years)
  df_list[["22_23"]]<-df %>% filter(teacher_years_22_23_tv2 == years)
  df_list[["23_24"]]<-df %>% filter(teacher_years_23_24_tv2 == years)
  
  return(df_list)
}

## -----------------------------------------------------------------------------
## Part 2 - Subset sample functions
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

#subset by school

#school_names_string2<-school_names_tbl_update$school_name_clean %>% unique()

#Create list (TOC and by age)

create_sch_list<-function(df){
  
  school_names_string2<-school_names_tbl_update$school_name_clean %>% unique()
  
  toc_hr_by_sch_list<-vector('list', length(df))
  names(toc_hr_by_sch_list)<-names(df)
  
  for (a in names(toc_hr_by_sch_list)){
    
    if (a == "overall"){
      
      toc_hr_by_sch_list[["overall"]]<-map(school_names_string2,
                                           function(x){filter_school(hr_color_list[["overall"]],x)})
      
      names(toc_hr_by_sch_list[["overall"]])<-school_names_string2
      
    }
    else{
      toc_hr_by_sch_list[[a]]<-map(hr_color_list[[a]],
                                   function(y){
                                     map(school_names_string2,
                                         function(x){filter_school(y,x)})})
    }
  }
  
  #update names
  
  sch_year_string<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  teach_string<-c("0-5 years","6-10 years","11-15 years","15+ years")
  
  for (teach_year in teach_string){
    for (sch_year in sch_year_string){
      names(toc_hr_by_sch_list[[teach_year]][[sch_year]])<-school_names_string2
    }
  }
  
  return(toc_hr_by_sch_list)
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
      
      # # Add a row of NA values
      # vec <- colnames(toc_hr_by_sch_list[["overall"]][["107th Street Elementary"]])
      # # Transform the vector into a data frame with each element as a column
      # df <- as.data.frame(t(vec))
      # 
      # colnames(df)<-colnames(toc_hr_by_sch_list[["overall"]][["107th Street Elementary"]])
      # 
      # # Add a row of NA values
      # df[nrow(df),] <- NA
      
      df<-NA
      
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



#Create list (TOC and by age)
create_sch_list<-function(df){
  
  school_names_string2<-school_names_tbl_update$school_name_clean %>% unique()
  
  toc_hr_by_sch_list<-vector('list', length(df))
  names(toc_hr_by_sch_list)<-names(df)
  
  
  df_list<-map(df,
               function(y){map(school_names_string2,
                               function(x){filter_school(y,x)})})
  
  sch_year_string<-c("18_19", "19_20", "20_21", "21_22", "22_23","23_24")
  teach_string<-c("0-5 years","6-10 years","11-15 years","15+ years")
  
  
  for (sch_year in sch_year_string){
    names(df_list[[sch_year]])<-school_names_string2
  }
  
  return(df_list)
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
      vec <- colnames(hr_color_by_school[["original_4"]][["0-5 years"]][["18_19"]][["107th Street Elementary"]])
      # Transform the vector into a data frame with each element as a column
      df <- as.data.frame(t(vec))
      
      colnames(df)<-colnames(hr_color_by_school[["original_4"]][["0-5 years"]][["18_19"]][["107th Street Elementary"]])
      
      # Add a row of NA values
      df[nrow(df),] <- NA
      
      return(df)
      
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
      
      # # Add a row of NA values
      # vec <- colnames(hr_color_by_school[["original_4"]][["0-5 years"]][["18_19"]][["107th Street Elementary"]])
      # # Transform the vector into a data frame with each element as a column
      # df <- as.data.frame(t(vec))
      # 
      # colnames(df)<-colnames(hr_color_by_school[["original_4"]][["0-5 years"]][["18_19"]][["107th Street Elementary"]])
      # 
      # # Add a row of NA values
      # df[nrow(df),] <- NA
      
      return(NA)
      
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

# hr_by_sch<-map2(hr_by_sch,names(hr_by_sch),
#                function(x,y){update_rent_variables(x,y)})
# 
# #toc version
# toc_hr_by_sch_list[["overall"]]<-map2(toc_hr_by_sch_list[["overall"]],
#                                       names(hr_by_sch),
#                 function(x,y){update_rent_variables(x,y)})
# 
# 
# for (teach_year in teach_string){
#   for (sch_year in sch_year_string){
#     toc_hr_by_sch_list[[teach_year]][[sch_year]]<-
#       map2(toc_hr_by_sch_list[[teach_year]][[sch_year]],
#            names(hr_by_sch),
#            function(x,y){update_rent_variables2(x,y)})
#   }
# }

# toc_hr_by_sch_list[["0-5 years"]][["19_20"]]<-
#   map2(toc_hr_by_sch_list[["0-5 years"]][["19_20"]],
#        names(hr_by_sch),
#        function(x,y){update_rent_variables2(x,y)})

## -----------------------------------------------------------------------------
## Part 5 - Save Data
## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------