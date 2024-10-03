







test<-create_neighborhood_rent_tbls(rent_by_neighborhood[["south_mid_city"]],
                                    neighborhood_string_list[["south_mid_city"]])

test<-
  
  
  
  
  #neighborhood
  #create a list with 4 things
  
  #Overall, CS in the Neigborhood, TS in the Neighborhood, Combined Overall Table
  
  
  #THING TO DO - MAKE THEM ALL INTO LISTS








# Abraham Lincoln Senior High
# Arleta Senior High
# Cal Burke High School
# George Washington Carver Middle School
# Glenn Hammond Curtiss Middle School
# Harold McAlister High School CYESIS
# Los Angeles Academy Middle School
# Mark Twain Middle School
# Theodore Roosevelt Senior High
# Miguel Contreras Learning Complex - Business and Tourism - HS
# Miguel Contreras Learning Complex - School of Social Justice - HS
# Orchard Academies 2B - MS
# Orchard Academies 2C - MS







ts_rent_elem<-ts_rent_elem


ts_rent_elem<-ts_rent_tbls %>%
  filter(grepl("Elementary", ts_rent_tbls$school))









#MS/HS

# Abraham Lincoln Senior High
# Arleta Senior High
# Cal Burke High School
# George Washington Carver Middle School
# Glenn Hammond Curtiss Middle School
# Harold McAlister High School CYESIS
# Los Angeles Academy Middle School
# Mark Twain Middle School
# Theodore Roosevelt Senior High
# Miguel Contreras Learning Complex - Business and Tourism - HS
# Miguel Contreras Learning Complex - School of Social Justice - HS
# Orchard Academies 2B - MS
# Orchard Academies 2C - MS


#Span/Others
# Elizabeth Learning Center - Span (UTK - 8)
# Judith F Baca Arts Academy - Span (UTK - 6)

#Elementary
# Carmen Lomas Garza Primary Center - Elem
# Caroldale Learning Community - Elem
# Rosa Parks Learning Center - elem
# Young Empowered Scholars Academy - elem

# -   Retention Rates of CS vs. TS overall
# 
# -   Retention Rates of CS vs. TS elementary Schools
# 
# -   Retention Rates of CS vs. TS middle/high schools
# 
# -   Retention Rates of CS vs. TS among novice teachers
# 
# -   Retention Rates of CS vs. TS among veteran teachers
# 
# -   Retention Rates of CS vs. TS by neighborhood



cs_rent_tbls<-rent_tbls %>% keep(names(rent_tbls) %in% cs_string)

cs_rent_tbls<-map2(cs_rent_tbls, names(cs_rent_tbls),
                   function(x,y){add_names_to_tables(x,y)})

#Create Overall Retention Rate
cs_rent_overall<-bind_rows(cs_rent_tbls) %>% filter(rent == "Retention Rate")

cs_pivot_rent<-cs_rent_overall %>%
  summarize(
    yr_2019 = sum(yr_2019, na.rm = T)/n() %>% round(),
    yr_2020 = sum(yr_2020, na.rm = T)/n() %>% round(),
    yr_2021 = sum(yr_2021, na.rm = T)/n() %>% round(),
    yr_2022 = sum(yr_2022, na.rm = T)/n() %>% round(),
    yr_2023 = sum(yr_2023, na.rm = T)/n() %>% round(),
  )

cs_pivot_rent$school<- "Overall Retention Rate"
cs_pivot_rent$school<- "Overall Retention Rate"
cs_pivot_rent$rent<- "Overall Retention Rate"

cs_pivot_rent<-cs_pivot_rent %>% select(school, rent, everything())


cs_rent_overall<-rbind(cs_rent_overall, cs_pivot_rent)

#round rental numbers to whole numbers

cs_rent_overall<-cs_rent_overall %>% 
  mutate(yr_2019 = round(yr_2019),
         yr_2020 = round(yr_2020),
         yr_2021 = round(yr_2021),
         yr_2022 = round(yr_2022),
         yr_2023 = round(yr_2023))

## -----------------------------------------------------------------------------
## Part 3 - Create Retention Tables - Comparison Schools
## -----------------------------------------------------------------------------



#Create Overall Retention Rate



## -----------------------------------------------------------------------------
## Part 1 - Retention Rates
## -----------------------------------------------------------------------------

test<-hr_by_sch[["107th Street Elementary"]]


data19<-test %>% group_by(rent_type_19_20) %>% summarize(count19 = n())
data20<-test %>% group_by(rent_type_20_21) %>% summarize(count20 = n())
data21<-test %>% group_by(rent_type_21_22) %>% summarize(count21 = n())
data22<-test %>% group_by(rent_type_22_23) %>% summarize(count22 = n())
data23<-test %>% group_by(rent_type_23_24) %>% summarize(count23 = n())
rent_tbl<-data.frame(rent = c("leaver", "new", "shifter", "stayer", NA))


rent_data<-left_join(rent_tbl,data19, by = c("rent" = "rent_type_19_20"))
rent_data<-left_join(rent_data,data20, by = c("rent" = "rent_type_20_21"))
rent_data<-left_join(rent_data,data21, by = c("rent" = "rent_type_21_22"))
rent_data<-left_join(rent_data,data22, by = c("rent" = "rent_type_22_23"))
rent_data<-left_join(rent_data,data23, by = c("rent" = "rent_type_23_24"))

colnames(rent_data)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")


total_teachers <- data.frame(rent = "Total",
                             `2019` = sum(rent_data[1,2], rent_data[3,2], rent_data[4,2], na.rm = T),
                             `2020` = sum(rent_data[1,3], rent_data[3,3], rent_data[4,3], na.rm = T),
                             `2021` = sum(rent_data[1,4], rent_data[3,4], rent_data[4,4], na.rm = T),
                             `2022` = sum(rent_data[1,5], rent_data[3,5], rent_data[4,5], na.rm = T),
                             `2023` = sum(rent_data[1,6], rent_data[3,6], rent_data[4,6], na.rm = T))
colnames(total_teachers)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")

rent_data<-rbind(rent_data,total_teachers)

rent_results<-data.frame(rent = "Retention Rate",
                         `2019` = ((sum(rent_data[3,2], rent_data[4,2], na.rm = T)/rent_data[6,2])*100) %>% round(),
                         `2020` = ((sum(rent_data[3,3], rent_data[4,3], na.rm = T)/rent_data[6,3])*100) %>% round(),
                         `2021` = ((sum(rent_data[3,4], rent_data[4,4], na.rm = T)/rent_data[6,4])*100) %>% round(),
                         `2022` = ((sum(rent_data[3,5], rent_data[4,5], na.rm = T)/rent_data[6,5])*100) %>% round(),
                         `2023` = ((sum(rent_data[3,6], rent_data[4,6], na.rm = T)/rent_data[6,6])*100) %>% round())
colnames(rent_results)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")

rent_data<-rbind(rent_data,rent_results)

#filter out rows
rent_data<-rent_data %>% filter(!is.na(rent))









test %>% filter(in_sch_18_19 == 1 & 
                  (job_type_18_19 %in% c("teacher (in)", "teacher (out)"))) %>%
  summarize(n_teachers = n())





#total teachers
teach18<-test %>% 
  filter(in_sch_18_19 == 1 & 
           (job_type_18_19 %in%
              c("teacher (in)", "teacher (out)"))) %>%
  summarize(n_teachers = n())

teach18<-test %>% 
  filter(in_sch_18_19 == 1 & 
           (job_type_18_19 %in%
              c("teacher (in)", "teacher (out)"))) %>%
  summarize(n_teachers = n())

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

#create list
hr_by_sch<-vector("list", length(school_names_string2))
names(hr_by_sch)<-school_names_string2

hr_by_sch<-map(school_names_string2,
               function(x){filter_school(hr_data_wide,x)})

names(hr_by_sch)<-school_names_string2

## -----------------------------------------------------------------------------
## Part 3 - Create Rent Type Functions
## -----------------------------------------------------------------------------

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

#add rent type (Will Need to Modify)
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

## -----------------------------------------------------------------------------
## Part 4 - Add Retention Type Variables
## -----------------------------------------------------------------------------

hr_by_sch<-map2(hr_by_sch,names(hr_by_sch),
                function(x,y){update_rent_variables(x,y)})

## -----------------------------------------------------------------------------
## Part 5 - Save Data
## -----------------------------------------------------------------------------

save(hr_by_sch,
     file = file.path(code_file_dir,'hr_by_sch.RData'))