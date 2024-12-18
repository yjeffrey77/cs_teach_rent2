################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 06.1_run_data_young_teachers.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/26/24 >
##
################################################################################

#Goal: Runs Analyses and Stores them in Pivot Tables

#Note this version only considers schools when they are designated as a 
#community school.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)
library(purrr)

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

#error function
safe_function_df<-function(expr, version){
  result<- tryCatch(
    {
      expr
      },
    error = function(e){
      
      return(data.frame(school = NA,rent = "Retention Rate",
                        yr_2019 = NA,yr_2020 = NA,
                        yr_2021 = NA,yr_2022 = NA,yr_2023 = NA))
      
    }
    
  )
  return(result)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "hr_by_sch.RDATA"))

load(file.path(code_file_dir, "hr_color_by_sch.RData"))

## -----------------------------------------------------------------------------
## Part 1.1 - Retention Rate Function
## -----------------------------------------------------------------------------

create_rent_tbl<-function(df){
  
  test<-df
  
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
                               yr_2019 = sum(rent_data[1,2], rent_data[3,2], rent_data[4,2], na.rm = T),
                               yr_2020 = sum(rent_data[1,3], rent_data[3,3], rent_data[4,3], na.rm = T),
                               yr_2021 = sum(rent_data[1,4], rent_data[3,4], rent_data[4,4], na.rm = T),
                               yr_2022 = sum(rent_data[1,5], rent_data[3,5], rent_data[4,5], na.rm = T),
                               yr_2023 = sum(rent_data[1,6], rent_data[3,6], rent_data[4,6], na.rm = T))
  colnames(total_teachers)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  rent_data<-rbind(rent_data,total_teachers)
  
  rent_results<-data.frame(rent = "Retention Rate %",
                           yr_2019 = ((sum(rent_data[3,2], rent_data[4,2], na.rm = T)/rent_data[6,2])*100) %>% round(),
                           yr_2020 = ((sum(rent_data[3,3], rent_data[4,3], na.rm = T)/rent_data[6,3])*100) %>% round(),
                           yr_2021 = ((sum(rent_data[3,4], rent_data[4,4], na.rm = T)/rent_data[6,4])*100) %>% round(),
                           yr_2022 = ((sum(rent_data[3,5], rent_data[4,5], na.rm = T)/rent_data[6,5])*100) %>% round(),
                           yr_2023 = ((sum(rent_data[3,6], rent_data[4,6], na.rm = T)/rent_data[6,6])*100) %>% round())
  colnames(rent_results)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  rent_data<-rbind(rent_data,rent_results)
  
  #filter out rows
  rent_data<-rent_data %>% filter(!is.na(rent))
  
  #add descriptive version
  rent_results2<-data.frame(rent = "Retention Rate % (n)",
                            yr_2019 = str_c(rent_data[6,2],"% (",rent_data[5,2],")"),
                            yr_2020 = str_c(rent_data[6,3],"% (",rent_data[5,3],")"),
                            yr_2021 = str_c(rent_data[6,4],"% (",rent_data[5,4],")"),
                            yr_2022 = str_c(rent_data[6,5],"% (",rent_data[5,5],")"),
                            yr_2023 = str_c(rent_data[6,6],"% (",rent_data[5,6],")"))
  
  colnames(rent_results2)<-c("rent", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  
  rent_data<-rbind(rent_data,rent_results2)
  
  return(rent_data)
}

#still need to update
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
## Part 1.2 - Update the datasets based by cohorts.
## -----------------------------------------------------------------------------

source(file.path(code_file_dir, "00_school_lists.R"))

#split by cohort

#overall
create_cohort_list<-function(df_list, sch_string){
  
  df_list_update<-df_list[sch_string]
  df_list_update <- Filter(Negate(is.null), df_list_update)
  return(df_list_update)
}

create_full_cohort_list<-function(sch_string){
  
  overall_list<-vector("list", 1)
  names(overall_list)<-c("overall")
  
  overall_list[["overall"]]<-create_cohort_list(toc_hr_by_sch_list[["overall"]],
                                                sch_string)
  
  veteran_list<-map(hr_color_by_sch[["original_5"]],
             function(veteran_status){
               map(veteran_status,
                   function(x){
                     create_cohort_list(x,sch_string)
                   })
             })
  
  cohort_list<-c(overall_list, veteran_list)
  
  return(cohort_list)
}

#Create cohort 3 string
cs_cohort_3_string<-setdiff(cs_cohort2_3_string, cs_cohort2_string)

#cohorts
cohort_1_list<-create_full_cohort_list(cs_cohort1_string)
cohort_2_3_list<-create_full_cohort_list(cs_cohort2_3_string)
cohort_2_list<-create_full_cohort_list(cs_cohort2_string)
cohort_3_list<-create_full_cohort_list(cs_cohort_3_string)

#clean datasets for each cohort

#cohort 1 - started in 2019-20 (don't need to touch)

#cohort 2 - started in 2020-21
overall_list<-vector("list", 1)
names(overall_list)<-c("overall")

overall_list[["overall"]]<-map(cohort_2_list[["overall"]],
          function(df){
            df_update<-df %>%  mutate(
              rent_type_19_20 = NA)
            return(df_update)})
  
veteran_list<-map(cohort_2_list[2:6],
           function(veteran_status){
             map(veteran_status,
                 function(df_year){
                   map(df_year,
                       function(df){
                         if (is.na(df)== TRUE){
                           
                           df_update<-NA
                         }
                         if (is.na(df) == FALSE){
                           df_update<-df %>% mutate(rent_type_19_20 = NA)
                         }
                         return(df_update)
                       })})})
  
cohort_2_list<-c(overall_list, veteran_list)

#cohort 3 - started in 2021-22

overall_list<-vector("list", 1)
names(overall_list)<-c("overall")

overall_list[["overall"]]<-map(cohort_3_list[["overall"]],
                               function(df){
                                 df_update<-df %>%  mutate(
                                   rent_type_19_20 = NA,
                                   rent_type_20_21 = NA)
                                 return(df_update)})

veteran_list<-map(cohort_3_list[2:6],
                  function(veteran_status){
                    map(veteran_status,
                        function(df_year){
                          map(df_year,
                              function(df){
                                if (is.na(df)== TRUE){
                                  
                                  df_update<-NA
                                }
                                if (is.na(df) == FALSE){
                                  df_update<-df %>%
                                    mutate(
                                      rent_type_19_20 = NA,
                                      rent_type_20_21 = NA,
                                      )
                                }
                                return(df_update)
                              })})})

cohort_3_list<-c(overall_list, veteran_list)

#after having all the cohorts then, combine them all together

combined_cohort_overall<-vector("list", 1)
names(combined_cohort_overall)<-"overall"

combined_cohort_overall[["overall"]]<-c(cohort_1_list[["overall"]],
                                cohort_2_list[["overall"]],
                                cohort_3_list[["overall"]])

combine_list<-function(veteran_status,cohort_string){
  
  df_list<-c(cohort_1_list[[veteran_status]][[cohort_string]],
    cohort_2_list[[veteran_status]][[cohort_string]],
    cohort_3_list[[veteran_status]][[cohort_string]])
  
  return(df_list)
}

combine_full_list<-function(veteran_status){

  df_yr_list<-c("18_19", "19_20", "20_21", "21_22", "22_23", "23_24")
    
  df_list<-map(df_yr_list,
               function(z){
                 combine_list(veteran_status, z)
               })
  
  names(df_list)<-df_yr_list
  return(df_list)
  
}

veteran_status_string<-c("0-3 years","4-5 years",
                         "6-10 years","11-15 years", "15+ years")

combined_veteran_list<-map(veteran_status_string,combine_full_list)
names(combined_veteran_list)<-veteran_status_string
  
combined_cohort<-c(combined_cohort_overall, combined_veteran_list)

rm(combined_cohort_overall, combined_veteran_list)

## -----------------------------------------------------------------------------
## Part 2.1 - Create Retention Tables - Preparation & Functions
## -----------------------------------------------------------------------------

#create rent tables

create_rent_tbl_list<-function(df_list){
  overall<-map(df_list[["overall"]],create_rent_tbl)
  veteran_list<-map(df_list[2:6],function(z){
    map(z,
        function(x){
          map(x, function(y){
            if (is.na(y) == TRUE){
              
              update_df<-data.frame(
                rent = NA,yr_2019 = NA,yr_2020 = NA,
                yr_2021 = NA, yr_2022 = NA, yr_2023 = NA)
            }
            
            if (is.na(y) == FALSE){
              update_df<-create_rent_tbl(y)
            }   
            return(update_df) 
          })})
  })
  df_list_update<-c(list(overall), veteran_list)
  names(df_list_update)[1]<-"overall"
  
  rm(overall, veteran_list) 
  
  return(df_list_update)
}

#combined cohort
combined_cohort_rent_tbl<-create_rent_tbl_list(combined_cohort)

#cohort 1
cohort_1_rent_tbl<-create_rent_tbl_list(cohort_1_list)

#cohort 2
cohort_2_rent_tbl<-create_rent_tbl_list(cohort_2_list)

#cohort 3
cohort_3_rent_tbl<-create_rent_tbl_list(cohort_3_list)

#4 hours

## -----------------------------------------------------------------------------
## Part 2.2 - Create Retention Tables - Create Teach Experience Functions
## -----------------------------------------------------------------------------

#Create teach_year_string2
teach_year_string2<-names(hr_color_by_sch[["original_5"]])

#Create Teacher Experience Function

create_teach_exp_rent_tbl<-function(df, teach_year, sch_name){
  
  test<-df[[teach_year]][["18_19"]][[sch_name]] %>%
    select(rent, yr_2019)
  
  test2<-df[[teach_year]][["19_20"]][[sch_name]] %>%
    select(rent, yr_2020)

  test3<-df[[teach_year]][["20_21"]][[sch_name]] %>%
    select(rent, yr_2021)

  test4<-df[[teach_year]][["21_22"]][[sch_name]] %>%
    select(rent, yr_2022)

  test5<-df[[teach_year]][["22_23"]][[sch_name]] %>%
    select(rent, yr_2023)

  merge_data<-test %>% left_join(test2, by = "rent") %>%
    left_join(test3, by = "rent") %>%
    left_join(test4, by = "rent") %>%
    left_join(test5, by = "rent")
  
  return(merge_data)
}

#Create List to store retention tables

cohort_teach_exp_rent_tbls<-vector("list", 3)
names(cohort_teach_exp_rent_tbls)<-c("cohort_1","cohort_2","cohort_3")

cohort_teach_exp_rent_tbls[["cohort_1"]]<-
  map(teach_year_string2, function(teach_years){
    
    df_list<-map(names(cohort_1_rent_tbl[["0-3 years"]][["18_19"]]),
        function(a){
          create_teach_exp_rent_tbl(cohort_1_rent_tbl,
                                    teach_years,a)})
    names(df_list)<-names(cohort_1_rent_tbl[["0-3 years"]][["18_19"]])
    return(df_list)}
      )
names(cohort_teach_exp_rent_tbls[["cohort_1"]])<- teach_year_string2 

cohort_teach_exp_rent_tbls[["cohort_2"]]<-
  map(teach_year_string2, function(teach_years){
    
    df_list<-map(names(cohort_2_rent_tbl[["0-3 years"]][["18_19"]]),
                 function(a){
                   create_teach_exp_rent_tbl(cohort_2_rent_tbl,
                                             teach_years,a)})
    names(df_list)<-names(cohort_2_rent_tbl[["0-3 years"]][["18_19"]])
    return(df_list)}
  )
names(cohort_teach_exp_rent_tbls[["cohort_2"]])<- teach_year_string2 


cohort_teach_exp_rent_tbls[["cohort_3"]]<-
  map(teach_year_string2, function(teach_years){
    
    df_list<-map(names(cohort_3_rent_tbl[["0-3 years"]][["18_19"]]),
                 function(a){
                   create_teach_exp_rent_tbl(cohort_3_rent_tbl,
                                             teach_years,a)})
    names(df_list)<-names(cohort_3_rent_tbl[["0-3 years"]][["18_19"]])
    return(df_list)}
  )
names(cohort_teach_exp_rent_tbls[["cohort_3"]])<- teach_year_string2 

## -----------------------------------------------------------------------------
## Part 2.3 - Add Function to Build Overall Tables
## -----------------------------------------------------------------------------

add_names_to_tables<-function(df, sch_name){
  
  df_update<-df %>% mutate(school = sch_name)
  df_update<-df_update %>% select(school, everything())
  
  return(df_update)
}

create_overall_rent_tbl<-function(df, school_string, tbl_type){
  
  df_update<-df %>% keep(names(df) %in% school_string)
  
  df_update<-map2(df_update, names(df_update),
                     function(x,y){add_names_to_tables(x,y)})
  
  #Create Overall Retention Rate
  
  
  rent_overall<-bind_rows(df_update) %>%
    filter(rent == "Retention Rate %")
  
  rent_overall<-rent_overall %>%
    mutate(
      yr_2019 = as.numeric(yr_2019),
      yr_2020 = as.numeric(yr_2020),
      yr_2021 = as.numeric(yr_2021),
      yr_2022 = as.numeric(yr_2022),
      yr_2023 = as.numeric(yr_2023),
    )
  
  pivot_rent<-rent_overall %>%
    summarize(
      yr_2019 = sum(yr_2019, na.rm = T)/n() %>% round(),
      yr_2020 = sum(yr_2020, na.rm = T)/n() %>% round(),
      yr_2021 = sum(yr_2021, na.rm = T)/n() %>% round(),
      yr_2022 = sum(yr_2022, na.rm = T)/n() %>% round(),
      yr_2023 = sum(yr_2023, na.rm = T)/n() %>% round(),
    )
  
  pivot_rent$school<- "Overall Retention Rate %"
  
  pivot_rent$rent<- "Overall Retention Rate %"
  
  pivot_rent<-pivot_rent %>% select(school, rent, everything())
  
  if (tbl_type == "Retention Rate %"){
  rent_overall<-rbind(rent_overall, pivot_rent)
  
  #round rental numbers to whole numbers
  
  rent_overall<-rent_overall %>% 
    mutate(yr_2019 = round(yr_2019),
           yr_2020 = round(yr_2020),
           yr_2021 = round(yr_2021),
           yr_2022 = round(yr_2022),
           yr_2023 = round(yr_2023))
  }
  
  if (tbl_type == "Retention Rate % (n)"){
  
    rent_overall<-bind_rows(df_update) %>%
      filter(rent == "Retention Rate % (n)")
    
    pivot_rent<-pivot_rent %>% mutate(
      yr_2019 = str_c(as.character(yr_2019 %>% round()), "%"),
      yr_2020 = str_c(as.character(yr_2020 %>% round()), "%"),
      yr_2021 = str_c(as.character(yr_2021 %>% round()), "%"),
      yr_2022 = str_c(as.character(yr_2022 %>% round()), "%"),
      yr_2023 = str_c(as.character(yr_2023 %>% round()), "%"))
    
    rent_overall<-rbind(rent_overall, pivot_rent)
  }
  
  return(rent_overall)
}

create_overall_rent_tbl2<-function(df, school_string, tbl_type){
  
  df_update<-df %>% keep(names(df) %in% school_string)
  
  df_update<-map2(df_update, names(df_update),
                  function(x,y){add_names_to_tables(x,y)})
  
  #Create Overall Retention Rate
  
  rent_overall<-bind_rows(df_update) %>%
    filter(rent == "Retention Rate %")
  
  rent_overall<-rent_overall %>%
    mutate(
      yr_2019 = as.numeric(yr_2019),
      yr_2020 = as.numeric(yr_2020),
      yr_2021 = as.numeric(yr_2021),
      yr_2022 = as.numeric(yr_2022),
      yr_2023 = as.numeric(yr_2023),
    )
  
  #Make sure all missing values are consistent
  # rent_overall<-rent_overall %>% mutate(
  #   yr_2019 = case_when(yr_2019 == NaN ~ NA)
  # )
  
  pivot_rent<-rent_overall %>%
    summarize(
      yr_2019 = sum(yr_2019, na.rm = T)/sum(!is.na(yr_2019)) %>% round(),
      yr_2020 = sum(yr_2020, na.rm = T)/sum(!is.na(yr_2020)) %>% round(),
      yr_2021 = sum(yr_2021, na.rm = T)/sum(!is.na(yr_2021)) %>% round(),
      yr_2022 = sum(yr_2022, na.rm = T)/sum(!is.na(yr_2022)) %>% round(),
      yr_2023 = sum(yr_2023, na.rm = T)/sum(!is.na(yr_2023)) %>% round(),
    )
  
  pivot_rent$school<- "Overall Retention Rate %"
  
  pivot_rent$rent<- "Overall Retention Rate %"
  
  pivot_rent<-pivot_rent %>% select(school, rent, everything())
  
  if (tbl_type == "Retention Rate %"){
    rent_overall<-rbind(rent_overall, pivot_rent)
    
    #round rental numbers to whole numbers
    
    rent_overall<-rent_overall %>% 
      mutate(yr_2019 = round(yr_2019),
             yr_2020 = round(yr_2020),
             yr_2021 = round(yr_2021),
             yr_2022 = round(yr_2022),
             yr_2023 = round(yr_2023))
  }
  
  if (tbl_type == "Retention Rate % (n)"){
    
    rent_overall<-bind_rows(df_update) %>%
      filter(rent == "Retention Rate % (n)")
    
    pivot_rent<-pivot_rent %>% mutate(
      yr_2019 = str_c(as.character(yr_2019 %>% round()), "%"),
      yr_2020 = str_c(as.character(yr_2020 %>% round()), "%"),
      yr_2021 = str_c(as.character(yr_2021 %>% round()), "%"),
      yr_2022 = str_c(as.character(yr_2022 %>% round()), "%"),
      yr_2023 = str_c(as.character(yr_2023 %>% round()), "%"))
    
    rent_overall<-rbind(rent_overall, pivot_rent)
  }
  
  return(rent_overall)
}

## -----------------------------------------------------------------------------
## Part 2.2 - Create Retention Tables - Overall
## -----------------------------------------------------------------------------

#all teachers
#all cohorts at once
combined_cohorts<-vector("list", 6)
names(combined_cohorts)<-names(cohort_1_rent_tbl)

combined_cohorts[["overall"]]<-c(cohort_1_rent_tbl[["overall"]],
           cohort_2_rent_tbl[["overall"]],cohort_3_rent_tbl[["overall"]])

for(a in names(cohort_1_rent_tbl)[2:length(names(cohort_1_rent_tbl))]){
  
  combined_cohorts[[a]]<-c(cohort_teach_exp_rent_tbls[["cohort_1"]][[a]],
                           cohort_teach_exp_rent_tbls[["cohort_2"]][[a]],
                           cohort_teach_exp_rent_tbls[["cohort_3"]][[a]])
}

#create overall
combined_overall_rent_tbls<-
  map(combined_cohorts, function(teach_year){
    create_overall_rent_tbl2(teach_year,
                             cs_string, "Retention Rate %")
  })


#by cohort (Version 1)
cohort_overall_v1<-vector("list", 3)
names(cohort_overall_v1)<-str_c("cohort_",c(1:3))

cohort_overall_v1[["cohort_1"]]<-
  create_overall_rent_tbl(cohort_1_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

cohort_overall_v1[["cohort_2"]]<-
  create_overall_rent_tbl(cohort_2_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

cohort_overall_v1[["cohort_3"]]<-
  create_overall_rent_tbl(cohort_3_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

#by cohort (Version 2)
cohort_overall_v2<-vector("list", 3)
names(cohort_overall_v2)<-str_c("cohort_",c(1:3))

cohort_overall_v2[["cohort_1"]]<-
  create_overall_rent_tbl2(cohort_1_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

cohort_overall_v2[["cohort_2"]]<-
  create_overall_rent_tbl2(cohort_2_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

cohort_overall_v2[["cohort_3"]]<-
  create_overall_rent_tbl2(cohort_3_rent_tbl[["overall"]],
                          cs_string,"Retention Rate %")

#cohort 
cohort_overall_teach_exp_v1<-map(cohort_teach_exp_rent_tbls,
            function(cohort){
              map(cohort,
                  function(teach_years){
                    teach_years %>% 
                      create_overall_rent_tbl(cs_string,"Retention Rate %")
                  })})
  
cohort_overall_teach_exp_v2<-map(cohort_teach_exp_rent_tbls,
                                 function(cohort){
                                   map(cohort,
                                       function(teach_years){
                                         teach_years %>% 
                                           create_overall_rent_tbl2(cs_string,"Retention Rate % (n)")
                                       })})

#TOC - create overall list
create_overall_list<-function(df1, df2, string1, string2, tbl_type,
                              version){
  
  rent_overall<-vector("list", 2)
  names(rent_overall)<-c("cs","ts")
  
  if (version == 4){
    for(sch_type in c("cs","ts")){
      rent_overall[[sch_type]]<-vector("list", 4)
      # names(rent_overall[[sch_type]])<-c("overall", "0-5 years","6-10 years",
      #                                        "11-15 years", "15+ years")
      
      names(rent_overall[[sch_type]])<-c("0-5 years","6-10 years",
                                         "11-15 years", "15+ years")
    }
  }

  if (version == 5){
    for(sch_type in c("cs","ts")){
      rent_overall[[sch_type]]<-vector("list", 5)
      # names(rent_overall[[sch_type]])<-c("overall", "0-5 years","6-10 years",
      #                                        "11-15 years", "15+ years")
      
      names(rent_overall[[sch_type]])<-c("0-3 years","4-5 years",
                                         "6-10 years",
                                         "11-15 years", "15+ years")
    }
  }
  
  #add overall retention tables
  
  #cs
  # for (table_type in c("overall")){
  #   rent_overall[["cs"]][[table_type]]<-
  #     create_overall_rent_tbl(df1[[table_type]], string1, tbl_type)
  # }
  
  for (table_type in teach_year_string){
    rent_overall[["cs"]][[table_type]]<-
      create_overall_rent_tbl(df2[[table_type]], string1, tbl_type)
  }

  #ts
  # for (table_type in c("overall")){
  #   rent_overall[["ts"]][[table_type]]<-
  #     create_overall_rent_tbl(df1[[table_type]], string2, tbl_type)
  # }

  for (table_type in teach_year_string){
    rent_overall[["ts"]][[table_type]]<-
      create_overall_rent_tbl(df2[[table_type]], string2, tbl_type)
  }
  
  return(rent_overall)
}

#Create Function
create_overall_list2<-function(df1,  string1, string2, tbl_type,
                              version){
  
  toc_rent_overall2<-vector("list",2)
  names(toc_rent_overall2)<-c("cs","ts")
  
  toc_rent_overall2[["cs"]]<-map(df1, function(a){
    map(a,function(x) create_overall_rent_tbl2(x,string1,tbl_type))
  })
  
  toc_rent_overall2[["ts"]]<-map(df1, function(a){
    map(a,function(x) create_overall_rent_tbl2(x,string2,tbl_type))
  })
  
  return(toc_rent_overall2)
}

## -----------------------------------------------------------------------------
## Part 2.4 - Create Retention Tables - TS Create Elementary, MS/HS Strings
## -----------------------------------------------------------------------------

#[DO LATER]

#CS Schools
# cs_elem_rent_tbls<-create_overall_rent_tbl(rent_tbls, cs_elem,
#                                            "Retention Rate %")
# 
# cs_ms_hs_rent_tbls<-create_overall_rent_tbl(rent_tbls, cs_mid_hi,
#                                             "Retention Rate %")
# 
# #TS Schools
# ts_elem_rent_tbls<-create_overall_rent_tbl(rent_tbls,ts_elem_string,
#                                            "Retention Rate %")
# ts_ms_hs_rent_tbls<-create_overall_rent_tbl(rent_tbls, ts_ms_hs_string,
#                                             "Retention Rate %")
# ts_span_rent_tbls<-create_overall_rent_tbl(rent_tbls, ts_span_string,
#                                            "Retention Rate %")
# 
# #TOC - create overall list
# 
# toc_rent_overall_elem<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                             cs_elem, ts_elem_string,
#                                             "Retention Rate %")
# 
# toc_rent_overall_ms_hs<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                              cs_mid_hi, ts_ms_hs_string,
#                                              "Retention Rate %")
# 
# toc_rent_overall_span<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                             cs_mid_hi, ts_span_string,
#                                             "Retention Rate %")
# 
# toc_rent_overall_elem2<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                             cs_elem, ts_elem_string,
#                                             "Retention Rate % (n)")
# 
# toc_rent_overall_ms_hs2<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                              cs_mid_hi, ts_ms_hs_string,
#                                              "Retention Rate % (n)")
# 
# toc_rent_overall_span2<-create_overall_list2(hr_color_by_sch_rent_exp,
#                                             cs_mid_hi, ts_span_string,
#                                             "Retention Rate % (n)")  

## -----------------------------------------------------------------------------
## Part 2.5 - Create Retention Tables - By Neighborhood
## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(cohort_1_list, cohort_2_3_list, cohort_2_list,cohort_3_list,
     combined_cohort_rent_tbl, cohort_1_rent_tbl,
     cohort_2_rent_tbl, cohort_3_rent_tbl,cohort_teach_exp_rent_tbls,
     combined_overall_rent_tbls, cohort_overall_v1,
     cohort_overall_v2, cohort_overall_teach_exp_v1,cohort_overall_teach_exp_v2,
file = file.path(code_file_dir,"combined_cohort_rent_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------