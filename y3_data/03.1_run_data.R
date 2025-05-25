################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 03.1_run_data.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/26/24 >
##
################################################################################

#Goal: Runs Analyses and Stores them in Pivot Tables

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
                         "Analyses", "cs_teach_rent2", "y3_data")

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
source(file.path(code_file_dir, "00_school_lists.R"))

## -----------------------------------------------------------------------------
## Part 1 - Retention Rate Function
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
## Part 2.1 - Create Retention Tables - Preparation & Functions
## -----------------------------------------------------------------------------

rent_tbls<-map(hr_by_sch, create_rent_tbl)

#TOC
toc_rent_tbls<-toc_hr_by_sch_list

toc_rent_tbls[["overall"]]<-map(toc_rent_tbls[["overall"]], create_rent_tbl)

teach_year_string<-c("0-5 years","6-10 years","11-15 years","15+ years")
sch_name_string<-names(toc_rent_tbls[["overall"]])


for(teach_year in teach_year_string){
  for(sch_year in c("18_19", "19_20","20_21","21_22","22_23","23_24")){
    
    toc_rent_tbls[[teach_year]][[sch_year]]<-
      map(toc_rent_tbls[[teach_year]][[sch_year]], create_rent_tbl)
  }
}

#Keep Only the necessary year 
#18-19 - 19
#19-20 - 20
#20-21 - 21
#21-22 - 22
#22-23 - 23

#select and merge variables

#function merges all the specific years together by veteran status
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

toc_rent_tbls_update<-vector("list", 4)
names(toc_rent_tbls_update)<-teach_year_string

for (teach_year in teach_year_string){
  toc_rent_tbls_update[[teach_year]]<-
    map(sch_name_string,
        function(x) create_teach_exp_rent_tbl(toc_rent_tbls, teach_year,x))
  names(toc_rent_tbls_update[[teach_year]])<-sch_name_string
}

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

## -----------------------------------------------------------------------------
## Part 2.2 - Create Retention Tables - Overall
## -----------------------------------------------------------------------------

#all teachers
#community schools
cs_rent_tbls<-create_overall_rent_tbl(rent_tbls, cs_string,
                                      "Retention Rate %")

cs_rent_tbls2<-create_overall_rent_tbl(rent_tbls, cs_string,
                                      "Retention Rate % (n)")

#comparison schools (ts)
all_sch_string<-names(rent_tbls)
ts_sch_string<-all_sch_string[!all_sch_string %in% cs_string]
ts_rent_tbls<-create_overall_rent_tbl(rent_tbls,ts_sch_string,
                                      "Retention Rate %")

#TOC - create overall list

create_overall_list<-function(df1, df2, string1, string2, tbl_type){
  
  rent_overall<-vector("list", 2)
  names(rent_overall)<-c("cs","ts")
  
  for(sch_type in c("cs","ts")){
    rent_overall[[sch_type]]<-vector("list", 5)
    names(rent_overall[[sch_type]])<-c("overall", "0-5 years","6-10 years",
                                           "11-15 years", "15+ years")
  }
  
  #add overall retention tables
  
  #cs
  for (table_type in c("overall")){
    rent_overall[["cs"]][[table_type]]<-
      create_overall_rent_tbl(df1[[table_type]], string1, tbl_type)
  }
  
  for (table_type in teach_year_string){
    rent_overall[["cs"]][[table_type]]<-
      create_overall_rent_tbl(df2[[table_type]], string1, tbl_type)
  }
  
  #ts
  for (table_type in c("overall")){
    rent_overall[["ts"]][[table_type]]<-
      create_overall_rent_tbl(df1[[table_type]], string2, tbl_type)
  }
  
  for (table_type in teach_year_string){
    rent_overall[["ts"]][[table_type]]<-
      create_overall_rent_tbl(df2[[table_type]], string2, tbl_type)
  }
  
  return(rent_overall)
}

toc_rent_overall<-create_overall_list(toc_rent_tbls,
                                      toc_rent_tbls_update,
                                      cs_string, ts_sch_string,
                                      "Retention Rate %")

toc_rent_overall2<-create_overall_list(toc_rent_tbls,
                                      toc_rent_tbls_update,
                                      cs_string, ts_sch_string,
                                      "Retention Rate % (n)")

## -----------------------------------------------------------------------------
## Part 2.3 - Create Retention Tables - Create Elementary & MS/HS Strings
## -----------------------------------------------------------------------------

#create elementary and middle/high school comparison school lists
ts_rent_elem<-ts_rent_tbls %>%
  filter(grepl("Elementary", ts_rent_tbls$school))

ts_elem_string<-c(ts_rent_elem$school,
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

## -----------------------------------------------------------------------------
## Part 2.4 - Create Retention Tables - TS Create Elementary, MS/HS Strings
## -----------------------------------------------------------------------------

#CS Schools
cs_elem_rent_tbls<-create_overall_rent_tbl(rent_tbls, cs_elem,
                                           "Retention Rate %")
cs_ms_hs_rent_tbls<-create_overall_rent_tbl(rent_tbls, cs_mid_hi,
                                            "Retention Rate %")

#TS Schools
ts_elem_rent_tbls<-create_overall_rent_tbl(rent_tbls,ts_elem_string,
                                           "Retention Rate %")
ts_ms_hs_rent_tbls<-create_overall_rent_tbl(rent_tbls, ts_ms_hs_string,
                                            "Retention Rate %")
ts_span_rent_tbls<-create_overall_rent_tbl(rent_tbls, ts_span_string,
                                           "Retention Rate %")

#TOC - create overall list
toc_rent_overall_elem<-create_overall_list(toc_rent_tbls,
                                      toc_rent_tbls_update,
                                      cs_elem, ts_elem_string,
                                      "Retention Rate %")

toc_rent_overall_ms_hs<-create_overall_list(toc_rent_tbls,
                                           toc_rent_tbls_update,
                                           cs_mid_hi, ts_ms_hs_string,
                                           "Retention Rate %")

toc_rent_overall_span<-create_overall_list(toc_rent_tbls,
                                           toc_rent_tbls_update,
                                           cs_mid_hi, ts_ms_hs_string,
                                           "Retention Rate %")
  
toc_rent_overall_span<-toc_rent_overall_span[["ts"]]

## -----------------------------------------------------------------------------
## Part 2.5 - Create Retention Tables - By Neighborhood
## -----------------------------------------------------------------------------

neighborhood_strings<-c("hamilton","monroe","bell_cudahy_maywood",
                         "hollywood","la_mid_city","downtown","sun_valley",
                         "gardena","south_la","riveria","south_mid_city",
                         "venice","heet","macarthur_park",
                         "historic_central_ave","fremont","carson",
                         "boyle_heights","lincoln_heights_el_sereno",
                         "van_nuys_valley_glen","huntington_park_vernon",
                         "panorama_city")

rent_by_neighborhood<-vector("list", length(neighborhood_strings))
names(rent_by_neighborhood)<-neighborhood_strings

add_tables_neighborhood<-function(df){
  df<-vector("list", 4)
  names(df)<-c("overall", "cs", "ts", "combined_overall")
  return(df)
}

create_neighborhood_rent_tbls<-function(df, df_rent_tbl,
                                        string_neighborhood, tbl_type){
  
  df[['overall']]<-create_overall_rent_tbl(df_rent_tbl,
                                           string_neighborhood,
                                           tbl_type) %>% 
    safe_function_df()
  df[['cs']]<-
    create_overall_rent_tbl(df_rent_tbl,
                            string_neighborhood[string_neighborhood %in%
                                                  cs_string],
                            tbl_type) %>% 
    safe_function_df()
  df[['ts']]<-
    create_overall_rent_tbl(df_rent_tbl,
                            string_neighborhood[string_neighborhood %in%
                                                  ts_sch_string],
                            tbl_type) %>% 
    safe_function_df()
  
  #created combined overall table
  combined<-df[['overall']] %>% filter(rent == "Overall Retention Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate %" ~ "Combined Overall Retention Rate")) 
  
  cs<-df[['cs']] %>% filter(rent == "Overall Retention Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate %" ~ "CS Overall Retention Rate")) 
  
  ts<-df[['ts']] %>% filter(rent == "Overall Retention Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate %" ~ "TS Overall Retention Rate")) 
  
  df[['combined_overall']]<-rbind(cs, ts, combined)
    
  return(df)
}

#Create neighborhood rent

#all teachers
rent_by_neighborhood<-map(rent_by_neighborhood, add_tables_neighborhood)
rent_by_neighborhood<-map2(rent_by_neighborhood,
                           neighborhood_string_list,
                           function(x,y) create_neighborhood_rent_tbls(x,rent_tbls, y,
                                                                       "Retention Rate %"))

#toc
rent_by_neighborhood_toc<-map(rent_by_neighborhood, add_tables_neighborhood)
rent_by_neighborhood_toc<-map2(rent_by_neighborhood,
                           neighborhood_string_list,
                           function(x,y) create_neighborhood_rent_tbls(x,toc_rent_tbls[["overall"]], y,
                                                                       "Retention Rate %"))


## -----------------------------------------------------------------------------
## Part 2.6 - Create Retention Tables - By Cohort
## -----------------------------------------------------------------------------

test_df<-list(overall = NA, cs = NA, ts = NA)

#cohort 1
rent_cohort_1<-create_neighborhood_rent_tbls(test_df,toc_rent_tbls[["overall"]],
                                               cs_cohort1_string,
                                               "Retention Rate %")

#cohort 2 only
rent_cohort_2_only<-create_neighborhood_rent_tbls(test_df,toc_rent_tbls[["overall"]],
                                                  cs_cohort2_string,
                                               "Retention Rate %")

#cohort 2 and 3
rent_cohort_2_3<-create_neighborhood_rent_tbls(test_df,toc_rent_tbls[["overall"]],
                                    cs_cohort2_3_string,
                                    "Retention Rate %")

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(cs_rent_tbls, cs_rent_tbls2, ts_rent_tbls,
     cs_elem_rent_tbls, cs_ms_hs_rent_tbls,
     ts_elem_rent_tbls, ts_ms_hs_rent_tbls, ts_span_rent_tbls, ts_sch_string,
     rent_by_neighborhood, file = file.path(code_file_dir, "rent_tbls.RData"))


save(toc_rent_overall,toc_rent_overall2, toc_rent_overall_elem,
     toc_rent_overall_ms_hs,toc_rent_overall_span, 
     rent_by_neighborhood_toc,toc_rent_tbls, toc_rent_tbls_update,
     rent_cohort_1, rent_cohort_2_3,rent_cohort_2_only,
     file = file.path(code_file_dir, "toc_rent_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------