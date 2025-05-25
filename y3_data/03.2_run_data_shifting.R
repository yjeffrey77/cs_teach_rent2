################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 03.1_run_data_shifting.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 5/23/25 >
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
      
      return(data.frame(school = NA,shift = "Shifting Rate %",
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

create_shift_tbl<-function(df){
  
  test<-df
  
  data19<-test %>% group_by(rent_type_19_20) %>% summarize(count19 = n())
  data20<-test %>% group_by(rent_type_20_21) %>% summarize(count20 = n())
  data21<-test %>% group_by(rent_type_21_22) %>% summarize(count21 = n())
  data22<-test %>% group_by(rent_type_22_23) %>% summarize(count22 = n())
  data23<-test %>% group_by(rent_type_23_24) %>% summarize(count23 = n())
  shift_tbl<-data.frame(shift = c("leaver", "new", "shifter", "stayer", NA))
  
  
  shift_data<-left_join(shift_tbl,data19, by = c("shift" = "rent_type_19_20"))
  shift_data<-left_join(shift_data,data20, by = c("shift" = "rent_type_20_21"))
  shift_data<-left_join(shift_data,data21, by = c("shift" = "rent_type_21_22"))
  shift_data<-left_join(shift_data,data22, by = c("shift" = "rent_type_22_23"))
  shift_data<-left_join(shift_data,data23, by = c("shift" = "rent_type_23_24"))
  
  colnames(shift_data)<-c("shift", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  
  total_teachers <- data.frame(shift = "Total",
                               yr_2019 = sum(shift_data[1,2], shift_data[3,2], shift_data[4,2], na.rm = T),
                               yr_2020 = sum(shift_data[1,3], shift_data[3,3], shift_data[4,3], na.rm = T),
                               yr_2021 = sum(shift_data[1,4], shift_data[3,4], shift_data[4,4], na.rm = T),
                               yr_2022 = sum(shift_data[1,5], shift_data[3,5], shift_data[4,5], na.rm = T),
                               yr_2023 = sum(shift_data[1,6], shift_data[3,6], shift_data[4,6], na.rm = T))
  colnames(total_teachers)<-c("shift", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  shift_data<-rbind(shift_data,total_teachers)
  
  shift_results<-data.frame(shift = "Shifting Rate %",
                            yr_2019 = ((sum(shift_data[3,2], na.rm = T)/shift_data[6,2])*100) %>% round(),
                            yr_2020 = ((sum(shift_data[3,3], na.rm = T)/shift_data[6,3])*100) %>% round(),
                            yr_2021 = ((sum(shift_data[3,4], na.rm = T)/shift_data[6,4])*100) %>% round(),
                            yr_2022 = ((sum(shift_data[3,5], na.rm = T)/shift_data[6,5])*100) %>% round(),
                            yr_2023 = ((sum(shift_data[3,6], na.rm = T)/shift_data[6,6])*100) %>% round())
  colnames(shift_results)<-c("shift", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  shift_data<-rbind(shift_data,shift_results)
  
  #filter out rows
  shift_data<-shift_data %>% filter(!is.na(shift))
  
  #add descriptive version
  shift_results2<-data.frame(shift = "Shifting Rate % (n)",
                             yr_2019 = str_c(shift_data[6,2],"% (",shift_data[5,2],")"),
                             yr_2020 = str_c(shift_data[6,3],"% (",shift_data[5,3],")"),
                             yr_2021 = str_c(shift_data[6,4],"% (",shift_data[5,4],")"),
                             yr_2022 = str_c(shift_data[6,5],"% (",shift_data[5,5],")"),
                             yr_2023 = str_c(shift_data[6,6],"% (",shift_data[5,6],")"))
  
  colnames(shift_results2)<-c("shift", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023")
  
  
  shift_data<-rbind(shift_data,shift_results2)
  
  return(shift_data)
}

## -----------------------------------------------------------------------------
## Part 2.1 - Create Shifting Tables - Preparation & Functions
## -----------------------------------------------------------------------------

shift_tbls<-map(hr_by_sch, create_shift_tbl)

#TOC
toc_shift_tbls<-toc_hr_by_sch_list

toc_shift_tbls[["overall"]]<-map(toc_shift_tbls[["overall"]], create_shift_tbl)

teach_year_string<-c("0-5 years","6-10 years","11-15 years","15+ years")
sch_name_string<-names(toc_shift_tbls[["overall"]])


for(teach_year in teach_year_string){
  for(sch_year in c("18_19", "19_20","20_21","21_22","22_23","23_24")){
    
    toc_shift_tbls[[teach_year]][[sch_year]]<-
      map(toc_shift_tbls[[teach_year]][[sch_year]], create_shift_tbl)
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
create_teach_exp_shift_tbl<-function(df, teach_year, sch_name){
  
  test<-df[[teach_year]][["18_19"]][[sch_name]] %>%
    select(shift, yr_2019)
  
  test2<-df[[teach_year]][["19_20"]][[sch_name]] %>%
    select(shift, yr_2020)
  
  test3<-df[[teach_year]][["20_21"]][[sch_name]] %>%
    select(shift, yr_2021)
  
  test4<-df[[teach_year]][["21_22"]][[sch_name]] %>%
    select(shift, yr_2022)
  
  test5<-df[[teach_year]][["22_23"]][[sch_name]] %>%
    select(shift, yr_2023)
  
  merge_data<-test %>% left_join(test2, by = "shift") %>% 
    left_join(test3, by = "shift") %>% 
    left_join(test4, by = "shift") %>% 
    left_join(test5, by = "shift")
  
  return(merge_data)
}

toc_shift_tbls_update<-vector("list", 4)
names(toc_shift_tbls_update)<-teach_year_string

for (teach_year in teach_year_string){
  toc_shift_tbls_update[[teach_year]]<-
    map(sch_name_string,
        function(x) create_teach_exp_shift_tbl(toc_shift_tbls, teach_year,x))
  names(toc_shift_tbls_update[[teach_year]])<-sch_name_string
}

add_names_to_tables<-function(df, sch_name){
  
  df_update<-df %>% mutate(school = sch_name)
  df_update<-df_update %>% select(school, everything())
  
  return(df_update)
}

create_overall_shift_tbl<-function(df, school_string, tbl_type){
  
  df_update<-df %>% keep(names(df) %in% school_string)
  
  df_update<-map2(df_update, names(df_update),
                     function(x,y){add_names_to_tables(x,y)})
  
  #Create Overall Shifting Rate
  
  shift_overall<-bind_rows(df_update) %>%
    filter(shift == "Shifting Rate %")
  
  shift_overall<-shift_overall %>%
    mutate(
      yr_2019 = as.numeric(yr_2019),
      yr_2020 = as.numeric(yr_2020),
      yr_2021 = as.numeric(yr_2021),
      yr_2022 = as.numeric(yr_2022),
      yr_2023 = as.numeric(yr_2023),
    )
  
  pivot_shift<-shift_overall %>%
    summarize(
      yr_2019 = sum(yr_2019, na.rm = T)/n() %>% round(),
      yr_2020 = sum(yr_2020, na.rm = T)/n() %>% round(),
      yr_2021 = sum(yr_2021, na.rm = T)/n() %>% round(),
      yr_2022 = sum(yr_2022, na.rm = T)/n() %>% round(),
      yr_2023 = sum(yr_2023, na.rm = T)/n() %>% round(),
    )
  
  pivot_shift$school<- "Overall Shifting Rate %"
  
  pivot_shift$shift<- "Overall Shifting Rate %"
  
  pivot_shift<-pivot_shift %>% select(school, shift, everything())
  
  if (tbl_type == "Shifting Rate %"){
  shift_overall<-rbind(shift_overall, pivot_shift)
  
  #round shifting numbers to whole numbers
  shift_overall<-shift_overall %>% 
    mutate(yr_2019 = round(yr_2019),
           yr_2020 = round(yr_2020),
           yr_2021 = round(yr_2021),
           yr_2022 = round(yr_2022),
           yr_2023 = round(yr_2023))
  }
  
  if (tbl_type == "Shifting Rate % (n)"){
  
    shift_overall<-bind_rows(df_update) %>%
      filter(shift == "Shifting Rate % (n)")
    
    pivot_shift<-pivot_shift %>% mutate(
      yr_2019 = str_c(as.character(yr_2019 %>% round()), "%"),
      yr_2020 = str_c(as.character(yr_2020 %>% round()), "%"),
      yr_2021 = str_c(as.character(yr_2021 %>% round()), "%"),
      yr_2022 = str_c(as.character(yr_2022 %>% round()), "%"),
      yr_2023 = str_c(as.character(yr_2023 %>% round()), "%"))
    
    shift_overall<-rbind(shift_overall, pivot_shift)
  }
  
  return(shift_overall)
}

## -----------------------------------------------------------------------------
## Part 2.2 - Create Shifting Tables - Overall
## -----------------------------------------------------------------------------

#all teachers
#community schools
cs_shift_tbls<-create_overall_shift_tbl(shift_tbls, cs_string,
                                      "Shifting Rate %")

cs_shift_tbls2<-create_overall_shift_tbl(shift_tbls, cs_string,
                                      "Shifting Rate % (n)")

#comparison schools (ts)
all_sch_string<-names(shift_tbls)
ts_sch_string<-all_sch_string[!all_sch_string %in% cs_string]
ts_shift_tbls<-create_overall_shift_tbl(shift_tbls,ts_sch_string,
                                      "Shifting Rate %")

#TOC - create overall list

create_overall_list<-function(df1, df2, string1, string2, tbl_type){
  
  shift_overall<-vector("list", 2)
  names(shift_overall)<-c("cs","ts")
  
  for(sch_type in c("cs","ts")){
    shift_overall[[sch_type]]<-vector("list", 5)
    names(shift_overall[[sch_type]])<-c("overall", "0-5 years","6-10 years",
                                        "11-15 years", "15+ years")
  }
  
  #add overall retention tables
  
  #cs
  for (table_type in c("overall")){
    shift_overall[["cs"]][[table_type]]<-
      create_overall_shift_tbl(df1[[table_type]], string1, tbl_type)
  }
  
  for (table_type in teach_year_string){
    shift_overall[["cs"]][[table_type]]<-
      create_overall_shift_tbl(df2[[table_type]], string1, tbl_type)
  }
  
  #ts
  for (table_type in c("overall")){
    shift_overall[["ts"]][[table_type]]<-
      create_overall_shift_tbl(df1[[table_type]], string2, tbl_type)
  }
  
  for (table_type in teach_year_string){
    shift_overall[["ts"]][[table_type]]<-
      create_overall_shift_tbl(df2[[table_type]], string2, tbl_type)
  }
  
  return(shift_overall)
}

toc_shift_overall<-create_overall_list(toc_shift_tbls,
                                      toc_shift_tbls_update,
                                      cs_string, ts_sch_string,
                                      "Shifting Rate %")

toc_shift_overall2<-create_overall_list(toc_shift_tbls,
                                      toc_shift_tbls_update,
                                      cs_string, ts_sch_string,
                                      "Shifting Rate % (n)")

## -----------------------------------------------------------------------------
## Part 2.3 - Create Shifting Tables - Create Elementary & MS/HS Strings
## -----------------------------------------------------------------------------

#create elementary and middle/high school comparison school lists
ts_shift_elem<-ts_shift_tbls %>%
  filter(grepl("Elementary", ts_shift_tbls$school))

ts_elem_string<-c(ts_shift_elem$school,
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
## Part 2.4 - Create Shifting Tables - TS Create Elementary, MS/HS Strings
## -----------------------------------------------------------------------------

#CS Schools
cs_elem_shift_tbls<-create_overall_shift_tbl(shift_tbls, cs_elem,
                                             "Shifting Rate %")
cs_ms_hs_shift_tbls<-create_overall_shift_tbl(shift_tbls, cs_mid_hi,
                                              "Shifting Rate %")

#TS Schools
ts_elem_shift_tbls<-create_overall_shift_tbl(shift_tbls,ts_elem_string,
                                             "Shifting Rate %")
ts_ms_hs_shift_tbls<-create_overall_shift_tbl(shift_tbls, ts_ms_hs_string,
                                              "Shifting Rate %")
ts_span_shift_tbls<-create_overall_shift_tbl(shift_tbls, ts_span_string,
                                             "Shifting Rate %")

#TOC - create overall list
toc_shift_overall_elem<-create_overall_list(toc_shift_tbls,
                                            toc_shift_tbls_update,
                                            cs_elem, ts_elem_string,
                                            "Shifting Rate %")

toc_shift_overall_ms_hs<-create_overall_list(toc_shift_tbls,
                                             toc_shift_tbls_update,
                                             cs_mid_hi, ts_ms_hs_string,
                                             "Shifting Rate %")

toc_shift_overall_span<-create_overall_list(toc_shift_tbls,
                                            toc_shift_tbls_update,
                                            cs_mid_hi, ts_ms_hs_string,
                                            "Shifting Rate %")

toc_shift_overall_span<-toc_shift_overall_span[["ts"]]

## -----------------------------------------------------------------------------
## Part 2.5 - Create Shifting Tables - By Neighborhood
## -----------------------------------------------------------------------------

neighborhood_strings<-c("hamilton","monroe","bell_cudahy_maywood",
                        "hollywood","la_mid_city","downtown","sun_valley",
                        "gardena","south_la","riveria","south_mid_city",
                        "venice","heet","macarthur_park",
                        "historic_central_ave","fremont","carson",
                        "boyle_heights","lincoln_heights_el_sereno",
                        "van_nuys_valley_glen","huntington_park_vernon",
                        "panorama_city")

shift_by_neighborhood<-vector("list", length(neighborhood_strings))
names(shift_by_neighborhood)<-neighborhood_strings

add_tables_neighborhood<-function(df){
  df<-vector("list", 4)
  names(df)<-c("overall", "cs", "ts", "combined_overall")
  return(df)
}

create_neighborhood_shift_tbls<-function(df, df_shift_tbl,
                                         string_neighborhood, tbl_type){
  
  df[['overall']]<-create_overall_shift_tbl(df_shift_tbl,
                                            string_neighborhood,
                                            tbl_type) %>% 
    safe_function_df()
  df[['cs']]<-
    create_overall_shift_tbl(df_shift_tbl,
                             string_neighborhood[string_neighborhood %in%
                                                   cs_string],
                             tbl_type) %>% 
    safe_function_df()
  df[['ts']]<-
    create_overall_shift_tbl(df_shift_tbl,
                             string_neighborhood[string_neighborhood %in%
                                                   ts_sch_string],
                             tbl_type) %>% 
    safe_function_df()
  
  #created combined overall table
  combined<-df[['overall']] %>% filter(shift == "Overall Shifting Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Shifting Rate %" ~ "Combined Overall Shifting Rate")) 
  
  cs<-df[['cs']] %>% filter(shift == "Overall Shifting Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Shifting Rate %" ~ "CS Overall Shifting Rate")) 
  
  ts<-df[['ts']] %>% filter(shift == "Overall Shifting Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Shifting Rate %" ~ "TS Overall Shifting Rate")) 
  
  df[['combined_overall']]<-rbind(cs, ts, combined)
  
  return(df)
}

#Create neighborhood shift

#all teachers
shift_by_neighborhood<-map(shift_by_neighborhood, add_tables_neighborhood)
shift_by_neighborhood<-map2(shift_by_neighborhood,
                            neighborhood_string_list,
                            function(x,y) create_neighborhood_shift_tbls(x,shift_tbls, y,
                                                                         "Shifting Rate %"))

#toc
shift_by_neighborhood_toc<-map(shift_by_neighborhood, add_tables_neighborhood)
shift_by_neighborhood_toc<-map2(shift_by_neighborhood,
                                neighborhood_string_list,
                                function(x,y) create_neighborhood_shift_tbls(x,toc_shift_tbls[["overall"]], y,
                                                                             "Shifting Rate %"))

## -----------------------------------------------------------------------------
## Part 2.6 - Create Retention Tables - By Cohort
## -----------------------------------------------------------------------------

test_df<-list(overall = NA, cs = NA, ts = NA)

#cohort 1
shift_cohort_1<-create_neighborhood_shift_tbls(test_df,toc_shift_tbls[["overall"]],
                                               cs_cohort1_string,
                                               "Shifting Rate %")

#cohort 2 only
shift_cohort_2_only<-create_neighborhood_shift_tbls(test_df,toc_shift_tbls[["overall"]],
                                                    cs_cohort2_string,
                                                    "Shifting Rate %")

#cohort 2 and 3
shift_cohort_2_3<-create_neighborhood_shift_tbls(test_df,toc_shift_tbls[["overall"]],
                                                 cs_cohort2_3_string,
                                                 "Shifting Rate %")

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(cs_shift_tbls, cs_shift_tbls2, ts_shift_tbls,
     cs_elem_shift_tbls, cs_ms_hs_shift_tbls,
     ts_elem_shift_tbls, ts_ms_hs_shift_tbls, ts_span_shift_tbls, ts_sch_string,
     shift_by_neighborhood, file = file.path(code_file_dir, "shift_tbls.RData"))


save(toc_shift_overall,toc_shift_overall2, toc_shift_overall_elem,
     toc_shift_overall_ms_hs,toc_shift_overall_span, 
     shift_by_neighborhood_toc,toc_shift_tbls, toc_shift_tbls_update,
     shift_cohort_1, shift_cohort_2_3,shift_cohort_2_only,
     file = file.path(code_file_dir, "toc_shift_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------