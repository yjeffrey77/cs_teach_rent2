################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 08.5-significance-tests.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/25/25 >
##
################################################################################

#Goal: Create significance tests for teacher retention study.

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

code_file_dir<-file.path("C:/Users/yjeff/Desktop/Community Schools/Teacher Retention Data","LA",
                         "Analyses","cs_teach_rent2", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")

data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

lausd_vet_bipoc<-read_excel(file.path(".","y3_data","lausd-vet-perc.xlsx"),
                            sheet = "Sheet2")

## ---------------------------
## helper functions
## ---------------------------


## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "vet_combined_tbls.RData"))
load(file.path(code_file_dir, "demo_combined_tbls.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Update the lausd_vet_bipoc dataset
## -----------------------------------------------------------------------------

#create a 2019 row
row_2019<-data.frame(year = "2019-20")

lausd_vet_bipoc<-full_join(row_2019, lausd_vet_bipoc, by = c("year"))

#filter out the last year
lausd_vet_bipoc<-lausd_vet_bipoc %>% mutate(
  all = case_when(year == "2019-20" ~ 0, TRUE ~ all),
  elem = case_when(year == "2019-20" ~ 0, TRUE ~ elem),
  secondary = case_when(year == "2019-20" ~ 0, TRUE ~ secondary)
)

#filter out cases
lausd_vet_bipoc<-lausd_vet_bipoc %>% filter(year != "2024-25")

## -----------------------------------------------------------------------------
## Part 2 - 1-sample t-test (Overall)
## -----------------------------------------------------------------------------

extract_ttest_stats_df <- function(test_result) {
  data.frame(
    mean      = unname(test_result$estimate) %>% round(2),
    mu        = unname(test_result$null.value),
    t_stat    = unname(test_result$statistic) %>% round(2),
    df        = unname(test_result$parameter),
    se        = unname(test_result$stderr) %>% round(2),   # Added SE
    p_value   = unname(test_result$p.value) %>% round(3),
    conf_low  = test_result$conf.int[1] %>% round(3),
    conf_high = test_result$conf.int[2] %>% round(3),
    stringsAsFactors = FALSE
  )
}

create_ttest_df<-function(p, mu_value){
 results<-t.test(p, mu = mu_value)
 results_df<-extract_ttest_stats_df(results)
  return(results_df)
}

create_results_df<-function(df, mu){
  
  cs_teacher_vet_perc<-df %>% 
    filter(vet_status != "Percent of TOC Teachers")
  
  p_list<-list(cs_teacher_vet_perc$yr_19_20[-length(cs_teacher_vet_perc$yr_19_20)],
               cs_teacher_vet_perc$yr_20_21[-length(cs_teacher_vet_perc$yr_20_21)],
               cs_teacher_vet_perc$yr_21_22[-length(cs_teacher_vet_perc$yr_21_22)],
               cs_teacher_vet_perc$yr_22_23[-length(cs_teacher_vet_perc$yr_22_23)],
               cs_teacher_vet_perc$yr_23_24[-length(cs_teacher_vet_perc$yr_23_24)])
  names(p_list)<-c("19-20","20-21","21-22","22-23","23-24")
  
  results_df<-map2(p_list, mu,
              function(x,y){
                create_ttest_df(x,y)
              })
  names(results_df)<-names(p_list)
  
  results_df<-bind_rows(results_df, .id = "year")
  return(results_df)
}

create_results_list<-function(demo_df){

  results<-vector("list",3)
  names(results)<-c("all", "elem", "second")
  
  results[["all"]]<-create_results_df(demo_df[["all"]][["teachers"]][["all"]][["percent"]][["10+ years"]],lausd_vet_bipoc$all)
  
  results[["elem"]]<-create_results_df(demo_df[["elem"]][["teachers"]][["all"]][["percent"]][["10+ years"]],lausd_vet_bipoc$elem)
  
  results[["second"]]<-create_results_df(demo_df[["mid_hi"]][["teachers"]][["all"]][["percent"]][["10+ years"]],lausd_vet_bipoc$secondary)
  
  results<-bind_rows(results, .id="teacher")
  return(results)  
}

create_results_list_cohort<-function(demo_df){
  
  results<-create_results_df(demo_df[["teachers"]][["all"]][["percent"]][["10+ years"]],lausd_vet_bipoc$all)
  results$teacher<-"all"

  results<-results %>% select(teacher, everything())
  
  return(results)  
}


results<-vector("list", 4)
names(results)<-c("overall","cohort1","cohort2","cohort3")

results[["overall"]]<-
  create_results_list(demo_combined_tbls2[["overall"]][["cs"]])

results[["cohort1"]]<-
  create_results_list_cohort(demo_combined_tbls2[["cohort"]][["cohort1"]])

results[["cohort2"]]<-
  create_results_list_cohort(demo_combined_tbls2[["cohort"]][["cohort2"]])

results[["cohort3"]]<-
  create_results_list_cohort(demo_combined_tbls2[["cohort"]][["cohort3"]])

results<-bind_rows(results, .id="sample")


#clean 2019-20 rows
results<-results %>% mutate(
  mu = case_when(year == "19-20" ~ NA, TRUE ~ mu),
  t_stat = case_when(year == "19-20" ~ NA, TRUE ~ t_stat),
  df = case_when(year == "19-20" ~ NA, TRUE ~ df),
  se = case_when(year == "19-20" ~ NA, TRUE ~ se),
  p_value = case_when(year == "19-20" ~ NA, TRUE ~ p_value),
  conf_low = case_when(year == "19-20" ~ NA, TRUE ~ conf_low),
  conf_high = case_when(year == "19-20" ~ NA, TRUE ~ conf_high),
)

# #check
test<-t.test(demo_combined_tbls2[["overall"]][["cs"]][["all"]][["teachers"]][["all"]][["percent"]][["10+ years"]]$yr_19_20,
       mu = lausd_vet_bipoc$all[1])

## -----------------------------------------------------------------------------
## Part 3 - Clean Data
## -----------------------------------------------------------------------------

results <- results %>%
  mutate(
    confidence_interval = str_c("[", conf_low, ", ", conf_high, "]"),
    mean_se = if_else(
      is.na(se),
      as.character(mean),                      # show only mean if SE is NA
      str_c(mean, " (", se, ")")               # show mean(SE) otherwise
    )
  )

results<-results %>% select(-c(conf_low, conf_high, mean, se))

results<-results %>% select(sample, teacher, year, mean_se, everything())

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

write.csv(results, file.path(".","y3_data","t-test-results.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------