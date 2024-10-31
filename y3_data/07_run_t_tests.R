################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 06_run_t_tests.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 10/31/24 >
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
                         "Analyses", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")


data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

#error function

safe_function_df<-function(expr){
  result<- tryCatch(
    {
      expr
    },
    error = function(e){
      
      return(data.frame(
        cs_mean = NA,
        ts_mean = NA,
        mean_difference = NA,
        statistic = NA,
        p_value = NA,
        conf_low = NA,
        conf_high = NA,
        df = NA
      ))
      
    }
    
  )
  return(result)
}


## ---------------------------
## load & inspect data
## ---------------------------

 load(file.path(code_file_dir, "hr_by_sch.RDATA"))
 
 load(file.path(code_file_dir, "rent_plots_toc.RData"))

## -----------------------------------------------------------------------------
## Part 1 - T-test
## -----------------------------------------------------------------------------

create_t_test_output<-function(df){
  
  df_update<-df %>% filter(School %in% c("CS", "TS")) %>% 
    select(Year, Retention, School)
  
  t_test_result<-t.test(Retention~School, data = df_update)
  
  t_test_df <- data.frame(
    cs_mean = t_test_result$estimate[1] %>% round(3),
    ts_mean = t_test_result$estimate[2] %>% round(3),
    mean_difference = (t_test_result$estimate[1] - t_test_result$estimate[2]) %>% round(3),
    statistic = t_test_result$statistic %>% round(2),
    p_value = t_test_result$p.value %>% round(3),
    conf_low = t_test_result$conf.int[1] %>% round(3),
    conf_high = t_test_result$conf.int[2] %>% round(3),
    df = t_test_result$parameter %>% round(3)
  )
  
  return(t_test_df)
}

neighborhood_t_test<-map(plot_neigh_tbls_toc,
                         function(x){create_t_test_output(x) %>%
                             safe_function_df()})

combined_t_test<-bind_rows(.id = "neigborhood", neighborhood_t_test)
rownames(combined_t_test)<-NULL


## -----------------------------------------------------------------------------
## Part 2 - Proportions test
## -----------------------------------------------------------------------------

#DO LATER




## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(neighborhood_t_test,combined_t_test,
     file = file.path(code_file_dir,"cs_teach_rent2", "test_tbls.RData"))



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------