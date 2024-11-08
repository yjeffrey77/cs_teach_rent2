################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 07_run_t_tests.R >
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
                         "Analyses", "cs_teach_rent2", "y3_data")

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
 # 
 #load(file.path(code_file_dir, "toc_rent_tbls.RData"))
 load(file.path(code_file_dir, "rent_plots_toc.RData"))
 load(file.path(code_file_dir, "veteran_plots.RData"))

load(file.path(code_file_dir, "veteran_rent_tbls.RData"))

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

#Overall Sample, Elementary School, Middle School

# overall_t_test<-map(toc_plot_combined_tbls,
#                     function(sample){
#                       map(sample,
#                           function(x){create_t_test_output(x) %>%
#                               safe_function_df()})
#                     })

full_sample_test<-create_t_test_output(toc_plot_combined_tbls[["overall"]][["overall"]])

overall_t_test<-map(plot_combined_tbls,
                     function(b){
                       map(b ,
                           function(a) map(a, create_t_test_output))
                     })

overall_t_test_df<-map(overall_t_test, function(b){
  map(b, function(sample){
    bind_rows(sample, .id = "veteran_status")
  })
})
 
overall_t_test_df<-map(overall_t_test_df, function(a) 
  bind_rows(a, .id = "version"))

                    
overall_t_test_df<-bind_rows(overall_t_test_df, .id = "type")
rownames(overall_t_test_df)<-NULL


#By Neighborhood
neighborhood_t_test<-map(plot_neigh_tbls_toc,
                         function(x){create_t_test_output(x) %>%
                             safe_function_df()})

combined_t_test<-bind_rows(.id = "neigborhood", neighborhood_t_test)
rownames(combined_t_test)<-NULL

#By Veteran Status
t_test_neighborhood_veteran<-map(neigh_plot_tbls_veteran,
                                 function(version){
                                   map(version,
                                       function(veteran_status){
                                         map(veteran_status,
                                             function(x){create_t_test_output(x) %>%
                                                 safe_function_df()})
                                       })
                                 })

#combined t-test  
create_combined_t_test_results<-function(df){
  
  update_df<-bind_rows(.id = "neigborhood", df)
  rownames(update_df)<-NULL
  return(update_df)
}

combined_t_test_veteran<-map(t_test_neighborhood_veteran,
                             function(version){
                               map(version,
                                   create_combined_t_test_results)
                             })
  
## -----------------------------------------------------------------------------
## Part 2.1 - Create Proportions Test Functions
## -----------------------------------------------------------------------------

#Steps 

#first step, convert table to show the number of teachers who stayed in each school

# Define cleaning function
extract_and_multiply <- function(input_string) {
  # Use regular expressions to extract the numbers
  # The first part extracts the percentage, the second part extracts the number in parentheses
  percent <- as.numeric(sub("%.*", "", input_string)) / 100
  number_in_parentheses <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", input_string))
  
  # Multiply them together
  result <- percent * number_in_parentheses
  
  return(result)
}

extract_and_sample_size <- function(input_string) {
  # Use regular expressions to extract the numbers
  number_in_parentheses <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", input_string))
  
  # Multiply them together
  result <- number_in_parentheses
  
  return(result)
}

create_stayed_tbl<-function(df){
  
  update_df<- df %>% 
    mutate(stayed_2019 = extract_and_multiply(yr_2019) %>% round(0),
           stayed_2020 = extract_and_multiply(yr_2020) %>% round(0),
           stayed_2021 = extract_and_multiply(yr_2021) %>% round(0),
           stayed_2022 = extract_and_multiply(yr_2022) %>% round(0),
           stayed_2023 = extract_and_multiply(yr_2023) %>% round(0),
           n_2019 = extract_and_sample_size(yr_2019),
           n_2020 = extract_and_sample_size(yr_2020),
           n_2021 = extract_and_sample_size(yr_2021),
           n_2022 = extract_and_sample_size(yr_2022),
           n_2023 = extract_and_sample_size(yr_2023)) %>% 
    select(school,stayed_2019, stayed_2020,stayed_2021, stayed_2022, stayed_2023,
           n_2019, n_2020, n_2021, n_2022,n_2023)
  
  return(update_df)
}

create_sum_tbl<-function(df){
  
  update_df<-data.frame(stayed_2019 = sum(df$stayed_2019, na.rm =T),
                    stayed_2020 = sum(df$stayed_2020, na.rm =T),
                    stayed_2021 = sum(df$stayed_2021, na.rm =T),
                    stayed_2022 = sum(df$stayed_2022, na.rm =T),
                    stayed_2023 = sum(df$stayed_2023, na.rm =T),
                    n_2019 = sum(df$n_2019, na.rm =T),
                    n_2020 = sum(df$n_2020, na.rm =T),
                    n_2021 = sum(df$n_2021, na.rm =T),
                    n_2022 = sum(df$n_2022, na.rm =T),
                    n_2023 = sum(df$n_2023, na.rm =T))
  update_df<-update_df %>% 
    mutate(
      prop_2019 = stayed_2019/n_2019,
      prop_2020 = stayed_2020/n_2020,
      prop_2021 = stayed_2021/n_2021,
      prop_2022 = stayed_2022/n_2022,
      prop_2023 = stayed_2023/n_2023,
    )
  return(update_df)
}

create_stayed_count_list<-function(df){
  
  list_update<-vector("list",5)
  list_update[[1]]<-c(df$stayed_2019[1],
                      df$stayed_2019[2])
  list_update[[2]]<-c(df$stayed_2020[1],
                      df$stayed_2020[2])
  list_update[[3]]<-c(df$stayed_2021[1],
                      df$stayed_2021[2])
  list_update[[4]]<-c(df$stayed_2022[1],
                      df$stayed_2022[2])
  list_update[[5]]<-c(df$stayed_2023[1],
                      df$stayed_2023[2])
  
  names(list_update)<-c(2019:2023)
  
  return(list_update)
}

create_n_count_list<-function(df){
  
  list_update<-vector("list",5)
  list_update[[1]]<-c(df$n_2019[1],
                      df$n_2019[2])
  list_update[[2]]<-c(df$n_2020[1],
                      df$n_2020[2])
  list_update[[3]]<-c(df$n_2021[1],
                      df$n_2021[2])
  list_update[[4]]<-c(df$n_2022[1],
                      df$n_2022[2])
  list_update[[5]]<-c(df$n_2023[1],
                      df$n_2023[2])
  
  names(list_update)<-c(2019:2023)
  
  return(list_update)
}

extract_prop_info<-function(test_results){
  
  result_df <- data.frame(
    estimate_school1 = test_results$estimate[1] %>% round(2),                    # Proportion of school 1
    estimate_school2 = test_results$estimate[2] %>% round(2),                    # Proportion of school 2
    p_value = test_results$p.value %>% round(2),                                 # p-value of the test
    conf_low = test_results$conf.int[1] %>% round(2),                            # Lower bound of confidence interval
    conf_high = test_results$conf.int[2] %>% round(2),                           # Upper bound of confidence interval
    statistic = test_results$statistic %>% round(2),                             # Test statistic
    parameter = test_results$parameter                              # Degrees of freedom
  )
  return(result_df)
}

create_prop_test_df<-function(stayed_counts_df,
                              total_counts_df){
  
  df <- prop.test(stayed_counts_df,
                  total_counts_df, alternative = "two.sided")
  
  test_result_df<-extract_prop_info(df)
  
  test_result_df<-test_result_df %>% 
    mutate(diff = estimate_school1 - estimate_school2)
  
  return(test_result_df)
}

create_prop_tbl<-function(cs_df, ts_df){
  
  #create update_df
  cs_df_update<-create_stayed_tbl(cs_df)
  ts_df_update<-create_stayed_tbl(ts_df)
  
  #sum the data together
  cs_sum_df<-create_sum_tbl(cs_df_update)
  ts_sum_df<-create_sum_tbl(ts_df_update)
  
  update_df<-rbind(cs_sum_df, ts_sum_df)
  
  #create lists
  stay_list<-create_stayed_count_list(update_df)
  n_list<-create_n_count_list(update_df)
  
  #run proportions test
  prop_tbl<-map2(stay_list,n_list,
                 function(x,y) create_prop_test_df(x,y))
  
  prop_tbl<-bind_rows(prop_tbl, .id = "year")
  rownames(prop_tbl)<-NULL
  
  return(prop_tbl)
}

## -----------------------------------------------------------------------------
## Part 2.2 - Create Proportions Test 
## -----------------------------------------------------------------------------

prop_test<-vector("list", 6)
names(prop_test)<-c("overall", names(toc_rent_overall2[["cs"]][["original_5"]]))

overall_prop_test<-create_prop_tbl(cs_rent_tbls2, ts_rent_tbls2)

prop_test<-map2(toc_rent_overall2[["cs"]][["original_5"]],
                toc_rent_overall2[["ts"]][["original_5"]],
                function(x,y){
                  create_prop_tbl(x,y)
                })
                
prop_test<-c(list(overall_prop_test), prop_test)
names(prop_test)[1]<-c("overall")


## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(neighborhood_t_test,combined_t_test,
     t_test_neighborhood_veteran, combined_t_test_veteran, overall_t_test_df,
     full_sample_test,prop_test,
     file = file.path(code_file_dir, "test_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------