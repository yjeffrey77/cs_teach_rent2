################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 09_create_lausd_teacher_tbls.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 2/19/25 >
##
################################################################################

#Goal: Create the LAUSD tables for the Y1-Y3 Teacher Reports

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(tabulizer)
library(readxl)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

pdf_folder<-file.path("..", "HR Veteran Data")

## ---------------------------
## file directories
## ---------------------------

code_file_dir<-file.path("C:/Users/yjeff/Desktop/Community Schools/Teacher Retention Data",
                         "Analyses", "cs_teach_rent2", "y3_data")

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

safe_function<-function(expr, version){
  result<- tryCatch(
    {
      expr
    },
    error = function(e){
      
      return(NA)
      
    }
    
  )
  return(result)
}

## ---------------------------
## load & inspect data
## ---------------------------

## -----------------------------------------------------------------------------
## Part 1 - Read in LAUSD Data
## -----------------------------------------------------------------------------

lausd_data<-lapply(1:6,
             function(i) read_excel(file.path("..", "HR Veteran Data",
                                              "hr_demo_data.xlsx"),
                                    sheet = i)
             )
names(lausd_data)<-c("tv_2020_21", "td_2020_21",
                     "tv_2021_22", "td_2021_22",
                     "tv_2022_23", "td_2022_23")

## -----------------------------------------------------------------------------
## Part 2 - Create Calculated Teacher of Color
## -----------------------------------------------------------------------------

#create toc dataframe
create_toc_df<-function(df){
  
  df_update<-df %>% clean_names() %>% 
    filter(ethnic_origin != "Totals")
  
  total_df<-df %>% clean_names() %>% 
    filter(ethnic_origin == "Totals")
  
  df_update<-df_update %>% mutate(
    toc = case_when(ethnic_origin %in% c("White", "Undeclared") ~ "Not BIPOC",
                    TRUE ~  "BIPOC")
  )
  
  #Create teacher of color (percentages)
  
  test2<-df_update %>% group_by(toc) %>% summarize(
    n_elementary = sum(elementary),
    p_elementary = sum(elementary)/total_df$elementary,
    n_secondary = sum(secondary),
    p_secondary = sum(secondary)/total_df$secondary,
    n_special_education = sum(special_education),
    p_special_education = sum(special_education)/total_df$special_education,
    n_resource_specialist = sum(resource_specialist),
    p_resource_specialist = sum(resource_specialist)/total_df$resource_specialist,
    n_total = sum(totals),
    p_total = sum(totals)/total_df$totals
  )
  
  return(test2)
  
}

toc_df_list<-map(list(lausd_data[["td_2020_21"]],
                      lausd_data[["td_2021_22"]],
                      lausd_data[["td_2022_23"]]),
                 create_toc_df)

names(toc_df_list)<-c("td_2020_21", "td_2021_22", "td_2022_23")


#test<-lausd_data[["tv_2021_22"]] %>% clean_names()

create_vet_df<-function(df){
  
  df_update<-df %>% clean_names() 
  
  # df_update<-df %>% clean_names() %>% 
  #   filter(job  %in% c("Elementary", "Secondary", "Special Ed"))
  
  total_by_year<-df_update %>%  
    filter(job == "Total")
  
  total_by_job<-df_update %>% select(total)
  
  
  df_update<-df_update %>% mutate(
    p_1 = x1/total,
    p_2 = x2/total,
    p_3 = x3/total,
    p_4 = x4/total,
    p_5 = x5/total,
    p_6 = x6/total,
    p_7 = x7/total,
    p_8 = x8/total,
    p_9 = x9/total,
    p_10 = x10/total,
  ) %>% select(job,p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10)
  
  return(df_update)
  
}

vet_df_list<-map(list(lausd_data[["tv_2020_21"]],
                      lausd_data[["tv_2021_22"]],
                      lausd_data[["tv_2022_23"]]),
                 create_vet_df)

names(vet_df_list)<-c("tv_2020_21", "tv_2021_22", "tv_2022_23")


multiply_toc_data<-function(df, vet_multiplier){
  
 df_update<-df %>% mutate(
   n_elementary = n_elementary * vet_multiplier[1],
   p_elementary = p_elementary * vet_multiplier[1],
   n_secondary = n_secondary * vet_multiplier[2],
   p_secondary = p_secondary * vet_multiplier[2],
   n_special_education = n_special_education * vet_multiplier[3],
   p_special_education = p_special_education * vet_multiplier[3],
   n_total = n_total * vet_multiplier[4],
   p_total = p_total * vet_multiplier[4],
 ) %>% select(-c(n_resource_specialist, p_resource_specialist)) 
  
 df_update<-df_update %>% 
   mutate(
     n_elementary = round(n_elementary,0),
     p_elementary = round(p_elementary*100, 0),
     n_secondary = round(n_secondary, 0),
     p_secondary = round(p_secondary * 100,0),
     n_special_education = round(n_special_education,0),
     p_special_education = round(p_special_education * 100,0),
     n_total = round(n_total,0),
     p_total = round(p_total * 100, 0)
   )
 
 df_update_t<-data.frame(t(df_update))
 colnames(df_update_t)<-df_update$toc
 df_update_t<-df_update_t[-1,]
# df_update_t<-data.frame(df_update_t)
 
  return(df_update_t)
}



bipoc_calculated_lausd_data<-map2(toc_df_list,vet_df_list,
                                  function(demo, vet){
                                    multiply_toc_data(demo,vet[["p_10"]])
                                  })
names(bipoc_calculated_lausd_data)<-c("2020-21", "2021-22", "2022-23")  

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(lausd_data,bipoc_calculated_lausd_data,
     file = file.path(code_file_dir, "lausd_vet_data.RData"))

## -----------------------------------------------------------------------------
## Part ) - Install Tabulizer
## -----------------------------------------------------------------------------

#Taken from this site: https://blog.djnavarro.net/posts/2023-06-16_tabulizer/

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))


## -----------------------------------------------------------------------------
## Part 1 - Extract Tables
## -----------------------------------------------------------------------------


# set Java memory limit to 600 MB (optional)
options(java.parameters = "-Xmx600m")
f <- system.file(pdf_folder, "2020-2021 TDemo.pdf", package = "tabulapdf")
# extract table from first page of example PDF
tab <- extract_tables(f, pages = 6)
tab[[1]]


## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(vet_combined_tbls,vet_tbls,
     file = file.path(code_file_dir, "vet_combined_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------