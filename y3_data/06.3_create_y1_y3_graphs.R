################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 06.3_create_y1_y3_graphs.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 12/24/24 >
##
################################################################################

#Goal: Creates graphs of all the schools in the Y1 to Y3 study.


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

#toc tables
load(file.path(code_file_dir, "toc_rent_tbls.RData"))
load(file.path(code_file_dir, "rent_plots_toc.RData"))
load(file.path(code_file_dir,  "veteran_rent_tbls.RData"))
load(file.path(code_file_dir, "hr_by_sch.RData"))

#Keep Only necessary Dataframes and directories
keep<-c("hr_color_by_sch_rent_exp","toc_rent_tbls",
        "code_file_dir", "data_file_dir", "data_file_dir_yr2")

rm(list = setdiff(ls(), keep))

## -----------------------------------------------------------------------------
## Part 1 - Add Functions
## -----------------------------------------------------------------------------

#add School list
source(file.path(code_file_dir, "00_school_lists.R"))

## -----------------------------------------------------------------------------
## Part 2 - Extract the Case Study School Tables
## -----------------------------------------------------------------------------

#extract school
extract_school<-function(school_name){
  
  sch<-vector("list", 6)
  names(sch)<-c("overall",
                 names(hr_color_by_sch_rent_exp[["original_5"]]))
  
  sch[["overall"]]<-toc_rent_tbls[["overall"]][[school_name]]
  sch[["0-3 years"]]<-hr_color_by_sch_rent_exp[["original_5"]][["0-3 years"]][[school_name]]
  sch[["4-5 years"]]<-hr_color_by_sch_rent_exp[["original_5"]][["4-5 years"]][[school_name]]
  sch[["6-10 years"]]<-hr_color_by_sch_rent_exp[["original_5"]][["6-10 years"]][[school_name]]
  sch[["11-15 years"]]<-hr_color_by_sch_rent_exp[["original_5"]][["11-15 years"]][[school_name]]
  sch[["15+ years"]]<-hr_color_by_sch_rent_exp[["original_5"]][["15+ years"]][[school_name]]
  
  return(sch)
}

#school list
schools<-vector("list", 3)
case_study_schools<-c("Miramonte Elementary",
                      "Felicitas And Gonzalo Mendez Senior High",
                      "Ellen Ochoa Learning Center")
names(schools)<-case_study_schools

for (sch_name in case_study_schools){
  
  schools[[sch_name]]<-extract_school(sch_name)
  
}

## -----------------------------------------------------------------------------
## Part 3 - Create Plot Tables
## -----------------------------------------------------------------------------

#keep 
transform_tbl<-function(df, sch_name){
  
  df_update<-df %>% filter(rent == "Retention Rate %")
  
  df_update<-pivot_longer(df_update, c(yr_2019, yr_2020,yr_2021,
                               yr_2022,yr_2023),
                      names_to = "year",
                      values_to = "retention")
  
  df_update<-df_update %>% mutate(school = sch_name)
  return(df_update)
  
}

school_plot_tbl_list<-map2(schools,
                           case_study_schools,
                           function(sch,sch_name){
                             map(sch,
                                 function(teach_status){
                                   transform_tbl(teach_status,
                                                 sch_name)
                                 })})
  
#row bind plot tables together
merge_plot_tbls<-function(tbl_type){
  
merge_tbl<-rbind(school_plot_tbl_list[["Miramonte Elementary"]][[tbl_type]],
                 school_plot_tbl_list[["Felicitas And Gonzalo Mendez Senior High"]][[tbl_type]],
                 school_plot_tbl_list[["Ellen Ochoa Learning Center"]][[tbl_type]])
#update columns
merge_tbl<-merge_tbl %>% 
  mutate(
    year = case_when(
      year == "yr_2019" ~ 2019,
      year == "yr_2020" ~ 2020,
      year == "yr_2021" ~ 2021,
      year == "yr_2022" ~ 2022,
      year == "yr_2023" ~ 2023,
      TRUE ~ NA
    ),
    retention = as.numeric(retention)
  ) %>% select(-c(rent))
 
return(merge_tbl) 
}

y1_y3_plot_tbls<-map(names(school_plot_tbl_list[["Miramonte Elementary"]]),
          merge_plot_tbls)
  
names(y1_y3_plot_tbls)<-names(school_plot_tbl_list[["Miramonte Elementary"]])

#y1_y3 plot tables version 2 (School A - C)

hide_school_names<-function(df){
  df_update<-df %>% mutate(
    school = case_when(
      school == "Miramonte Elementary" ~ "School A",
      school == "Felicitas And Gonzalo Mendez Senior High" ~ "School B",
      school == "Ellen Ochoa Learning Center" ~ "School c",
    )
  )
  return(df_update)
}

y1_y3_plot_tbls_v2<-map(y1_y3_plot_tbls, hide_school_names)
  
## -----------------------------------------------------------------------------
## Part 4 - Create Plots
## -----------------------------------------------------------------------------

library(ggplot2)

create_y1_y3_plot<-function(df, teach_type){
  
  # Create the line graph
cs_plot<-ggplot(df,
         aes(x = year, y = retention, color = school, group = school)) +
    geom_line(size = 1) +            # Add lines
    geom_point(size = 2) +           # Add points at each data point
    labs(
      title = str_c("Retention Rates in Profiled Community Schools: ",
                    str_to_title(teach_type)),
      x = "Year",
      y = "Retention (%)",
      color = "School"
    ) +
    theme_minimal() +                # Use a clean theme
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the title
      legend.position = "bottom"               # Move the legend below the plot
    )
  
return(cs_plot)  
}

y1_y3_plots<-map2(y1_y3_plot_tbls,
                  names(y1_y3_plot_tbls),
                  function(x,y){create_y1_y3_plot(x,y)})

y1_y3_plots_v2<-map2(y1_y3_plot_tbls_v2,
                  names(y1_y3_plot_tbls_v2),
                  function(x,y){create_y1_y3_plot(x,y)})

## -----------------------------------------------------------------------------
## Part 5 - Create Combined Retention Tables
## -----------------------------------------------------------------------------

create_combined_rent_tbls<-function(df){
  
  # Use pivot_wider to reshape
  df_wide <- pivot_wider(
    data = df,
    names_from = year,        # Columns will be created from the "year" values
    values_from = retention   # Fill values will come from the "retention" column
  )
  colnames(df_wide)[1]<-"School"
  return(df_wide)
}

combined_rent_tbls<-map(y1_y3_plot_tbls, create_combined_rent_tbls)

combined_rent_tbls_v2<-map(y1_y3_plot_tbls_v2, create_combined_rent_tbls)
  
## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(schools,case_study_schools,y1_y3_plot_tbls,y1_y3_plot_tbls_v2,
     y1_y3_plots, y1_y3_plots_v2, combined_rent_tbls,
     combined_rent_tbls_v2,
file = file.path(code_file_dir,"y1_y3_plots_and_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------