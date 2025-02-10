################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 08.2_create_plots_vet_teachers.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 2/6/25 >
##
################################################################################

#Goal: Create Plots of the percentage of veteran teachers in a school

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
                         "Analyses","cs_teach_rent2", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")

data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

create_combined_tbl<-function(df_cs, df_ts){
  
  #created combined overall table
  
  cs<-df_cs %>% filter(vet_status == "Percent of Veteran Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of Veteran Teachers" ~ "CS Percent of Veteran Teachers")) 
  
  ts<-df_ts %>% filter(vet_status == "Percent of Veteran Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of Veteran Teachers" ~ "TS Percent of Veteran Teachers")) 
  
  df_update<-rbind(cs, ts) %>% select(-c(vet_status))
  
  return(df_update)
}

create_plot_tbl <- function(df) {
  
  data_long <- pivot_longer(df, 
                            cols = starts_with("yr_"), 
                            names_to = "Year", 
                            values_to = "Percent") %>% 
    mutate(
      School = case_when(
        school == "CS Percent of Veteran Teachers" ~ "CS",
        school == "TS Percent of Veteran Teachers" ~ "TS",
        ),
  Year = case_when(
    Year == "yr_18_19" ~ "2019",
    Year == "yr_19_20" ~ "2020",
    Year == "yr_20_21" ~ "2021",
    Year == "yr_21_22" ~ "2022",
    Year == "yr_22_23" ~ "2023",
    Year == "yr_23_24" ~ "2024",
     )
  )
  
  return(data_long)
}

create_plot_tbl2 <- function(df) {
  
  if(is.null(colnames(df[["cs"]]))){
    
    cs_data_long<-data.frame(school = "a",
                             Year = "a",
                             Retention = "a",
                             School = "a")
  }
  
  if(!is.null(colnames(df[["cs"]]))){
    cs_data_long <- pivot_longer(df[["cs"]], 
                                 cols = c("2019","2020","2021","2022","2023"), 
                                 names_to = "Year", 
                                 values_to = "Retention") %>% 
      mutate(
        School = case_when(
          school == "CS Percent of Veteran Teachers" ~ "CS",
        )) %>% filter(School == "CS")
  }

      
  if(is.null(colnames(df[["ts"]]))){
    
    
    ts_data_long<-data.frame(school = "a",
                             Year = "a",
                             Retention = "a",
                             School = "a")
  }
  
  else{
    ts_data_long <- pivot_longer(df[["ts"]], 
                                 cols = c("2019","2020","2021","2022","2023"), 
                                 names_to = "Year", 
                                 values_to = "Retention") %>% 
      mutate(
        School = case_when(
          school == "TS Percent of Veteran Teachers" ~ "TS",
        )) %>% filter(School == "TS")
    
  }

  data_long<-rbind(cs_data_long, ts_data_long)
  
  #filter out any invalid "a" values
  data_long<-data_long %>% filter(school != "a")
  data_long<-data_long %>% mutate(
    Retention = as.numeric(Retention)
  )
  
  return(data_long)
}

create_bar_plot1<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "TS" = "#F2A900")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

create_bar_plot2<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Retention, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Retention, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Retention Rate (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "TS" = "#F2A900",
                                 "Overall" = "gray")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

create_bar_plot_cs<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Retention, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Retention, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Retention Rate (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0, 100)

  return(bar_graph)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "vet_combined_tbls.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Create CS/TS Combined Tables 
## -----------------------------------------------------------------------------

sch_type_string<-c("all","elem","mid_hi")
sch_type_string2<-c("All School","Elementary School", "Middle/High School")

combined_plot_tbls<-map(vet_combined_tbls,
                        function(version){
                          sch_type_df<-map(sch_type_string, function(sch_type){
                            
                            map2(version[["overall"]][["cs"]][[sch_type]],
                                 version[["overall"]][["ts"]][[sch_type]],
                                 function(cs_sample, ts_sample){
                                   map2(cs_sample,ts_sample,
                                        function(cs_teach,ts_teach){
                                          map2(cs_teach[["percent"]],
                                               ts_teach[["percent"]],
                                               function(x,y) create_combined_tbl(x,y))
                                        })
                                 })
                            
                          })
                          
                          names(sch_type_df)<-sch_type_string
                          return(sch_type_df)
                        })
  
## -----------------------------------------------------------------------------
## Part 2 - Transform Tables into Long Versions
## -----------------------------------------------------------------------------

long_plot_tbls<-map(combined_plot_tbls,
                    function(version){
                      map(version,
                          function(sch_type){
                            map(sch_type,
                                function(sample){
                                  map(sample,
                                      function(teach_type){
                                        map(teach_type,create_plot_tbl)
                                      })})})})
  
## -----------------------------------------------------------------------------
## Part 3 - Plot Tables
## -----------------------------------------------------------------------------

plots<-map(long_plot_tbls,
                    function(version){
                      map2(version, sch_type_string2,
                          function(sch_type, sch_type_name){
                            map2(sch_type,names(sch_type),
                                function(sample, person_type_name){
                                  map2(sample, names(sample),
                                      function(teach_type, sample_name){
                                        map2(teach_type,names(teach_type),
                    function(x,y) create_bar_plot1(x,
                  str_c("Percentage of ",sch_type_name," ",
                        str_to_title(person_type_name),": ",
                        str_to_title(sample_name),", ",
                        str_to_title(y))))
                        })})})})

plots[["version3"]][["all"]][["staff"]][["color"]][["10+ years"]]

## -----------------------------------------------------------------------------
## Part 4 - Save Data
## -----------------------------------------------------------------------------

save(plots,file = file.path(code_file_dir,"veteran_percent_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------