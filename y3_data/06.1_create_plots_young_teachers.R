################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 06_create_plots_young_teachers.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 10/16/24 >
##
################################################################################

#Goal: Create Plots for veteran teachers

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
  
  cs<-df_cs %>% filter(rent == "Overall Retention Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate %" ~ "CS Overall Retention Rate")) 
  
  ts<-df_ts %>% filter(rent == "Overall Retention Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Retention Rate %" ~ "TS Overall Retention Rate")) 
  
  df_update<-rbind(cs, ts) %>% select(-c(rent))
  
  return(df_update)
}

create_plot_tbl <- function(df) {
  
  data_long <- pivot_longer(df, 
                            cols = starts_with("yr_"), 
                            names_to = "Year", 
                            values_to = "Retention") %>% 
    mutate(
      School = case_when(
        school == "CS Overall Retention Rate" ~ "CS",
        school == "TS Overall Retention Rate" ~ "TS",
        school == "Combined Overall Retention Rate" ~ "Overall",
        ),
      Year = case_when(
        Year == "yr_2019" ~ "2019",
        Year == "yr_2020" ~ "2020",
        Year == "yr_2021" ~ "2021",
        Year == "yr_2022" ~ "2022",
        Year == "yr_2023" ~ "2023",
      ))
  
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
          school == "Overall Retention Rate (%)" ~ "CS",
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
          school == "Overall Retention Rate (%)" ~ "TS",
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

create_plot_tbl3 <- function(df) {
  
  df<-df %>% filter(school == "Overall Retention Rate %")
  colnames(df)[3:7]<-c(2019:2023)
  
  if(is.null(colnames(df))){

    cs_data_long<-data.frame(school = "a",
                             Year = "a",
                             Retention = "a",
                             School = "a")
  }

  if(!is.null(colnames(df))){
    cs_data_long <- pivot_longer(df,
                                 cols = c("2019","2020","2021",
                                          "2022","2023"),
                                 names_to = "Year",
                                 values_to = "Retention") %>%
      mutate(
        School = case_when(
          school == "Overall Retention Rate %" ~ "CS",
        )) %>% filter(School == "CS")
  }

  data_long<-rbind(cs_data_long)
  
  #filter out any invalid "a" values
  data_long<-data_long %>% filter(school != "a")
  
  data_long$Retention <- as.numeric(gsub("%", "", data_long$Retention))
  
  
  return(data_long)
}


create_bar_plot1<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Retention, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Retention, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Retention Rate (%)") +
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

load(file.path(code_file_dir, "combined_cohort_rent_tbls.RData"))


## -----------------------------------------------------------------------------
## Part 1.2 - Create Neighborhood Tables by Veteran Status 
## -----------------------------------------------------------------------------

#Get neighborhood string list
source(file.path(".","y3_data", "00_school_lists.R"))

#create functions
create_rent_tbl<-function(sch_string, df_list){
  
  sch_list<-df_list[sch_string]
  
  # Remove NULL values
  sch_list <- sch_list[!is.na(names(sch_list))]
  #sch_list <- Filter(Negate(is.null), sch_string)
  
  #filter by retention rate
  update_sch_list<-map(sch_list, function(a){
    update_df<-a %>% filter(rent == "Retention Rate %") %>% 
      mutate(
        yr_2019 = as.numeric(yr_2019),
        yr_2020 = as.numeric(yr_2020),
        yr_2021 = as.numeric(yr_2021),
        yr_2022 = as.numeric(yr_2022),
        yr_2023 = as.numeric(yr_2023),
      ) %>% select(-c(rent))
    
    return(update_df)
  } )
  
  #rowbind for them to come together
  update_sch_df<-bind_rows(update_sch_list, .id = "school")
  
  if(nrow(update_sch_df)==0){
    update_sch_df<-NA
  }
  
  else{

    overall<- update_sch_df %>% summarize(
      school = "Overall Retention Rate (%)",
      yr_2019 = mean(yr_2019, na.rm = T) %>% round(),
      yr_2020 = mean(yr_2020, na.rm = T) %>% round(),
      yr_2021 = mean(yr_2021, na.rm = T) %>% round(),
      yr_2022 = mean(yr_2022, na.rm = T) %>% round(),
      yr_2023 = mean(yr_2023, na.rm = T) %>% round(),
    )
    
    update_sch_df<-rbind(update_sch_df, overall)
    
    colnames(update_sch_df)<-c("school", "2019", "2020", "2021", "2022", "2023")
    
  }
  return(update_sch_df)
}

create_neighborhood_list<-function(neighborhood_string,
                                   cs_sch_string, df_list){
  #create strings
  cs_neighborhood_string<-cs_sch_string[cs_sch_string %in% neighborhood_string]
  ts_neighborhood_string<-neighborhood_string[!c(neighborhood_string %in% cs_neighborhood_string)]
  
  #create rent datasets
  cs_tbls<-create_rent_tbl(cs_neighborhood_string,df_list)
  ts_tbls<-create_rent_tbl(ts_neighborhood_string,df_list)
  
  neigh_tbls<-list(cs_tbls, ts_tbls)
  names(neigh_tbls)<-c("cs", "ts")
  
  return(neigh_tbls)
}

create_specific_strings<-function(neighborhood_string,
                                  cs_sch_string){
  #create strings
  cs_neighborhood_string_cs<-cs_sch_string[cs_sch_string %in% neighborhood_string]
  ts_neighborhood_string_cs<-neighborhood_string[!c(neighborhood_string %in% cs_neighborhood_string_cs)]
  
  return(list(cs_neighborhood_string_cs,
              ts_neighborhood_string_cs))
}

#create tables
toc_neigh_rent_tbls<-map(hr_color_by_sch_rent_exp,
                         function(type){
                           map(type,
                               function(veteran_status){
                                 map(neighborhood_string_list,
                                     function(neighborhood){
                                       create_neighborhood_list(neighborhood,cs_string,
                                                                veteran_status)})
                               })})

## -----------------------------------------------------------------------------
## Part 2.1 - Create CS Plot Tables
## -----------------------------------------------------------------------------

#combined overall
combined_overall_plot_tbls<-map(combined_overall_rent_tbls,create_plot_tbl3)

#overall
overall_plot_tbls<-map(cohort_overall_v2, create_plot_tbl3)

#By Cohort
cohort_plot_tbls<-map(cohort_overall_teach_exp_v2,
          function(cohort){
            map(cohort,
                function(teach_yrs){
                  create_plot_tbl3(teach_yrs)
                })})
  
## -----------------------------------------------------------------------------
## Part 2.2 - Create Bar Graphs
## -----------------------------------------------------------------------------

create_cs_plots_y1_y2<-function(df, sch_type, y1, y2, plot_title){
  
  df_update<-df %>% filter(School == sch_type)
  
  df_update$Year<-as.numeric(df_update$Year) %>% round()
  df_update<-df_update %>% filter(Year %in% c(y1:y2))
  df_update$Year<-as.character(df_update$Year)
  
  df_plot<-create_bar_plot_cs(df_update, plot_title)
  
  return(df_plot)
  
}

#combined overall 
combined_overall_bp<-vector("list", 2)
names(combined_overall_bp)<-c("22_23", "all_yrs")

combined_overall_bp[["22_23"]]<-
  map2(combined_overall_plot_tbls,
       str_to_title(names(combined_overall_plot_tbls)),
       function(x,y){
         create_cs_plots_y1_y2(x,
                               "CS", 2022, 2023,
                               str_c("2022-2023 Retention: Overall, Teacher Status: ", y))
         
       })
  
combined_overall_bp[["all_yrs"]]<-
  map2(combined_overall_plot_tbls,
       str_to_title(names(combined_overall_plot_tbls)),
       function(x,y){
         create_cs_plots_y1_y2(x,
                               "CS", 2019, 2023,
                               str_c("2019-2023 Retention: Overall, Teacher Status: ", y))
         
       })  
  
#overall  
overall_bp<-vector("list", 2)
names(overall_bp)<-c("22_23", "all_yrs")
  
overall_bp[["22_23"]]<-
  map2(overall_plot_tbls,
       str_to_title(gsub("_", " ", names(overall_plot_tbls))),
       function(x,y){
            create_cs_plots_y1_y2(x,
                                  "CS", 2022, 2023,
                                  str_c("2022-2023 Overall Retention: ",y))
          })

overall_bp[["all_yrs"]]<-
  map2(overall_plot_tbls,
       str_to_title(gsub("_", " ", names(overall_plot_tbls))),
       function(x,y){
         create_cs_plots_y1_y2(x,
                               "CS", 2019, 2023,
                               str_c("2019-2023 Overall Retention: ",y))
       })

#by cohort
cohort_bp<-vector("list", 2)
names(cohort_bp)<-c("22_23", "all_yrs")


cohort_bp[["22_23"]]<-map2(cohort_plot_tbls,
           str_to_title(gsub("_", " ", names(cohort_plot_tbls))),
           function(a, cohort){
             map2(a,
                  str_to_title(names(a)),
                  function(x,teach_yr){
                    create_cs_plots_y1_y2(x,
                                          "CS", 2022, 2023,
                                          str_c("2022-2023 Overall Retention: ",
                                                cohort, ", ", teach_yr))
                  })})
  
cohort_bp[["all_yrs"]]<-map2(cohort_plot_tbls,
                           str_to_title(gsub("_", " ", names(cohort_plot_tbls))),
                           function(a, cohort){
                             map2(a,
                                  str_to_title(names(a)),
                                  function(x,teach_yr){
                                    create_cs_plots_y1_y2(x,
                                                          "CS", 2019, 2023,
                                                          str_c("2019-2023 Overall Retention: ",
                                                                cohort, ", ", teach_yr))
                                  })})  
  
## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(combined_overall_bp, overall_bp, cohort_bp,
     file = file.path(code_file_dir,"by_cohort_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------