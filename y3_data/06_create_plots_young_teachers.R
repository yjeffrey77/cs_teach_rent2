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

## ---------------------------
## load & inspect data
## ---------------------------

# load(file.path(code_file_dir, "rent_tbls.RData"))
# 
# load(file.path(code_file_dir, "toc_rent_tbls.RData"))

load(file.path(code_file_dir, "veteran_rent_tbls.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Create CS/TS Combined Tables 
## -----------------------------------------------------------------------------

combined_tbls<-vector("list", 3)
names(combined_tbls)<-c("overall", "elem", "ms_hs")

#CS/TS Overall

combined_tbls[["overall"]]<-map2(toc_rent_overall[["cs"]],
                                 toc_rent_overall[["ts"]],
                                 function(a,b) map2(a,b,
                                 function(x,y) create_combined_tbl(x,y))) 

#CS/TS Elementary
combined_tbls[["elem"]]<-map2(toc_rent_overall_elem[["cs"]],
                              toc_rent_overall_elem[["ts"]],
                              function(a,b) map2(a,b,
                              function(x,y) create_combined_tbl(x,y)))  

#CS/TS Middle/High School
combined_tbls[["ms_hs"]]<-map2(toc_rent_overall_ms_hs[["cs"]],
                               toc_rent_overall_ms_hs[["ts"]],
                               function(a,b) map2(a,b,
                               function(x,y) create_combined_tbl(x,y))) 


## -----------------------------------------------------------------------------
## Part 2 - Create CS/TS Plots
## -----------------------------------------------------------------------------

#create plot tables
plot_combined_tbls<-map(combined_tbls,
                        function(b) map(b,
                        function(a) map(a,create_plot_tbl)))


#create bar plots
bar_plots_combined<-vector("list", 3)
names(bar_plots_combined)<-c("overall", "elem", "ms_hs")

bar_plots_combined[["overall"]]<-map(plot_combined_tbls[["overall"]],
          function(a) map2(a,names(a),
           function(x,y) create_bar_plot1(x,
          str_c("2019-2023 Overall TOC Retention: ",y))))


bar_plots_combined[["elem"]]<-map(plot_combined_tbls[["elem"]],
                                     function(a) map2(a,names(a),
                                    function(x,y) create_bar_plot1(x,
                                    str_c("2019-2023 Elementary TOC Retention: ",
                                          y))))

bar_plots_combined[["ms_hs"]]<-map(plot_combined_tbls[["ms_hs"]],
                                  function(a) map2(a,names(a),
                                  function(x,y) create_bar_plot1(x,
                                  str_c("2019-2023 Middle/High TOC Retention: ",
                                        y))))

#Save For now
save(plot_combined_tbls,combined_tbls,bar_plots_combined,
     file = file.path(code_file_dir,"veteran_plots.RData"))




#Stop Here




#TOC
toc_plot_combined_tbls<-vector("list", 3)
names(toc_plot_combined_tbls)<-c("overall", "elem", "ms_hs")

#create plot tables

for (a in names(toc_plot_combined_tbls)){
  toc_plot_combined_tbls[[a]]<-
    map(toc_combined_tbls[[a]], create_plot_tbl)
}

#create bar plots
toc_bar_plots_combined<-vector("list", 3)
names(toc_bar_plots_combined)<-c("overall", "elem", "ms_hs")

for (teach_type in names(toc_combined_tbls)){
  toc_bar_plots_combined[[teach_type]]<-vector("list", 5)
  names(toc_bar_plots_combined[[teach_type]])<-teach_year_string
}

toc_bar_plots_combined[["overall"]][["overall"]]<-
  create_bar_plot1(toc_plot_combined_tbls[["overall"]][["overall"]],
                   "2019-2023 TOC Retention Rate: Overall")

for (a in c("0-5 years","6-10 years","11-15 years","15+ years")){
  
  toc_bar_plots_combined[["overall"]][[a]]<-
    create_bar_plot1(toc_plot_combined_tbls[["overall"]][[a]],
                     str_c("2019-2023 TOC Retention Rate: ", a))
}

#Elementary and High School

for (b in c("elem", "ms_hs")){
  
  if (b == "elem"){c = "Elementary"}
  
  if (b == "ms_hs"){c = "Middle/High School"}
  
  toc_bar_plots_combined[[b]][["overall"]]<-
    create_bar_plot1(toc_plot_combined_tbls[["overall"]][["overall"]],
                     str_c("2019-2023 TOC ", c,
                           " Retention Rate: Overall"))
  
  for (a in c("0-5 years","6-10 years","11-15 years","15+ years")){
    
    toc_bar_plots_combined[[b]][[a]]<-
      create_bar_plot1(toc_plot_combined_tbls[["overall"]][[a]],
                       str_c("2019-2023 TOC ",c,
                             " Retention Rate: ", a))
  }
  
}


## -----------------------------------------------------------------------------
## Part 2 - Neighborhood Combined Tables & Plots
## -----------------------------------------------------------------------------

plot_neigh_tbls<-map(names(rent_by_neighborhood),
                              function(x) create_plot_tbl(rent_by_neighborhood[[x]][["combined_overall"]]))
names(plot_neigh_tbls)<-names(rent_by_neighborhood)

#create function to transform string
transform_neighborhood_string <- function(string) {
  string %>%
    str_replace_all("_", " ") %>%  # Replace underscores with spaces
    str_to_title() %>%             # Capitalize each word
    str_replace_all("\\bLa\\b", "LA") %>%   # Capitalize "la" as "LA"
    str_replace_all("\\bMid City\\b", "Mid-City")
}

update_neigh_string<-transform_neighborhood_string(names(plot_neigh_tbls))

#create list to store bar plots
#bar_plots_neigh<-vector("list", length(update_neigh_string))

bar_plots_neigh<-map2(plot_neigh_tbls, update_neigh_string,
                      function(x,y) create_bar_plot2(x,
                                                     str_c("2019-2023 Retention Rate: ",
                                                           y)))

#TOC
plot_neigh_tbls_toc<-map(names(rent_by_neighborhood_toc),
                     function(x) create_plot_tbl(rent_by_neighborhood_toc[[x]][["combined_overall"]]))
names(plot_neigh_tbls_toc)<-names(rent_by_neighborhood_toc)


update_neigh_string<-transform_neighborhood_string(names(plot_neigh_tbls_toc))

#create list to store bar plots
#bar_plots_neigh<-vector("list", length(update_neigh_string))

toc_bar_plots_neigh<-map2(plot_neigh_tbls_toc, update_neigh_string,
                      function(x,y) create_bar_plot2(x,
                                                     str_c("2019-2023 TOC Retention Rate: ",
                                                           y)))

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

# save(combined_tbls,plot_combined_tbls,bar_plots_combined,
#      plot_neigh_tbls, bar_plots_neigh, update_neigh_string,
#      file = file.path(code_file_dir, "rent_plots.RData"))
# 
# 
# save(toc_combined_tbls,toc_plot_combined_tbls,toc_bar_plots_combined,
#      plot_neigh_tbls_toc, toc_bar_plots_neigh, update_neigh_string,
#      file = file.path(code_file_dir, "rent_plots_toc.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------