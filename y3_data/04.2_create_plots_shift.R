################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 04.1_create_plots_shift.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 5/24/25 >
##
################################################################################

#Goal: Create Plots for shifting

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
                         "Analyses","cs_teach_rent2",  "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")


data_file_dir_yr2<-file.path("C:/Users/yjeff/Box/LAUSD TR Data")

## ---------------------------
## helper functions
## ---------------------------

create_combined_tbl<-function(df_cs, df_ts){
  
  #created combined overall table
  
  cs<-df_cs %>% filter(shift == "Overall Shifting Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Shifting Rate %" ~ "CS Overall Shifting Rate")) 
  
  ts<-df_ts %>% filter(shift == "Overall Shifting Rate %") %>% 
    mutate(school = case_when(
      school == "Overall Shifting Rate %" ~ "TS Overall Shifting Rate")) 
  
  df_update<-rbind(cs, ts) %>% select(-c(shift))
  
  return(df_update)
}

create_plot_tbl <- function(df) {
  
  data_long <- pivot_longer(df, 
                            cols = starts_with("yr_"), 
                            names_to = "Year", 
                            values_to = "Shifting") %>% 
    mutate(
      School = case_when(
        school == "CS Overall Shifting Rate" ~ "CS",
        school == "TS Overall Shifting Rate" ~ "TS",
        school == "Combined Overall Shifting Rate" ~ "Overall",
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
  
  bar_graph<-ggplot(df, aes(x = Year, y = Shifting, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Shifting, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Shifting Rate (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "TS" = "#F2A900")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

create_bar_plot2<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Shifting, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Shifting, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Shifting Rate (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "TS" = "#F2A900",
                                 "Overall" = "gray")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "shift_tbls.RData"))

load(file.path(code_file_dir, "toc_shift_tbls.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Create CS/TS Combined Tables 
## -----------------------------------------------------------------------------

combined_tbls<-vector("list", 3)
names(combined_tbls)<-c("overall", "elem", "ms_hs")

#CS/TS Overall
combined_tbls[["overall"]]<-create_combined_tbl(cs_shift_tbls,ts_shift_tbls) 

#CS/TS Elementary
combined_tbls[["elem"]]<-create_combined_tbl(cs_elem_shift_tbls,ts_elem_shift_tbls) 

#CS/TS Middle/High School
combined_tbls[["ms_hs"]]<-create_combined_tbl(cs_ms_hs_shift_tbls,ts_ms_hs_shift_tbls) 


#TOC List

teach_year_string<-names(toc_shift_overall[["cs"]])
toc_combined_tbls<-vector("list", 3)
names(toc_combined_tbls)<-c("overall", "elem", "ms_hs")


for (teach_type in names(toc_combined_tbls)){
  toc_combined_tbls[[teach_type]]<-vector("list", 5)
  names(toc_combined_tbls[[teach_type]])<-teach_year_string
}


for (teach in teach_year_string){
  toc_combined_tbls[["overall"]][[teach]]<-
    create_combined_tbl(toc_shift_overall[["cs"]][[teach]],
                        toc_shift_overall[["ts"]][[teach]])
}

for (teach in teach_year_string){
  toc_combined_tbls[["elem"]][[teach]]<-
    create_combined_tbl(toc_shift_overall_elem[["cs"]][[teach]],
                        toc_shift_overall_elem[["ts"]][[teach]])
}

for (teach in teach_year_string){
  toc_combined_tbls[["ms_hs"]][[teach]]<-
    create_combined_tbl(toc_shift_overall_ms_hs[["cs"]][[teach]],
                        toc_shift_overall_ms_hs[["ts"]][[teach]])
}

## -----------------------------------------------------------------------------
## Part 2 - Create CS/TS Plots
## -----------------------------------------------------------------------------

#create plot tables
plot_combined_tbls<-map(combined_tbls, create_plot_tbl)

#create bar plots
bar_plots_combined<-vector("list", 3)
names(bar_plots_combined)<-c("overall", "elem", "ms_hs")

bar_plots_combined[["overall"]]<-
  create_bar_plot1(plot_combined_tbls[["overall"]],
                   "2019-2023 Shifting Rate: Overall")

bar_plots_combined[["elem"]]<-
  create_bar_plot1(plot_combined_tbls[["elem"]],
                   "2019-2023 Shifting Rate: Elementary Schools")

bar_plots_combined[["ms_hs"]]<-
  create_bar_plot1(plot_combined_tbls[["ms_hs"]],
                   "2019-2023 Shifting Rate: Middle/High Schools")


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
                   "2019-2023 TOC Shifting Rate: Overall")

for (a in c("0-5 years","6-10 years","11-15 years","15+ years")){
  
  toc_bar_plots_combined[["overall"]][[a]]<-
    create_bar_plot1(toc_plot_combined_tbls[["overall"]][[a]],
                     str_c("2019-2023 TOC Shifting Rate: ", a))
}

#Elementary and High School

for (b in c("elem", "ms_hs")){
  
  if (b == "elem"){c = "Elementary"}
  
  if (b == "ms_hs"){c = "Middle/High School"}
  
  toc_bar_plots_combined[[b]][["overall"]]<-
    create_bar_plot1(toc_plot_combined_tbls[["overall"]][["overall"]],
                     str_c("2019-2023 TOC ", c,
                           " Shifting Rate: Overall"))
  
  for (a in c("0-5 years","6-10 years","11-15 years","15+ years")){
    
    toc_bar_plots_combined[[b]][[a]]<-
      create_bar_plot1(toc_plot_combined_tbls[["overall"]][[a]],
                       str_c("2019-2023 TOC ",c,
                             " Shifting Rate: ", a))
  }
  
}

## -----------------------------------------------------------------------------
## Part 2 - Neighborhood Combined Tables & Plots
## -----------------------------------------------------------------------------
plot_neigh_tbls<-map(names(shift_by_neighborhood),
                     function(x) create_plot_tbl(shift_by_neighborhood[[x]][["combined_overall"]]))
names(plot_neigh_tbls)<-names(shift_by_neighborhood)

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
                                                     str_c("2019-2023 Shifting Rate: ",
                                                           y)))

#TOC
plot_neigh_tbls_toc<-map(names(shift_by_neighborhood_toc),
                         function(x) create_plot_tbl(shift_by_neighborhood_toc[[x]][["combined_overall"]]))
names(plot_neigh_tbls_toc)<-names(shift_by_neighborhood_toc)


update_neigh_string<-transform_neighborhood_string(names(plot_neigh_tbls_toc))

#create list to store bar plots
#bar_plots_neigh<-vector("list", length(update_neigh_string))

toc_bar_plots_neigh<-map2(plot_neigh_tbls_toc, update_neigh_string,
                          function(x,y) create_bar_plot2(x,
                                                         str_c("2019-2023 TOC Shifting Rate: ",
                                                               y)))

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(combined_tbls,plot_combined_tbls,bar_plots_combined,
     plot_neigh_tbls, bar_plots_neigh, update_neigh_string,
     file = file.path(code_file_dir, "shift_plots.RData"))

save(toc_combined_tbls,toc_plot_combined_tbls,toc_bar_plots_combined,
     plot_neigh_tbls_toc, toc_bar_plots_neigh, update_neigh_string,
     file = file.path(code_file_dir,  "shift_plots_toc.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------