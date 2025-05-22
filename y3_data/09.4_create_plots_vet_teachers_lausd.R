################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 09.4_create_plots_vet_teachers_lausd.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 3/25/25 >
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

#Note: LAUSD represents District
## ---------------------------
## helper functions
## ---------------------------

#bar plots
create_bar_plot1 <- function(df, graph_title) {
  
  bar_graph <- ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", 
                                 "TS" = "#F2A900",
                                 "District" = "#4CAF50")) +  # Added color for LAUSD
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
  
  bar_graph<-ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "TS" = "#F2A900",
                                 "District" = "#4CAF50")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))

  return(bar_graph)
}

create_bar_plot_cs_no_ts<-function(df, graph_title){
  
  bar_graph<-ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4",
                                 "District" = "#4CAF50")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

create_bar_plot_cs_cohort<-function(df, graph_title){

  df<-df %>% 
    mutate(Percent = case_when(
      Percent == 0 ~ NA,
      TRUE ~ Percent
    ))
  
  bar_graph<-ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4", "District" = "#4CAF50")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "veteran_percent_plots.RData"))
load(file.path(code_file_dir, "veteran_plot_tbls.RData"))
load(file.path(code_file_dir, "lausd_vet_data.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Create Long Versions of LAUSD BIPOC Veteran Tables
## -----------------------------------------------------------------------------

#overall
lausd_vet_perc_overall<-list(lausd_data[["tv_2020_21"]],
                             lausd_data[["tv_2021_22"]],
                             lausd_data[["tv_2022_23"]],
                             lausd_data[["tv_2023_24"]],
                             lausd_data[["tv_2024_25"]])

names(lausd_vet_perc_overall)<-c("2020_21","2021_22","2022_23","2023_24",
                                 "2024_25")

extract_lausd_vet_data_overall<-function(df, fit_criteria){
  df_update<-df %>% clean_names()
  
  df_update<-df_update %>% 
    mutate(x10 = round(((x10/total)*100),0)) 
  
  df_update<-df_update %>% filter(job == fit_criteria) %>% select(year, x10)
  
  #make variables to follow long tables 
  df_update<-df_update %>% mutate(
    school = "District", #District is LAUSD
    Year = case_when(
      year == "2020_21" ~ "2021",
      year == "2021_22" ~ "2022", 
      year == "2022_23" ~ "2023", 
      year == "2023_24" ~ "2024", 
      year == "2024_25" ~ "2025"),
    Percent = x10
    )
  
  #filter data
  df_update<-df_update %>% filter(year != "2025")
  
  df_update<-df_update %>% mutate(School = school)
  
  df_update<-df_update %>% select(school, Year, Percent, School)
    
  return(df_update)
}

extract_lausd_vet_data<-function(df, fit_criteria){
  df_update<-df %>% clean_names()
  df_update<-df_update %>% filter(job == fit_criteria) %>% select(year, x10)
  
  #make variables to follow long tables 
  df_update<-df_update %>% mutate(
    school = "District",
    Year = case_when(
      year == "2020_21" ~ "2021",
      year == "2021_22" ~ "2022", 
      year == "2022_23" ~ "2023", 
      year == "2023_24" ~ "2024", 
      year == "2024_25" ~ "2025"),
    Percent = x10
  )
  
  #filter data
  df_update<-df_update %>% filter(year != "2025")
  
  df_update<-df_update %>% mutate(School = school)
  
  df_update<-df_update %>% select(school, Year, Percent, School)
  
  df_update<-df_update %>% mutate(Percent = round(Percent))
  
  return(df_update)
}

sch_string<-c("Elementary", "Secondary", "Total")
lausd_vet_perc_overall_tbl<-map(sch_string,
                                function(type){
                                  bind_rows(lausd_vet_perc_overall, .id = "year") %>% 
                                    extract_lausd_vet_data_overall(type)
                                })

names(lausd_vet_perc_overall_tbl)<-sch_string  
  
#BIPOC Derived
lausd_vet_perc_bipoc_tbl<-map(sch_string,
                              function(type){
                                bind_rows(perc_tbl_bipoc, .id = "year") %>% 
                                  extract_lausd_vet_data(type)
                              })

names(lausd_vet_perc_bipoc_tbl)<-sch_string   

lausd_tbls<-list(lausd_vet_perc_overall_tbl, lausd_vet_perc_bipoc_tbl)
names(lausd_tbls)<-c("overall", "bipoc")

## -----------------------------------------------------------------------------
## Part 2.1 - Transform update_long_plot_tbls
## -----------------------------------------------------------------------------

#flatten data frame
update_long_plot_tbls<-map(long_plot_tbls,
          function(type){
            type_df<-map(type[["version4"]],
                function(sch_type){
                  sch_type_df<-map(sch_type,
                                   function(ppl_type){
                                     ppl_df<-map(ppl_type,
                                                 function(teach_sample){
                                                   vet_update<-teach_sample %>% bind_rows(.id = "vet_status")
                                                   return(vet_update)
                                                 }) %>% bind_rows(.id = "teach_sample")
                                     return(ppl_df)
                                   }
                  ) %>% bind_rows(.id = "ppl_type")
                  
                  return(sch_type_df)
                }) %>% bind_rows(.id = "sch_type")
           return(type_df) 
          }) %>% bind_rows(.id = "type_df")

make_list<-function(df_list){
  
  # Split by group first, then category, then sub_category
  update_df_list <- split(df_list, df_list$type_df)   # First level (group)
  update_df_list <- lapply(update_df_list, function(x) split(x, x$sch_type))  # Second level (category)
  update_df_list <- lapply(update_df_list, function(x) lapply(x, function(y) split(y, y$ppl_type)))  # Third level (sub_category)
  update_df_list <- lapply(update_df_list,function(x) lapply(x,function(y) lapply(y, function(z) split(z, z$teach_sample))))  # Third level (sub_category)
  update_df_list <- lapply(update_df_list,
                                  function(x) 
                                    lapply(x,function(y) 
                                      lapply(y, function(z) 
                                        lapply(z, function(a) split(a, a$vet_status)))))  # Third level (sub_category)
  
  #keep only "school" "Year" "Percent" "School"
  update_df_list<-map(update_df_list,
                             function(type){
                               map(type,
                                   function(sch_type){
                                     map(sch_type,
                                         function(ppl_type){
                                           map(ppl_type,
                                               function(sample_type){
                                                 map(sample_type,
                                                     function(vet_status){
                                                       vet_status %>% 
                                                         select(c("school","Year", "Percent","School"))
                                                     })})})})})
  
  return(update_df_list)
}

#keep only teachers of 10+ years, version 4 only  
update_long_plot_tbls<-update_long_plot_tbls %>% 
  filter(vet_status == "10+ years") %>% 
  filter(ppl_type == "teachers")

update_long_plot_tbls<-make_list(update_long_plot_tbls)

#bind rows  
add_lausd_rows<-function(df_list, overall_df, bipoc_df){
  
  update_df_list<-df_list
  
  update_df_list[["all"]][["10+ years"]]<-
    rbind(update_df_list[["all"]][["10+ years"]],overall_df) %>% 
    filter(Year != "2025")
  
  update_df_list[["color"]][["10+ years"]]<-
    rbind(update_df_list[["color"]][["10+ years"]],bipoc_df) %>% 
    filter(Year != "2025")
  
  return(update_df_list)
}

#add lausd rows to list at once
add_lausd_across_grades<-function(df_list, district_tbls){
  
  df_list[["all"]][["teachers"]]<-add_lausd_rows(df_list[["all"]][["teachers"]],
                                                 district_tbls[["overall"]][["Total"]],
                                                 district_tbls[["bipoc"]][["Total"]])
  
  df_list[["elem"]][["teachers"]]<-add_lausd_rows(df_list[["elem"]][["teachers"]],
                                                 district_tbls[["overall"]][["Elementary"]],
                                                 district_tbls[["bipoc"]][["Elementary"]])
  
  df_list[["mid_hi"]][["teachers"]]<-add_lausd_rows(df_list[["mid_hi"]][["teachers"]],
                                                  district_tbls[["overall"]][["Secondary"]],
                                                  district_tbls[["bipoc"]][["Secondary"]])
  return(df_list)
}

#update update_long_plot_tbls
update_long_plot_tbls<-map(update_long_plot_tbls,
                           function(x) add_lausd_across_grades(x,lausd_tbls))

## -----------------------------------------------------------------------------
## Part 2.2 - Transform update_long_plot_tbls_demo
## -----------------------------------------------------------------------------

#bind rows
add_lausd_rows_demo<-function(df_list, lausd_df){
  
  update_df_list<-df_list
  
  update_df_list[["all"]][["10+ years"]]<-
    rbind(update_df_list[["all"]][["10+ years"]],lausd_df) %>% 
    filter(Year != "2025")
  
  return(update_df_list)
}

#add lausd rows to list at once
add_lausd_across_grades_demo<-function(df_list, district_tbls){
  
  df_list[["all"]][["teachers"]]<-add_lausd_rows_demo(df_list[["all"]][["teachers"]],
                                                 district_tbls[["bipoc"]][["Total"]])
  
  df_list[["elem"]][["teachers"]]<-add_lausd_rows_demo(df_list[["elem"]][["teachers"]],
                                                  district_tbls[["bipoc"]][["Elementary"]])
  
  df_list[["mid_hi"]][["teachers"]]<-add_lausd_rows_demo(df_list[["mid_hi"]][["teachers"]],
                                                   district_tbls[["bipoc"]][["Secondary"]])
  return(df_list)
}

#flatten data frame
flatten_toc_list<-function(df_list){
  
  update_df_list<-map(df_list,
                      function(type){
                        type_df<-map(type,
                                     function(sch_type){
                                       sch_type_df<-map(sch_type,
                                                        function(ppl_type){
                                                          ppl_df<-map(ppl_type,
                                                                      function(teach_sample){
                                                                        vet_update<-teach_sample %>% bind_rows(.id = "vet_status")
                                                                        return(vet_update)
                                                                                  }) %>% bind_rows(.id = "teach_sample")
                                                                      return(ppl_df)
                                                                    }
                                                   ) %>% bind_rows(.id = "ppl_type")
                                                   
                                                   return(sch_type_df)
                                                 }) %>% bind_rows(.id = "sch_type")
                                    return(type_df) 
                                  }) %>% bind_rows(.id = "type_df")
  return(update_df_list)
}

update_long_plot_tbls_demo<-flatten_toc_list(long_plot_tbls_demo)

#Filter and clean values
update_long_plot_tbls_demo<-update_long_plot_tbls_demo %>% 
  filter(ppl_type == "teachers" & vet_status == "10+ years")

update_long_plot_tbls_demo<-update_long_plot_tbls_demo %>% 
  mutate(School = case_when(
    school == "CS Percent of TOC Teachers" ~ "CS",
    school == "TS Percent of TOC Teachers" ~ "TS"
  ))

update_long_plot_tbls_demo_cs<-update_long_plot_tbls_demo %>% 
  mutate(School = case_when(
    school == "CS Percent of TOC Teachers" ~ "CS",
    school == "TS Percent of TOC Teachers" ~ "TS"
  )) %>% filter(School != "TS")
  
#CS, TS, LAUSD Version
#make into list
update_long_plot_tbls_demo<-make_list(update_long_plot_tbls_demo)

#update update_long_plot_tbls_demo
update_long_plot_tbls_demo<-map(update_long_plot_tbls_demo,
                           function(x) add_lausd_across_grades_demo(x,
                                                                    lausd_tbls))

#CS and LAUSD Version Only
update_long_plot_tbls_demo_cs<-make_list(update_long_plot_tbls_demo_cs)


#update update_long_plot_tbls_demo
update_long_plot_tbls_demo_cs<-map(update_long_plot_tbls_demo_cs,
                                function(x) add_lausd_across_grades_demo(x,
                                                                         lausd_tbls))

## -----------------------------------------------------------------------------
## Part 2.4 - Transform update long_plot_tbls_demo_cohort
## -----------------------------------------------------------------------------

#flatten dataset
update_long_plot_tbls_demo_cohort<-flatten_toc_list(long_plot_tbls_demo_cohort)

#Filter and clean values
update_long_plot_tbls_demo_cohort<-update_long_plot_tbls_demo_cohort %>% 
  filter(ppl_type == "teachers" & teach_sample == "10+ years")

update_long_plot_tbls_demo_cohort<-update_long_plot_tbls_demo_cohort %>% 
  mutate(School = case_when(
    school == "CS Percent of TOC Teachers" ~ "CS",
    school == "TS Percent of TOC Teachers" ~ "TS"
  ))

#make into list
make_list2<-function(df_list){
  
  # Split by group first, then category, then sub_category
  update_df_list <- split(df_list, df_list$type_df)   # First level (group)
  update_df_list <- lapply(update_df_list, function(x) split(x, x$sch_type))  # Second level (category)
  update_df_list <- lapply(update_df_list, function(x) lapply(x, function(y) split(y, y$ppl_type)))  # Third level (sub_category)
  update_df_list <- lapply(update_df_list,function(x) lapply(x,function(y) lapply(y, function(z) split(z, z$teach_sample))))  # Third level (sub_category)

  #keep only "school" "Year" "Percent" "School"
  update_df_list<-map(update_df_list,
                      function(type){
                        map(type,
                            function(sch_type){
                              map(sch_type,
                                  function(ppl_type){
                                    map(ppl_type,
                                        function(sample_type){
                                          sample_type %>% 
                                                  select(c("school","Year", "Percent", "School"))
                                              })})})})
  
  return(update_df_list)
}

update_long_plot_tbls_demo_cohort<-make_list2(update_long_plot_tbls_demo_cohort)

#update update_long_plot_tbls_demo_cohort
#add lausd rows to list at once
add_lausd_across_grades_demo_c<-function(df_list, district_tbls){
  
  df_list_update<-rbind(df_list,district_tbls) %>% filter(Year != "2025")
  
  return(df_list_update)
}

#add lausd rows
update_long_plot_tbls_demo_cohort<-map(update_long_plot_tbls_demo_cohort,
          function(type){
            map(type,
                function(cohort){
                  map(cohort,
                      function(ppl_type){
                        map(ppl_type,
                            function(vet_status){
                              add_lausd_across_grades_demo_c(vet_status,
                                                             lausd_tbls[["bipoc"]][["Total"]])
                            })})})})
  
## -----------------------------------------------------------------------------
## Part 3 - Plot Tables
## -----------------------------------------------------------------------------

sch_type_string2<-c("all","elementary","secondary")

#Version: Among all teachers (or among all TOC teachers), what is the percentage
#breakup by veteran status. (DECIDE NOT TO USE IN GRAPHS)
update_plots<-map(update_long_plot_tbls,
                function(version){
                  map2(version, sch_type_string2,
                       function(sch_type, sch_type_name){
                         map2(sch_type,names(sch_type),
                              function(sample, person_type_name){
                                map2(sample, names(sample),
                                     function(teach_type, sample_name){
                                       map2(teach_type,names(teach_type),
                                            function(x,y) create_bar_plot1(x,
                                      str_c("Percentage of ",
                                            str_to_title(sch_type_name)," ",
                                      str_to_title(person_type_name),": ",
                                      str_to_title(sample_name),", ",
                                      str_to_title(y))))
                                     })})})})
  
update_plots[["original"]][["mid_hi"]][["teachers"]][["color"]][["10+ years"]]

#percentage of TOCs in a school. 
#Two types - vet version: focused on the percentage of vet TOCs within a vet status 
#school version: focused on the percentage of vet TOCs within a school
# My plan is to use the "school version"

update_plots_demo<-map(update_long_plot_tbls_demo,
                function(version){
                  map2(version, sch_type_string2,
                       function(sch_type, sch_type_name){
                         map2(sch_type,names(sch_type),
                              function(sample, person_type_name){
                                map2(sample, names(sample),
                                     function(teach_type, sample_name){
                                       map2(teach_type,names(teach_type),
                                            function(x,y) create_bar_plot_cs(x,
                                    str_c("Percentage of ",
                                          str_to_title(sch_type_name)," BIPOC ",
                                    str_to_title(person_type_name),": ",
                                    #str_to_title(sample_name),", ",
                                    str_to_title(y))))
                                     })})})})

update_plots_demo[["original_sch"]][["all"]][["teachers"]][["all"]][["10+ years"]]

#CS and LAUSD only version of update_plots_demo

update_plots_demo_cs<-map(update_long_plot_tbls_demo_cs,
                       function(version){
                         map2(version, sch_type_string2,
                              function(sch_type, sch_type_name){
                                map2(sch_type,names(sch_type),
                                     function(sample, person_type_name){
                                       map2(sample, names(sample),
                                            function(teach_type, sample_name){
                                              map2(teach_type,names(teach_type),
                                                   function(x,y) create_bar_plot_cs_no_ts(x,
                                                                                    str_c("Percentage of ",
                                                                                          str_to_title(sch_type_name)," BIPOC ",
                                                                                          str_to_title(person_type_name),": ",
                                                                                          #str_to_title(sample_name),", ",
                                                                                          str_to_title(y))))
                                            })})})})

update_plots_demo_cs[["original_sch"]][["elem"]][["teachers"]][["all"]][["10+ years"]]


#percentage of TOCs by cohort 
update_plot_tbls_demo_cohort<-map(update_long_plot_tbls_demo_cohort,
                           function(version){
                             map(version,
                                 function(cohort){
                                   map2(cohort,names(cohort),
                                       function(ppl_type, name_ppl_type){
                                         map2(ppl_type, names(ppl_type),
                                             function(vet_status, name_vet_status){
                                               vet_status %>% 
                                                 create_bar_plot_cs_cohort(
                                                   str_c("Percentage of BIPOC ",
                                                         str_to_title(name_ppl_type),": ",
                                                         str_to_title(name_vet_status))
                                                   )})})})})

update_plot_tbls_demo_cohort[["cohort_sch"]][["cohort2"]][["teachers"]][["10+ years"]]

## -----------------------------------------------------------------------------
## Part 4 - Save Data
## -----------------------------------------------------------------------------

save(update_plots,update_plots_demo, update_plot_tbls_demo_cohort,
     update_long_plot_tbls_demo_cs,update_plots_demo_cs,
     file = file.path(code_file_dir,"lasud_cs_veteran_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------