################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 08.4_create_plots_vet_teachers_update.R >
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

create_cohort_tbl<-function(df){
  cs<-df %>% filter(vet_status == "Percent of Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of Teachers" ~ "CS Percent of Veteran Teachers")) 
  cs<-cs %>% select(-c(vet_status))
  return(cs)
}

create_combined_tbl<-function(df_cs, df_ts){
  
  #created combined overall table
  
  cs<-df_cs %>% filter(vet_status == "Percent of Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of Teachers" ~ "CS Percent of Veteran Teachers")) 
  
  ts<-df_ts %>% filter(vet_status == "Percent of Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of Teachers" ~ "TS Percent of Veteran Teachers")) 
  
  df_update<-rbind(cs, ts) %>% select(-c(vet_status))
  
  return(df_update)
}

create_combined_tbl_toc<-function(df_cs, df_ts){
  
  #created combined overall table
  
  cs<-df_cs %>% filter(vet_status == "Percent of TOC Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of TOC Teachers" ~ "CS Percent of TOC Teachers")) 
  
  ts<-df_ts %>% filter(vet_status == "Percent of TOC Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of TOC Teachers" ~ "TS Percent of TOC Teachers")) 
  
  df_update<-rbind(cs, ts) %>% select(-c(vet_status))
  
  return(df_update)
}

create_combined_tbl_toc_cohort<-function(df_cs){
  
  #created combined overall table
  
  cs<-df_cs %>% filter(vet_status == "Percent of TOC Teachers") %>% 
    mutate(school = case_when(
      school == "Percent of TOC Teachers" ~ "CS Percent of TOC Teachers")) 
  
  # ts<-df_ts %>% filter(vet_status == "Percent of TOC Teachers") %>% 
  #   mutate(school = case_when(
  #     school == "Percent of TOC Teachers" ~ "TS Percent of TOC Teachers")) 
  
  df_update<-rbind(cs) %>% select(-c(vet_status))
  
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


create_plot_tbl_cohort <- function(df) {
  
  data_long <- pivot_longer(df, 
                            cols = starts_with("yr_"), 
                            names_to = "Year", 
                            values_to = "Percent") %>% 
    mutate(
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
  
  df<-df %>% mutate(
    School =case_when(school=="CS Percent of TOC Teachers" ~ "CS",
                      school=="TS Percent of TOC Teachers" ~ "TS"
  ))
  
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

create_bar_plot_cs_cohort<-function(df, graph_title){
  
  df<-df %>% mutate(
    School =case_when(school=="CS Percent of TOC Teachers" ~ "CS",
    ))
  
  bar_graph<-ggplot(df, aes(x = Year, y = Percent, fill = School)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +  # Places the text above the bars
    labs(title = graph_title, x = "Year",
         y = "Teacher Percentage (%)") +
    scale_fill_manual(values = c("CS" = "#2D68C4")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(bar_graph)
}

## ---------------------------
## load & inspect data
## ---------------------------

load(file.path(code_file_dir, "vet_combined_tbls.RData"))
load(file.path(code_file_dir, "demo_combined_tbls.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Create CS/TS Combined Tables 
## -----------------------------------------------------------------------------

sch_type_string<-c("all","elem","mid_hi")
sch_type_string2<-c("All School","Elementary School", "Middle/High School")

#combine plot tables
combined_plot_tbls<-vector("list", 2)
names(combined_plot_tbls)<-c("original", "cohort")

combined_plot_tbls[["original"]]<-map(vet_combined_tbls, function(version){
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
combined_plot_tbls[["cohort"]]<-map(vet_combined_tbls_cohort,
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
#demo version
create_demo_plot_tbls<-function(df_list){
  
  update_df_list<-map(sch_type_string, function(sch_type){
    
    map2(df_list[["overall"]][["cs"]][[sch_type]],
         df_list[["overall"]][["ts"]][[sch_type]],
         function(cs_sample, ts_sample){
           map2(cs_sample,ts_sample,
                function(cs_teach,ts_teach){
                  map2(cs_teach[["percent"]],
                       ts_teach[["percent"]],
                       function(x,y) create_combined_tbl_toc(x,y))
                })
         })
    
  })
  names(update_df_list)<-sch_type_string  
  
  return(update_df_list)
}

demo_combined_plot_tbls<-vector("list", 4)
names(demo_combined_plot_tbls)<-c("original_vet", "original_sch",
                                  "cohort_vet", "cohort_sch")

demo_combined_plot_tbls[["original_vet"]]<-create_demo_plot_tbls(demo_combined_tbls)
demo_combined_plot_tbls[["original_sch"]]<-create_demo_plot_tbls(demo_combined_tbls2)
demo_combined_plot_tbls[["cohort_vet"]]<-create_demo_plot_tbls(demo_combined_tbls_cohort)
demo_combined_plot_tbls[["cohort_sch"]]<-create_demo_plot_tbls(demo_combined_tbls_cohort2)

#create demo CS only cohort values
create_cohort_demo_tbl<-function(df_list){
  update_df_list<-map(df_list[["cohort"]],
            function(cohort){
              map(cohort,
                  function(ppl_type){
                    map(ppl_type[["all"]][["percent"]],
                        create_combined_tbl_toc_cohort)
                  })})
  return(update_df_list)
}

#key original_vet - Shows all years with percentages of BIPOC teachers by vet status
#original_sch - shows all years with percentages of BIPOC veteran teacher for sch overall
#cohort_vet - shows only years cohort joined the LAUSD CSI
#cohort_sch - shows only years cohort joined the LAUSD CSI

demo_cohort_plot_tbls<-vector("list", 4)
names(demo_cohort_plot_tbls)<-c("original_vet", "original_sch",
                                "cohort_vet", "cohort_sch")

demo_cohort_plot_tbls[["original_vet"]]<-create_cohort_demo_tbl(demo_combined_tbls)
demo_cohort_plot_tbls[["original_sch"]]<-create_cohort_demo_tbl(demo_combined_tbls2)
demo_cohort_plot_tbls[["cohort_vet"]]<-create_cohort_demo_tbl(demo_combined_tbls_cohort)
demo_cohort_plot_tbls[["cohort_sch"]]<-create_cohort_demo_tbl(demo_combined_tbls_cohort2)

## -----------------------------------------------------------------------------
## Part 2 - Transform Tables into Long Versions
## -----------------------------------------------------------------------------

long_plot_tbls<-map(combined_plot_tbls,
                    function(sample_type){
                      map(sample_type,
                          function(version){
                            map(version,
                                function(sch_type){
                                  map(sch_type,
                                      function(sample){
                                        map(sample,
                                            function(teach_type){
                                              map(teach_type,create_plot_tbl)
                                            })})})})})

long_plot_tbls_demo<-map(demo_combined_plot_tbls,
                         function(sample_type){
                           map(sample_type,
                               function(sch_type){
                                 map(sch_type,
                                     function(sample){
                                       map(sample,
                                           function(teach_type){
                                             map(teach_type,create_plot_tbl)
                                           })})})})

long_plot_tbls_demo_cohort<-map(demo_cohort_plot_tbls,
                                function(version){
                                  map(version,
                                      function(cohort){
                                        map(cohort,
                                            function(ppl_type){
                                              map(ppl_type,
                                                  create_plot_tbl_cohort)
                                            })})})
  
## -----------------------------------------------------------------------------
## Part 3 - Plot Tables
## -----------------------------------------------------------------------------

plots<-map(long_plot_tbls,
          function(version_large){
            map(version_large,
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
                                     })})})})})
  
plots[["original"]][["version3"]][["all"]][["staff"]][["color"]][["10+ years"]]
  
plots_demo<-map(long_plot_tbls_demo,
                function(version){
                  map2(version, sch_type_string2,
                       function(sch_type, sch_type_name){
                         map2(sch_type,names(sch_type),
                              function(sample, person_type_name){
                                map2(sample, names(sample),
                                     function(teach_type, sample_name){
                                       map2(teach_type,names(teach_type),
                                            function(x,y) create_bar_plot_cs(x,
                                    str_c("Percentage of ",sch_type_name," ",
                                    str_to_title(person_type_name),": ",
                                    str_to_title(sample_name),", ",
                                    str_to_title(y))))
                                     })})})})

plots_demo[["original_sch"]][["all"]][["teachers"]][["all"]][["10+ years"]]

plots_demo[["original_vet"]][["all"]][["teachers"]][["all"]][["10+ years"]]


long_plot_tbls_demo[["original_vet"]][["all"]][["teachers"]][["all"]][["0-3 years"]]


plot_tbls_demo_cohort<-map(long_plot_tbls_demo_cohort,
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

plot_tbls_demo_cohort[["cohort_sch"]][["cohort3"]][["teachers"]][["10+ years"]]

## -----------------------------------------------------------------------------
## Part 4 - Save Data
## -----------------------------------------------------------------------------

save(plots,plots_demo, plot_tbls_demo_cohort,
     file = file.path(code_file_dir,"veteran_percent_plots.RData"))

save(long_plot_tbls,long_plot_tbls_demo, long_plot_tbls_demo_cohort,
     file = file.path(code_file_dir,"veteran_plot_tbls.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------