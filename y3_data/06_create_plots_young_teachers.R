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

# load(file.path(code_file_dir, "rent_tbls.RData"))
# 
#load(file.path(code_file_dir, "toc_rent_tbls.RData"))

load(file.path(code_file_dir, "veteran_rent_tbls.RData"))

load(file.path(code_file_dir, "rent_plots_toc.RData"))

## -----------------------------------------------------------------------------
## Part 1.1 - Create CS/TS Combined Tables 
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
## Part 2.1 - Create CS/TS Plots Combined
## -----------------------------------------------------------------------------

#create plot tables
plot_combined_tbls<-map(combined_tbls,
                        function(b) map(b,
                        function(a) map(a,create_plot_tbl)))


#create bar plots - combined
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


## -----------------------------------------------------------------------------
## Part 2.1 - Create CS/TS Plots by Neighborhood & Veteran Status
## -----------------------------------------------------------------------------

#create plot tables
neigh_plot_tbls_veteran<-map(toc_neigh_rent_tbls,
           function(type) map(type,
           function(a) map(a,create_plot_tbl2)))


#create bar plots
neigh_bar_plot_tbls<-map(neigh_plot_tbls_veteran,
           function(type) map2(type,names(type),
                               function(df2, year){
                                 map2(df2,names(df2),
                                      function(df1, neigh_name){
                                        create_bar_plot1(df1,
                                                         str_c(str_to_title(str_replace_all(neigh_name, "_", " ")),
                                                               " TOC Retention: ",
                                                               str_to_title(year)))
                                      })
                               })) 

#create only CS school version
cs_neigh_plot_tbls_veteran<-map(neigh_plot_tbls_veteran,
          function(type){
            map(type,
                function(veteran_status){
                  map(veteran_status,
                      function(neighborhood){
                        neighborhood %>% 
                          filter(School == "CS")
                      })})})

#update cs_neigh_plot_tbls_veteran table

update_tbl<-function(df){
  if(nrow(df) == 0){
    update_df<-data.frame(school = rep("NA",5),
                          Year = as.character(c(2019:2023)),
                          Retention = rep(0,5),
                          School = rep("CS",5))
    
  }
  if(nrow(df) != 0){
    update_df<-df
    
  }
  return(update_df)
}


cs_neigh_plot_tbls_veteran<-map(cs_neigh_plot_tbls_veteran,
          function(type){
            map(type,
                function(status){
                  map(status,update_tbl)
                })})
  
#update names for cs_neigh_plot_tbls_veteran

format_strings <- function(strings) {
  strings <- gsub("_", " ", strings)                        # Replace underscores with spaces
  strings <- gsub("\\b([a-z])", "\\U\\1", strings, perl = TRUE) # Capitalize each word
  strings <- gsub("Mid City", "Mid-City", strings)          # Add hyphen for Mid-City
  strings <- gsub("\\bLa\\b", "LA", strings)                # Ensure "LA" is uppercase
  strings <- gsub("\\bMacarthur Park\\b", "MacArthur Park", strings) # Fix MacArthur Park capitalization
  strings <- gsub("\\bHeet\\b", "HEET", strings)            # Ensure HEET is uppercase
  return(strings)
}

sch_string<-format_strings(names(cs_neigh_plot_tbls_veteran[["original_5"]][["0-3 years"]]))

update_names<-function(df_list, sch_names){
  
  update_df_list<-df_list
  names(update_df_list)<-sch_names
  return(update_df_list)
}

cs_neigh_plot_tbls_veteran<-map(cs_neigh_plot_tbls_veteran,
          function(sch_type){
            map(sch_type,
                function(status){
                  update_names(status,sch_string)
                })
          })


cs_only_plots<-map(cs_neigh_plot_tbls_veteran,
                   function(type){
                     map2(type,
                          str_to_title(names(type)),
                          function(a,b){
                            map2(a,names(a),
                                 function(x,y){
                                   create_bar_plot_cs(x,
                                                      str_c("2019-2023 Retention: ", y,
                                                            ", Teacher Status: ", b))
                                 })})})



## -----------------------------------------------------------------------------
## Part 2.2 - Create CS/TS Plots by for combined Years
## -----------------------------------------------------------------------------


#filter by group and only keep the last 2 years

create_cs_plots_y1_y2<-function(df, sch_type, y1, y2, plot_title){
  
  df_update<-df %>% filter(School == sch_type)
  
  df_update$Year<-as.numeric(df_update$Year) %>% round()
  df_update<-df_update %>% filter(Year %in% c(y1:y2))
  df_update$Year<-as.character(df_update$Year)
  
  df_plot<-create_bar_plot_cs(df_update, plot_title)
  
  return(df_plot)
  
}

cs_overall_22_23<-map(plot_combined_tbls,
                      function(sch_type){
                        map(sch_type,
                            function(type){
                              map2(type,
                                   names(type),
                                   function(x,y){
                                     create_cs_plots_y1_y2(x, "CS", 2022, 2023,
                                                           str_c("2022-2023 Retention: Overall, Teacher Status: ", 
                                                                 y))}
                              )})})



cs_overall_all_yrs<-map(plot_combined_tbls,
                        function(sch_type){
                          map(sch_type,
                              function(type){
                                map2(type,
                                     names(type),
                                     function(x,y){
                                       create_cs_plots_y1_y2(x, "CS", 2019, 2023,
                                                             str_c("2019-2023 Retention: Overall, Teacher Status: ",
                                                                   str_to_title(y)))}
                                )})})


#by neigborhood

cs_only_plots_22_23_neigh<-map(cs_neigh_plot_tbls_veteran,
                   function(type){
                     map2(type,
                          str_to_title(names(type)),
                          function(a,b){
                            
                            map2(a,names(a),
                                 function(x,y){
                                   create_cs_plots_y1_y2(x,"CS",2022,2023,
                                                      str_c("2022-2023 Retention: ", y,
                                                            ", Teacher Status: ", b))
                                 })})})


#overall neighborhood plots

#update plot_neigh_tbls_toc

names(plot_neigh_tbls_toc)<-sch_string


cs_overall_22_23_neigh_plots<-map2(plot_neigh_tbls_toc,
           names(plot_neigh_tbls_toc),
          function(neigh, sch_name){
            create_cs_plots_y1_y2(neigh,"CS",2022,2023,
                                  str_c("2022-2023 CS Overall Retention: ",
                                        sch_name))  
            
          })
  
cs_overall_all_yrs_neigh_plots<-map2(plot_neigh_tbls_toc,
                               names(plot_neigh_tbls_toc),
                               function(neigh, sch_name){
                                 create_cs_plots_y1_y2(neigh,"CS",2019,2023,
                                                       str_c("2019-2023 CS Overall Retention: ",
                                                             sch_name))  
                                 
                               })  





#overall and all years and all schools

teach_status<-c(": All Years",": 0-5 Years",": 6-10 Years",
                ": 11-15 Years",": 15+ Years")
type_sch<-c("All", "Elementary", "Middle/High")


cs_only_overall_plots<-vector("list",2)
names(cs_only_overall_plots)<-c("all years", "22-23")

cs_only_overall_plots[["all years"]]<-
  map2(toc_plot_combined_tbls,type_sch,
       function(df,sch_type){
         map2(df,
              teach_status,
              function(x,y){
                create_cs_plots_y1_y2(x,"CS", 2019, 2023,
                                      str_c("2019-2023 Retention: ",
                                            sch_type,
                                            " Schools, Teacher Veteran Status", y))
                })})

cs_only_overall_plots[["22-23"]]<-
  map2(toc_plot_combined_tbls,type_sch,
       function(df,sch_type){
         map2(df,
              teach_status,
              function(x,y){
                create_cs_plots_y1_y2(x,"CS", 2022, 2023,
                                      str_c("2022-2023 Retention: ",
                                            sch_type,
                                            " Schools, Teacher Veteran Status", y))
              })})

## -----------------------------------------------------------------------------
## Part 3 - Save Data
## -----------------------------------------------------------------------------

save(combined_tbls, toc_neigh_rent_tbls,
     plot_combined_tbls,bar_plots_combined,
     neigh_plot_tbls_veteran,neigh_bar_plot_tbls, cs_only_plots,
     cs_overall_22_23,cs_overall_all_yrs,cs_only_plots_22_23_neigh,
     cs_overall_22_23_neigh_plots, cs_overall_all_yrs_neigh_plots,
     cs_only_overall_plots,
     file = file.path(code_file_dir,"veteran_plots.RData"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------