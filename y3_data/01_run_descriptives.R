################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 01_run_descriptives.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 6/17/24 >
##
################################################################################

#Goal: Explore 2024 LAUSD HR Data

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path("C:/Users/yjeff/Desktop/Community Schools/Teacher Retention Data",
                "Analyses", "y3_data")

data_file_dir<-file.path("C:/Users/yjeff/Box/LAUSD TR Data Year 3")

## ---------------------------
## helper functions
## ---------------------------

#source(file.path(code_file_dir, "functions.R"))

## ---------------------------
## load & inspect data
## ---------------------------

hr_data<-read_excel(file.path(data_file_dir,
                              "CoS_Employee_Retention_HR_Data_20240523.xlsx"))

hr_data_norm<-read_excel(file.path(data_file_dir,
                                   "CoS_Full_Certificated_Employee_Norm_Day_List_20240523.xlsx"))

#load(file.path(code_file_dir, "c1_plot_data_v2_1_11_24.RData"))

## -----------------------------------------------------------------------------
## Part 1 - Information About Each Dataset
## -----------------------------------------------------------------------------

#From Declan Chin:

#CoS_Employee_Retention_HR_Data - provides all the staff employed on Norm Day 
#2018 (18-19 school year) at any of the cohort schools you provided. 
#It then follows them over the next 5 years til the this years norm day, 
#so you can see if they left the district or moved to work in other roles/schools.

#CoS_Full_Certificated_Employee_Norm_Day_List - I clarified with HR and found 
#out that the first file only contained employees if they were present on Norm 
#Day 2018 (i.e., if they first began teaching in, for example, 2020-21, then 
#they would not be in that file.) Because we figured you need all employees at 
#the cohort schools for every year, we requested they also provide the full list 
#of certificated employees for each school year, which is what is in this file. 


## -----------------------------------------------------------------------------
## Part 2 - Explore Each Dataset
## -----------------------------------------------------------------------------

colnames(hr_data)
colnames(hr_data_norm)

#hr_data_norm - look at counts
for(i in 1:16){
  colnames(hr_data_norm)[i] %>% print()
  hr_data_norm %>% count(get(colnames(hr_data_norm)[i])) %>% print()
}

#Notes:
#- hr_data_norm is a long file.
# From Years 2018


## -----------------------------------------------------------------------------
## Part 3 - Create Descriptive Tables
## -----------------------------------------------------------------------------


hr_data_norm %>% 



add_plot_tbls_mira_mendez<-function(name){
  df_list<-vector('list',4)
  names(df_list)<-c("overall", "overall_toc", "comp", "comp_toc")
  df_list[["overall"]]<-plot_tbls_cs_overall[[name]]
  df_list[["overall_toc"]]<-plot_tbls_cs_overall_toc[[name]]
  df_list[["comp"]]<-plot_tbls[[name]]
  df_list[["comp_toc"]]<-plot_tbls_toc[[name]]
  
  return(df_list)
}

mendez<-add_plot_tbls_mira_mendez("Felicitas And Gonzalo Mendez Senior High")
miramonte<-add_plot_tbls_mira_mendez("Miramonte Elementary")


#Edit out the Comparison Schools names
#miramonte
miramonte[["comp"]]<-miramonte[["comp"]] %>% 
  mutate(
    type = case_when(
      type == "CS: Miramonte Elem" ~ 'Miramonte ES',
      type == "Comp: Hooper Ave Elem" ~ "Comparison School"
    )
  )

miramonte[["comp_toc"]]<-miramonte[["comp_toc"]] %>% 
  mutate(
    type = case_when(
      type == "CS: Miramonte Elem" ~ 'Miramonte ES',
      type == "Comp: Hooper Ave Elem" ~ "Comparison School"
    )
  )

#mendez
mendez[["comp"]]<-mendez[["comp"]] %>% 
  mutate(
    type = case_when(
      type == "CS: Mendez HS" ~ 'Mendez HS',
      type == "Comp: Roosevelt HS" ~ "Comparison School"
    )
  )

mendez[["comp_toc"]]<-mendez[["comp_toc"]] %>% 
  mutate(
    type = case_when(
      type == "CS: Mendez HS" ~ 'Mendez HS',
      type == "Comp: Roosevelt HS" ~ "Comparison School"
    )
  )

#Update Graphs

create_miramonte_mendez_plots<-function(df_list){
  
  df_list_plot<-vector("list",4)
  names(df_list_plot)<-c("overall", "overall_toc", "comp", "comp_toc")
  
  for (name in c("overall", "overall_toc", "comp", "comp_toc")){
    df_list_plot[[name]]<-
      ggplot(data=df_list[[name]],
             aes(x=start_year, y=retention, group=type)) +
      geom_line(aes(color=type))+
      geom_point(aes(color=type)) +
      labs(x = "Start Year",
           y = "Retention Data %") +
      scale_color_discrete(name = "School Type")+
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(df_list_plot)  
}

miramonte_plots<-create_miramonte_mendez_plots(miramonte)

miramonte[["overall_toc"]]$retention<-round(as.numeric(miramonte[["overall_toc"]]$retention))


miramonte_plots[["overall_toc"]]<- 
  ggplot(data=miramonte[["overall_toc"]],
         aes(x=start_year, y=retention, group=type)) +
  geom_line(aes(color=type))+
  geom_point(aes(color=type)) +
  labs(x = "Start Year",
       y = "Retention Data %") +
  scale_color_discrete(name = "School Type")+
  theme(plot.title = element_text(hjust = 0.5)) 


#View(miramonte[["overall_toc"]])
#print(miramonte_plots[["overall_toc"]])


mendez_plots<-create_miramonte_mendez_plots(mendez)

## -----------------------------------------------------------------------------
## Part 2.1 - Create Mendez Overall Graphs
## -----------------------------------------------------------------------------

add_overall_plot_tbls_mendez<-function(name){
  df_list<-vector('list',4)
  names(df_list)<-c("rent", "rent_toc", "shift", "shift_toc")
  df_list[["rent"]]<-overall_graph_tbl_sch_type[["hs"]][["overall_rent"]]
  df_list[["rent_toc"]]<-overall_graph_tbl_sch_type[["hs"]][["toc_rent"]]
  df_list[["sch_rent"]]<-plot_tbls_cs_overall[[name]]
  df_list[["sch_rent_toc"]]<-plot_tbls_cs_overall_toc[[name]]
  df_list[["shift"]]<-overall_graph_tbl_sch_type[["hs"]][["overall_shift"]]
  df_list[["shift_toc"]]<-overall_graph_tbl_sch_type[["hs"]][["toc_shift"]]
  df_list[["sch_shift"]]<-shift_hs[["cs"]][["overall"]][[name]]
  df_list[["sch_shift_toc"]]<-shift_hs[["cs"]][["toc"]][[name]]
  return(df_list)
}

#Note: Look at check plot_tbls_cs_overall_toc as something is wrong here.


mendez_o<-add_overall_plot_tbls_mendez("Felicitas And Gonzalo Mendez Senior High")

#Everyone
#create overall Mendez plot table
mendez_only<-mendez_o[["sch_rent"]] %>% filter(type == "Mendez HS")

#rbind to Overall Information
mendez_plot_tbl<-rbind(mendez_only, mendez_o[["rent"]])

#Just TOCs
mendez_only_toc<-mendez_o[["sch_rent_toc"]] %>% filter(type == "Mendez HS")

#rbind to Overall Information
mendez_plot_toc_tbl<-rbind(mendez_only_toc, mendez_o[["rent_toc"]])

#Shift
transform_shift_tbl<-function(df){
  df_update<-df %>%
    filter(type == "shifting rate (%)") %>% select(-c(type))
  
  df_update<-data.frame(t(df_update))
  df_update <- df_update %>% mutate(
    type = "Mendez HS",
    year = c("17-18","18-19","19-20","20-21","21-22","22-23")
  )
  
  colnames(df_update)<-c("retention","type","start_year")
  return(df_update)
}

#shift only
mendez_shift_only<-transform_shift_tbl(mendez_o[["sch_shift"]])
mendez_shift_only<-rbind(mendez_shift_only, overall_graph_tbl_shift)


mendez_shift_toc_only<-transform_shift_tbl(mendez_o[["sch_shift_toc"]])
mendez_shift_toc_only<-rbind(mendez_shift_toc_only,overall_graph_tbl_toc_shift)


#put table a list
mendez_tbl_list<-vector("list",4)
names(mendez_tbl_list)<-c("overall", "overall_toc", "shift", "shift_toc")
mendez_tbl_list[["overall"]]<-mendez_plot_tbl
mendez_tbl_list[["overall_toc"]]<-mendez_plot_toc_tbl
mendez_tbl_list[["shift"]]<-mendez_shift_only
mendez_tbl_list[["shift_toc"]]<-mendez_shift_toc_only

#Plot Graphs

create_mendez_plots<-function(df_list){
  
  df_list_plot<-vector("list",4)
  names(df_list_plot)<-c("overall", "overall_toc", "shift", "shift_toc")
  
  for (name in c("overall", "overall_toc","shift", "shift_toc")){
    
    if (name == "overall"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=retention, group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Retention Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("Retention Rate: Comparing Mendez to CS & Comparison HS") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "overall_toc"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=retention, group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Retention Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("TOC Retention Rate: Comparing Mendez to CS & Comparison HS") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "shift"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=retention, group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Shifting Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("Shifting Rate: Comparing Mendez to CS & Comparison HS") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "shift_toc"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=retention, group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Shifting Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("TOC Shifting Rate: Comparing Mendez to CS & Comparison HS") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
  }
  
  return(df_list_plot)  
}

mendez_plts<-create_mendez_plots(mendez_tbl_list)

## -----------------------------------------------------------------------------
## Part 2.2 - Create Miramonte Overall Graphs
## -----------------------------------------------------------------------------

add_overall_plot_tbls_miramonte<-function(name){
  df_list<-vector('list',4)
  names(df_list)<-c("rent", "rent_toc", "shift", "shift_toc")
  df_list[["rent"]]<-overall_graph_tbl_sch_type[["elem"]][["overall_rent"]]
  df_list[["rent_toc"]]<-overall_graph_tbl_sch_type[["elem"]][["toc_rent"]]
  df_list[["sch_rent"]]<-plot_tbls_cs_overall[[name]]
  df_list[["sch_rent_toc"]]<-plot_tbls_cs_overall_toc[[name]]
  df_list[["shift"]]<-overall_graph_tbl_sch_type[["elem"]][["overall_shift"]]
  df_list[["shift_toc"]]<-overall_graph_tbl_sch_type[["elem"]][["toc_shift"]]
  df_list[["sch_shift"]]<-shift_elem[["cs"]][["overall"]][[name]]
  df_list[["sch_shift_toc"]]<-shift_elem[["cs"]][["toc"]][[name]]
  return(df_list)
}

miramonte_o<-add_overall_plot_tbls_miramonte("Miramonte Elementary")

#Everyone
#create overall Mendez plot table
miramonte_only<-miramonte_o[["sch_rent"]] %>% filter(type == "Miramonte Elem")

#rbind to Overall Information
miramonte_plot_tbl<-rbind(miramonte_only, miramonte_o[["rent"]])

#Just TOCs
miramonte_only_toc<-miramonte_o[["sch_rent_toc"]] %>%
  filter(type == "Miramonte Elem")

#rbind to Overall Information
miramonte_plot_toc_tbl<-rbind(miramonte_only_toc,
                              miramonte_o[["rent_toc"]])

#Shift
transform_shift_tbl_miramonte<-function(df){
  df_update<-df %>%
    filter(type == "shifting rate (%)") %>% select(-c(type))
  
  df_update<-data.frame(t(df_update))
  df_update <- df_update %>% mutate(
    type = "Miramonte Elem",
    year = c("17-18","18-19","19-20","20-21","21-22","22-23")
  )
  
  colnames(df_update)<-c("retention","type","start_year")
  return(df_update)
}



#shift only
miramonte_shift_only<-transform_shift_tbl_miramonte(miramonte_o[["sch_shift"]])

miramonte_shift_only<-rbind(miramonte_shift_only, overall_graph_tbl_shift)


miramonte_shift_toc_only<-transform_shift_tbl_miramonte(miramonte_o[["sch_shift_toc"]])
miramonte_shift_toc_only<-rbind(miramonte_shift_toc_only,
                                overall_graph_tbl_toc_shift)


#put table a list
miramonte_tbl_list<-vector("list",4)
names(miramonte_tbl_list)<-c("overall", "overall_toc", "shift", "shift_toc")
miramonte_tbl_list[["overall"]]<-miramonte_plot_tbl
miramonte_tbl_list[["overall_toc"]]<-miramonte_plot_toc_tbl
miramonte_tbl_list[["shift"]]<-miramonte_shift_only
miramonte_tbl_list[["shift_toc"]]<-miramonte_shift_toc_only

#Plot Graphs

create_miramonte_plots<-function(df_list){
  
  df_list_plot<-vector("list",4)
  names(df_list_plot)<-c("overall", "overall_toc", "shift", "shift_toc")
  
  for (name in c("overall", "overall_toc","shift", "shift_toc")){
    
    if (name == "overall"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=as.numeric(retention), group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Retention Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("Retention Rate: Comparing Miramonte to CS & Comparison ES") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "overall_toc"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=as.numeric(retention), group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Retention Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("TOC Retention Rate: Comparing Miramonte to CS & Comparison ES") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "shift"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=as.numeric(retention), group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Shifting Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("Shifting Rate: Comparing Miramonte to CS & Comparison ES") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
    if (name == "shift_toc"){
      df_list_plot[[name]]<-
        ggplot(data=df_list[[name]],
               aes(x=start_year, y=as.numeric(retention), group=type)) +
        geom_line(aes(color=type))+
        geom_point(aes(color=type)) +
        labs(x = "Start Year",
             y = "Shifting Rate %") +
        scale_color_discrete(name = "School Type")+
        ggtitle("TOC Shifting Rate: Comparing Miramonte to CS & Comparison ES") + 
        theme(plot.title = element_text(hjust = 0.5))
    }
  }
  
  return(df_list_plot)  
}

miramonte_plts<-create_miramonte_plots(miramonte_tbl_list)


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
