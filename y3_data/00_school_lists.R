################################################################################
##
## [ PROJ ] < Community School Teacher Retention Study >
## [ FILE ] < 00_school_lists.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 8/4/24 >
##
################################################################################

#Goal: Store all the schools strings and lists.

################################################################################

## ---------------------------
## libraries
## ---------------------------

## ---------------------------
## directory paths
## ---------------------------

## ---------------------------
## file directories
## ---------------------------

## ---------------------------
## helper functions
## ---------------------------

## -----------------------------------------------------------------------------
## Part 1 - School Strings
## -----------------------------------------------------------------------------

#CS strings
cs_string_cohort1_elem<-
  c("74th Street Elementary",
    "93rd Street Elementary",
    "93rd Street Elementary DL Two-Way Im Spanish",
    "Alta California Elementary",
    "Alta Loma Elementary",
    "Alta Loma Elementary DL Two-Way Im Spanish",
    "Catskill Avenue Elementary",
    "Catskill Avenue Elementary DL Two-Way Im Spanish",
    "Euclid Avenue Elementary",
    "Euclid Avenue Elementary DL Two-Way Im Spanish",
    "Farmdale Elementary",
    "Farmdale Elementary DL Two-Way Im Spanish",
    "Miramonte Elementary",
    "Miramonte Elementary DL Two-Way Im Spanish",
    "Van Nuys Elementary",
    "Vine Street Elementary",
    "Walnut Park Elementary",
    "Walnut Park Elementary DL Two-Way Im Spanish")

cs_string_cohort1_ms_hi<-
  c("Marina Del Rey Middle School",
    "Felicitas And Gonzalo Mendez Senior High",
    "Miguel Contreras Learning Complex - LA Sch of Global Studies",
    "Panorama Senior High",
    "John H Francis Polytechnic Senior High",
    "Woodrow Wilson Senior High")

cs_string_cohort2_3_elem<-
  c("Baldwin Hills Elementary",
    "Carlos Santana Arts Academy",
    "Carlos Santana Arts Academy DL Two-Way Im Spanish",
    "Ellen Ochoa LC DL Two-Way Im Spanish",
    "Ellen Ochoa Learning Center",
    "Gardner Street Elementary",
    "Hillcrest Drive Elementary",
    "Hillcrest Drive Elementary DL Two-Way Im Spanish",
    "Logan Academy Globl Ecol Two-Way Im Spanish",
    "Logan Academy of Global Ecology",
    "Lucille Roybal-Allard Elementary",
    "Lucille Roybal-Allard Elementary DL Two-Way Im Spanish",
    "Palms Elementary",
    "Sharp Avenue Elementary",
    "Purche Avenue Elementary",
    "Trinity Street ES",
    "Trinity Street Elementary",
    "Trinity Street Elementary DL Two-Way Im Spanish")

cs_string_cohort2_3_mid_hi<-
  c("Audubon Middle School",
    "Carver MS",
    "Augustus Hawkins Senior High",
    "Augustus F Hawkins SH - Critical Design",
    "Susan Miller Dorsey Senior High",
    "Miguel Contreras Learning Complex - Acdemic Ldrshp Community",
    "Venice Senior High")

cs_string<-c(cs_string_cohort1_elem,
             cs_string_cohort1_ms_hi,
             cs_string_cohort2_3_elem,
             cs_string_cohort2_3_mid_hi)

cs_cohort1_string<-c(cs_string_cohort1_elem,cs_string_cohort1_ms_hi)
cs_cohort2_3_string<-c(cs_string_cohort2_3_elem,cs_string_cohort2_3_mid_hi)
cs_elem<-c(cs_string_cohort1_elem,cs_string_cohort2_3_elem)
cs_mid_hi<-c(cs_string_cohort1_ms_hi,cs_string_cohort2_3_mid_hi)


#cohort 2 string

cs_cohort2_string<-c("Baldwin Hills Elementary",
                     "Carlos Santana Arts Academy",
                     "Carlos Santana Arts Academy DL Two-Way Im Spanish",
                     "Ellen Ochoa LC DL Two-Way Im Spanish",
                     "Ellen Ochoa Learning Center",
                     "Gardner Street Elementary",
                     "Logan Academy Globl Ecol Two-Way Im Spanish",
                     "Logan Academy of Global Ecology",
                     "Lucille Roybal-Allard Elementary",
                     "Lucille Roybal-Allard Elementary DL Two-Way Im Spanish",
                     "Palms Elementary",
                     "Sharp Avenue Elementary",
                     "Audubon Middle School",
                     "Carver MS",
                     "Augustus Hawkins Senior High",
                     "Augustus F Hawkins SH - Critical Design",
                     "Susan Miller Dorsey Senior High")
                     

## -----------------------------------------------------------------------------
## Part 2 - Neighborhood Strings
## -----------------------------------------------------------------------------

neighborhood_strings<-c("hamilton","monroe","bell_cudahy_maywood",
                        "hollywood","la_mid_city","downtown","sun_valley",
                        "gardena","south_la","riveria","south_mid_city",
                        "venice","heet","macarthur_park",
                        "historic_central_ave","fremont","carson",
                        "boyle_heights","lincoln_heights_el_sereno",
                        "van_nuys_valley_glen","huntington_park_vernon",
                        "panorama_city")

neighborhood_string_list<-vector("list", length(neighborhood_strings))
names(neighborhood_string_list)<-neighborhood_strings


neighborhood_string_list[["hamilton"]]<-c("Baldwin Hills Elementary",
            "Charnock Road Elementary",
            "Palms Elementary",
            "Shenandoah Street Elementary")

neighborhood_string_list[["monroe"]]<-c("Alta California Elementary",
          "Carlos Santana Arts Academy",
          "Carlos Santana Arts Academy DL Two-Way Im Spanish",
          "Chase Street Elementary",
          "Langdon Avenue Elementary",
          "Liggett Street Elementary",
          "Panorama City Elementary",
          "Plummer Elementary",
          "Rosa Parks Learning Center")

neighborhood_string_list[["bell_cudahy_maywood"]]<-c("Corona Avenue Elementary",
                     "Jaime Escalante Elementary School",
                     "Loma Vista Elementary",
                     "Loma Vista Elementary DL Two-Way Im Spanish",
                     "Woodlawn Avenue Elementary",
                     "Woodlawn Avenue Elementary DL Two-Way Im Spanish",
                     "Orchard Academies 2B",
                     "Orchard Academies 2C",
                     "Elizabeth Learning Center",
                     "Elizabeth LC DL Two-Way Im Arabic",
                     "Ellen Ochoa Learning Center",
                     "Ellen Ochoa LC DL Two-Way Im Spanish")

neighborhood_string_list[["hollywood"]]<-c("Gardner Street Elementary",
             "Harvard Elementary",
             "Harvard Elementary DL Two-Way Im Spanish",
             "Hollywood Elementary",
             "Van Ness Avenue Elementary",
             "Cheremoya Avenue Elementary",
             "Vine Street Elementary")

neighborhood_string_list[["la_mid_city"]]<-c("Alta Loma Elementary",
               "Alta Loma Elementary DL Two-Way Im Spanish",
               "Arlington Heights Elementary",
               "Arlington Heights Elementary DL Two-Way Im Spanish",
               "Cienega Elementary",
               "Cienega Elementary DL Two-Way Im Spanish",
               "54th Street Elementary",
               "Virginia Road Elementary",
               "Virginia Road Elementary DL Two-Way Im Spanish")

neighborhood_string_list[["downtown"]]<-c("Logan Academy of Global Ecology",
            "Logan Academy Globl Ecol Two-Way Im Spanish",
            "Frank Del Olmo Elementary",
            "Rosemont Elementary",
            "Rosemont Avenue Elementary")

neighborhood_string_list[["sun_valley"]]<-
  c("Beachy Avenue Elementary",
    "Julie Korenstein Elementary",
    "Sharp Avenue Elementary",
    "Fernangeles Elementary",
    "Fernangeles DL One & Two-Way Im Spanish",
    "Arminta Street Elementary",
    "Arleta Senior High",
    "John H Francis Polytechnic Senior High")

neighborhood_string_list[["gardena"]]<-c("153rd Street Elementary",
           "153rd Street Elementary DL Two-Way Im Spanish",
           "Amestoy Elementary",
           "Chapman Elementary",
           "Chapman Elementary DL Two-Way Im Mandarin",
           "Denker Avenue Elementary",
           "Denker Avenue Elementary DL Two-Way Im Korean",
           "Purche Avenue Elementary")

neighborhood_string_list[["south_la"]]<-c("9th Street ES",
            "9th Street Elementary",
            "28th Street ES",
            "28th Street Elementary",
            "28th Street Elementary DL One-Way Im Spanish",
            "Ricardo Lizarraga ES",
            "Ricardo Lizarraga Elementary",
            "Ricardo Lizarraga Elementary DL Two-Way Im Spanish",
            "Trinity Street ES",
            "Trinity Street Elementary",
            "Trinity Street Elementary DL Two-Way Im Spanish",
            "Wadsworth Ave ES",
            "Wadsworth Avenue Elementary",
            "West Vernon Ave ES",
            "West Vernon Avenue Elementary")

neighborhood_string_list[["riveria"]]<-c("66th Street Elementary",
           "Judith F Baca Arts Academy",
           "Judith F Baca Arts Acad DL Two-Way Im Spanish",
           "McKinley Avenue Elementary",
           "Miramonte Elementary",
           "Miramonte Elementary DL Two-Way Im Spanish",
           "Parmelee Avenue Elementary",
           "Parmelee Avenue Elementary DL Two-Way Im Spanish",
           "Russell Elementary",
           "Wisdom Elementary")

neighborhood_string_list[["south_mid_city"]]<-c("Augustus Hawkins Senior High",
                  "Augustus F Hawkins SH - Critical Design")

neighborhood_string_list[["venice"]]<-c("Marina Del Rey Middle School",
          "Mark Twain Middle School",
          "Venice Senior High")

neighborhood_string_list[["heet"]]<-c("42nd Street Elementary",
        "42nd Street Elementary DL World Lang Im Spanish",
        "59th Street Elementary",
        "74th Street Elementary",
        "Hillcrest Drive Elementary",
        "Hillcrest Drive Elementary DL Two-Way Im Spanish",
        "Young Empowered Scholars Academy",
        "Coliseum Street Elementary",
        "Manhattan Place Elementary",
        "Manhatten Place Elementary",
        "Audubon Middle School",
        "Susan Miller Dorsey Senior High")

neighborhood_string_list[["macarthur_park"]]<-
  c("Harold McAlister High School CYESIS",
    "Miguel Contreras Learning Complex - Acdemic Ldrshp Community",
    "Miguel Contreras Learning Complex - Business and Tourism",
    "Miguel Contreras Learning Complex - LA Sch of Global Studies",
    "Miguel Contreras Learning Complex - School of Social Justice")

neighborhood_string_list[["historic_central_ave"]]<-
  c("Carver MS",
    "George Washington Carver Middle School",
    "Los Angeles Acad MS",
    "Los Angeles Academy Middle School")

neighborhood_string_list[["fremont"]]<-
  c("107th Street Elementary",
    "75th Street Elementary",
    "75Th Street Elementary DL Two-Way Im Spanish",
    "93rd Street Elementary",
    "93rd Street Elementary DL Two-Way Im Spanish",
    "99th Street Elementary",
    "Figueroa Street Elementary",
    "Florence Griffith Joyner Elementary",
    "South Park Elementary",
    "South Park Elementary DL Two-Way Im Spanish")

neighborhood_string_list[["carson"]]<-
  c("Bonita Street Elementary",
    "Caroldale Learning Community",
    "Catskill Avenue Elementary",
    "Catskill Avenue Elementary DL Two-Way Im Spanish",
    "Dolores Street Elementary",
    "Glenn Hammond Curtiss Middle School")

neighborhood_string_list[["boyle_heights"]]<-
  c("1st Street Elementary",
    "Euclid Avenue Elementary",
    "Euclid Avenue Elementary DL Two-Way Im Spanish",
    "Carmen Lomas Garza Primary Center",
    "2nd Street Elementary",
    "2nd Street Elementary DL Two-Way Im Spanish",
    "Malabar Street Elementary",
    "Soto Street Elementary",
    "Felicitas And Gonzalo Mendez Senior High",
    "Theodore Roosevelt Senior High")

neighborhood_string_list[["lincoln_heights_el_sereno"]]<-
  c("El Sereno Elementary",
    "Farmdale Elementary",
    "Farmdale Elementary DL Two-Way Im Spanish",
    "Multnomah Street Elementary",
    "Sierra Park Elementary",
    "Abraham Lincoln Senior High",
    "Woodrow Wilson Senior High")

neighborhood_string_list[["van_nuys_valley_glen"]]<-
  c("Columbus Avenue Elementary",
    "Erwin Elementary",
    "Hazeltine Avenue Elementary",
    "Kittridge Street Elementary",
    "Kittridge Street Elementary DL Two-Way Im Armenian",
    "Kittridge Street Elementary DL Two-Way Im Spanish",
    "Sylvan Park Elementary",
    "Van Nuys Elementary")

neighborhood_string_list[["huntington_park_vernon"]]<-
  c("Florence Avenue Elementary",
    "Florence Avenue Elementary DL Two-Way Im Spanish",
    "Hope Street Elementary",
    "Huntington Park Elementary",
    "Huntington Park Elementary DL Two-Way Im Spanish",
    "Lucille Roybal-Allard Elementary",
    "Lucille Roybal-Allard Elementary DL Two-Way Im Spanish",
    "Middleton Street Elementary",
    "Miles Avenue Elementary",
    "Miles Avenue Elementary DL Two-Way Im Spanish",
    "San Antonio Elementary",
    "Walnut Park Elementary",
    "Walnut Park Elementary DL Two-Way Im Spanish")

neighborhood_string_list[["panorama_city"]]<-
  c("Cal Burke High School",
    "Panorama Senior High")

## -----------------------------------------------------------------------------
## Part 2 - Comparison Sites
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------