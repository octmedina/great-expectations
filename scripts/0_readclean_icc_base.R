# Load and clean the ICC data, from the original files (not fid)

#Idea: we want to retrieve party affiliations. For this we need individual surveys, 
#not available in the fichero integrado.
#First part of the script reads from an external file, and uses Gonzalo Rivero's script to read
#CIS files. The list of files is then save. The rest of the script can be executed 
#loading the original 0_readclean_icc_file.Rdata .
#Luis M. Guirola

#0. Load packages #####
library(haven)
library(tidyverse)

# 1. Get list of names files ####
vfiles = c(  
    "2016-07-15"= "3147",
    "2016-08-15"= "3148",  
    "2016-09-15"= "3151",
    "2016-10-15"= "3158",
    "2016-11-15"= "3160",
    "2016-12-15"= "3163",
    "2017-01-15"= "3165",
    "2017-02-15"= "3169",
    "2017-03-15"= "3171",
    "2017-04-15"= "3174",
    "2017-05-15"= "3176",
    "2017-06-15"= "3180",
    "2017-07-15"= "3185",
    "2017-08-15"= "3186",  
    "2017-09-15"= "3189",
    "2017-10-15"= "3193",
    "2017-11-15"= "3196",
    "2017-12-15"= "3200",
    "2018-01-15"= "3204",
    "2018-02-15"= "3206",
    "2018-03-15"= "3208",
    "2018-04-15"= "3211",
    "2018-05-15"= "3214",
    "2018-06-15"= "3218",
    "2018-07-15"= "3220",
    "2018-08-15"= "3222",  
  "2018-09-15"= "3225",
  "2018-10-15"= "3228",
  "2018-11-15"= "3232",
  "2018-12-15"= "3237",
  "2019-01-15"= "3239",
  "2019-02-15"= "3241",
  "2019-03-15"= "3243",
  "2019-04-15"= "3246",
  "2019-05-15"= "3249",
  "2019-06-15"= "3254",
  "2019-07-15"= "3258",
  "2019-08-15"= "3260",
  "2019-09-15"= "3262",
  "2019-10-15"= "3264",
  #SEcond election 2019 starts here
  "2019-11-15"= "3268",
  "2019-12-15"= "3270",
  "2020-01-15"= "3272",
  "2020-02-15"= "3274",
  "2020-03-15"= "3278",
  "2020-04-15"= "3280",
  "2020-05-15"= "3282",
  "2020-06-15"= "3284",
  "2020-07-15"= "3289",
  "2020-08-15" ='3291',
  "2020-09-15" ='3295',
  "2020-10-15" ='3297',
  "2020-11-15" ='3301',
  "2020-12-15" ='3304',
  "2021-01-15" ='3308',
  "2021-02-15" ='3311',
  "2021-03-15" ='3315',
  "2021-04-15" ='3319',
  "2021-05-15" ='3323',
  "2021-06-15" ='3327',
  "2021-07-15" ='3331',
  "2021-08-15" ='3333',
  "2021-09-15" ='3335',
  "2021-10-15" ='3338',
  "2021-11-15" ='3342',
  "2021-12-15" ='3345'
  )


# 1. Functions #######

#1.1 Function to read CIS#
######################
read_cis <- function(syntaxf, dataf) {
  ##Gonzalo's scripts
  ## @syntaxf is the location of the syntax file (starting with ES)
  ## @dataf is the location of the data file (starting with DA)
  require(foreign)
  require(haven)  
  ## Add EOL to the last line
  system(paste("sed -i -e '$a\\'", syntaxf, sep = " "))
  
  ## Convert to utf8
  new_syntaxf <- paste("utf8_", basename(syntaxf), sep = "")
  out_syntaxf <- file.path(dirname(syntaxf), new_syntaxf)
  system(paste("iconv -f ISO-8859-1 -t UTF-8", 
               syntaxf, ">", out_syntaxf, sep = " "))
  
  ## Create output file
  out_dataf <- paste(basename(dataf), ".sav", sep = "")
  exportline <- paste("SAVE OUTFILE='", basename(dataf), ".sav'.", sep = "")
  system(paste("echo", exportline, ">>", out_syntaxf, sep = " "))
  
  ## Replace commas with dots for decimals in datafile
  system(paste("sed -i -e 's/,/./g'", dataf, sep = " "))
  
  ## Run, damnit, run!
  cwd <- getwd()
  setwd(dirname(dataf))
  system(paste("pspp", basename(out_syntaxf), sep = " "), wait = TRUE)
  setwd(cwd)
  
  ## Read SAV file 
  #  outdf <- read.spss(file.path(dirname(dataf), out_dataf), 
  #                     to.data.frame = TRUE, use.value.labels = FALSE)
  outdf <- read_spss(file.path(dirname(dataf), out_dataf))
  return(outdf)
}
f_icc_read <-  function(ffile){
  
  folder = paste0("../data/consumer_confidence/MD", ffile)
  ssyntax = paste0(folder,'/ES',ffile)
  ddata = paste0(folder,'/DA',ffile)
  df = read_cis(syntaxf = ssyntax, dataf = ddata)
  return(df)
}


# 2. Read the files ####
l_icc_files = lapply(vfiles,function(ffile)f_icc_read(ffile))
save(l_icc_files, file  = "data/0_readclean_icc_file.Rdata")


#Execute from here.


load("data/0_readclean_icc_file.Rdata")

# 3. Explore ####
#Explore that the names of the columns coincides for all files ####
l_icc_files %>%
    lapply(function(x)colnames(x)) %>%unlist()%>%table()
#Explore variables names for a couple of data frame 
l_icc_files$`2019-09-15` %>%lapply(function(x)attr(x, which = "label"))
#lapply(l_icc_files,function(df)df%>%lapply(function(x)attr(x, which = "label")))%>%unlist()%>%table()


f_icc_clean_pre9_20 = function(estu_n){
  ddate = names(estu_n) %>%unlist()
  
  df = l_icc_files[[ddate]]
  
  df %>%
    transmute(ESTUDIO = ddate,
              year = substr(ddate, 1,4)%>%as.numeric,
              month = substr(ddate, 6,7)%>%as.numeric,
              date = ddate%>%as.Date(),
              d_ccaa = CCAA,
              d_prov = PROV,
              d_muni = MUN,
              d_density = TAMUNI,
              d_edu =                    na_if(x =ESTUDIOS, 9),
              d_woman =                  if_else(P19 ==2,1,0),#same for all
              d_active =                 na_if(P28, 9),#same for all
              d_income_merged =          na_if(x = P27, 9 ),#same for all
              d_houseowner =             na_if(x = P33, 8) %>%  na_if(9)%>%na_if(98)%>%na_if(99),
              d_age =                    na_if(P20,99),#
              d_birth =                  as.numeric(year),
              d_nationality  =           na_if(P30,9),
              #items
              it_sit_eco_hh_current =    P2%>%na_if(8)%>%na_if(9), #same for all
              it_sit_eco_hh_retro =      P3%>%na_if(8)%>%na_if(9),#same for all
              it_unem_around_n =         na_if(P5, 999 ),#same for all
              it_unem_around_retro =     na_if(x = P6, 8 ) %>% na_if(9) ,#same for all
              it_unem_findjob_retro =    na_if(x = P7, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_general_retro = na_if(x = P8, 8 ) %>% na_if(9),#same for all
              it_unem_findjob_pro =      na_if(x = P9, 8 ) %>% na_if(9) ,#same for all
              it_durable_pro_1y =        na_if(x = P10, 8 ) %>% na_if(9) ,#same for all
              it_saving_pro_1y =         na_if(x = P11, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_hh_pro =        na_if(x = P12, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_general_pro =   na_if(x = P14, 8 ) %>% na_if(9) ,#same for all
              it_inflation_pro =         na_if(x = P15, 8 ) %>% na_if(9),#same for all
              it_interest_pro =          na_if(x = P16, 8 ) %>% na_if(9),#same for all
              it_house_prices_pro =      na_if(x = P17, 8 ) %>% na_if(9),#same for all
              it_house_buy_pro =         na_if(x = P18, 8 ) %>% na_if(9) ,#same for all
              #Consumption#
              c_motor =                  as.integer( P1A01 + P1A02 > 0),#same for all
              c_motor_na =               as.integer( P1A04+P1A05 > 0),#same for all
              c_furniture =              as.integer( P1B01 + P1A02 > 0),#same for all
              c_furniture_na =           as.integer( P1B04+P1B05 > 0),#same for all
              c_electro_large =         as.integer( P1C01 + P1C02 > 0),#same for all
              c_electro_large_na =      as.integer( P1C04+P1C05 > 0),#same for all
              c_electro_small =         as.integer( P1D01 + P1D02 > 0),#same for all
              c_electro_small_na =       as.integer( P1D04+P1D05 > 0),#same for all
              #Vote and ideology
#              vote_yesno = P31 %>% na_if(6) %>% na_if(9),#same for all
#              vote_party = P31A ,#same for all
              vote_remember = RECUERDO,#same for all
              ideology =               na_if(P29, 99) %>%na_if(98)#same for all
    )%>%#Recode NA for consumption manually
    mutate(c_furniture = ifelse( c_furniture_na > 0, NA, c_furniture),
           c_motor = ifelse( c_motor_na > 0, NA, c_motor),
           c_electro_large = ifelse( c_electro_large_na > 0, NA, c_electro_large),
           c_electro_small = ifelse( c_electro_small_na > 0, NA, c_electro_small)) %>%
    select(-ends_with("_na"))
}


f_icc_clean_post9_20 = function(estu_n){
  ddate = names(estu_n) %>%unlist()
  
  df = l_icc_files[[ddate]]
  
  
  df %>%
    transmute(ESTUDIO = ddate,
              year = substr(ddate, 1,4)%>%as.numeric,
              month = substr(ddate, 6,7)%>%as.numeric,
              date = ddate%>%as.Date(),
              d_ccaa = CCAA,
              d_prov = PROV,
              d_muni = MUN,
              d_density = TAMUNI,
              d_edu =                    na_if(x =ESTUDIOS, 9),
              d_woman =                  if_else(P19 ==2,1,0),#same for all
              d_active =                 na_if(P28, 9),#same for all
              d_income_merged =          na_if(x = INGREHOG, 9 ),#same for all
              d_houseowner =             na_if(x = VIVIENDA, 8) %>%  na_if(9)%>%na_if(98)%>%na_if(99),
              d_age =                    na_if(P20,99),#
              d_birth =                  as.numeric(year),
              d_nationality  =           na_if(MOMENNAC,9),
              #items
              it_sit_eco_hh_current =    P2%>%na_if(8)%>%na_if(9), #same for all
              it_sit_eco_hh_retro =      P3%>%na_if(8)%>%na_if(9),#same for all
              it_unem_around_n =         na_if(NUMPERSONAS, 999 ),#specific after sept 2020
              it_unem_around_retro =     na_if(x = P6, 8 ) %>% na_if(9) ,#same for all
              it_unem_findjob_retro =    na_if(x = P7, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_general_retro = na_if(x = P8, 8 ) %>% na_if(9),#same for all
              it_unem_findjob_pro =      na_if(x = P9, 8 ) %>% na_if(9) ,#same for all
              it_durable_pro_1y =        na_if(x = P10, 8 ) %>% na_if(9) ,#same for all
              it_saving_pro_1y =         na_if(x = P11, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_hh_pro =        na_if(x = P12, 8 ) %>% na_if(9) ,#same for all
              it_sit_eco_general_pro =   na_if(x = P14, 8 ) %>% na_if(9) ,#same for all
              it_inflation_pro =         na_if(x = P15, 8 ) %>% na_if(9),#same for all
              it_interest_pro =          na_if(x = P16, 8 ) %>% na_if(9),#same for all
              it_house_prices_pro =      na_if(x = P17, 8 ) %>% na_if(9),#same for all
              it_house_buy_pro =         na_if(x = P18, 8 ) %>% na_if(9) ,#same for all
              #Consumption#
              c_motor =                  as.integer( P1_1_1 + P1_1_2 > 0),#same for all
              c_motor_na =               as.integer( P1_1_8+P1_1_9 > 0),#same for all
              c_furniture =              as.integer( P1_2_1 + P1_2_2 > 0),#same for all
              c_furniture_na =           as.integer( P1_2_8 + P1_2_9 > 0),#same for all
              c_electro_large =         as.integer( P1_3_1 + P1_3_2 > 0),#same for all
              c_electro_large_na =      as.integer( P1_3_8 + P1_3_9 > 0),#same for all
              c_electro_small =         as.integer( P1_4_1 + P1_4_2 > 0),#same for all
              c_electro_small_na =       as.integer( P1_4_8 + P1_4_9 > 0),#same for all
              #Vote and ideology
              #              vote_yesno = P31 %>% na_if(6) %>% na_if(9),#same for all
              #              vote_party = P31A ,#same for all
              vote_remember = RECUERDO,#same for all
              ideology =               na_if(ESCIDEOL, 99) %>%na_if(98)#same for all
    )%>%#Recode NA for consumption manually
    mutate(c_furniture = ifelse( c_furniture_na > 0, NA, c_furniture),
           c_motor = ifelse( c_motor_na > 0, NA, c_motor),
           c_electro_large = ifelse( c_electro_large_na > 0, NA, c_electro_large),
           c_electro_small = ifelse( c_electro_small_na > 0, NA, c_electro_small)) %>%
    select(-ends_with("_na"))
}




l_icc_clean = list()
for(iiter in  seq_along(vfiles)){
  print(vfiles[iiter])
  if(iiter <which(vfiles== '3295') ){#if before sept 20, estudio 3295
    l_icc_clean[[vfiles[iiter]]] = f_icc_clean_pre9_20(vfiles[iiter])
  }
  
  if(iiter >=which(vfiles== '3295') ){
    l_icc_clean[[vfiles[iiter]]] = f_icc_clean_post9_20(vfiles[iiter])
  }
  
}

# Check that the proper questions were selected
lapply(l_icc_clean, function(y)lapply(y,function(x)attr(x, which = "label"))) %>%
  unlist()%>%table





### 4. Recode individual variables ######
#                                       #
#                                       #

# 4.1. Vote ####
vvote19 = c(  "2019-05-15"= "3249",
              "2019-06-15"= "3254",
              "2019-07-15"= "3258",
              "2019-08-15"= "3260",
              "2019-09-15"= "3262",
              "2019-10-15"= "3264") 
#For november 2019
vvote19_bis = c("2019-11-15"= "3268",
                "2019-12-15"= "3270",
                "2020-01-15"= "3272",
                "2020-02-15"= "3274",
                "2020-03-15"= "3278",
                "2020-04-15"= "3280",
                "2020-05-15"= "3282",
                "2020-06-15"= "3284",
                "2020-07-15"= "3289",
                "2020-08-15" ='3291',
                "2020-09-15" ='3295',
                "2020-10-15" ='3297',
                "2020-11-15" ='3301',
                "2020-12-15" ='3304',
                "2021-01-15" ='3308',
                "2021-02-15" ='3311',
                "2021-03-15" ='3315',
                "2021-04-15" ='3319',
                "2021-05-15" ='3323',
                "2021-06-15" ='3327',
                "2021-07-15" ='3331',
                "2021-08-15" ='3333',
                "2021-09-15" ='3335',
                "2021-10-15" ='3338',
                "2021-11-15" ='3342',
                "2021-12-15" ='3345')




# Write a funciton that converters the labels to columns
frecode_vote =  function(dff){
  vvote = dff$vote_remember
  df_dictionary = data.frame(
    vote_n = attr(vvote,which = "labels") %>% names %>%as.character(),
    vote_remember = as.numeric(attr(vvote,which = "labels")))
  dff = mutate(dff,vote_remember = as.numeric(as.character(vote_remember))  )
  df1 = left_join(dff,df_dictionary, by = "vote_remember") %>%
    mutate(election = if_else(ESTUDIO %in% as.numeric(vvote19), "2019" , "2016"),
           election = if_else(ESTUDIO %in% as.numeric(vvote19_bis), "2019_bis" , election),
           vote_n = as.character(vote_n)%>% str_trim%>%tolower())
  return(df1)
}
#Do everything in a list, an recode those for which it does not work
l_ = l_icc_clean %>%lapply(frecode_vote)

l_[l_ %>% sapply(function(x)all(is.na(x$vote_n))) %>%which] %>%
  bind_rows()%>%
  select(ESTUDIO) %>% unlist()%>%unique

#In this case, three are empty: 3147, 3297 and 3301; 
#We use another one to recode it appropriately
# 3147 is empty "2016-07-15"
l_$`3147` = select(l_$`3147`,-vote_n)%>%
 left_join(select(l_$`3163`,vote_remember, vote_n) %>%unique, by = "vote_remember" )

# 2020-10 - 3297
l_$`3297` = select(l_$`3297`,-vote_n)%>%
  left_join(select(l_$`3295`,vote_remember, vote_n) %>%unique, by = "vote_remember" )%>%glimpse

#2020-10 - 3301
l_$`3301` = select(l_$`3301`,-vote_n)%>%
  left_join(select(l_$`3295`,vote_remember, vote_n) %>%unique, by = "vote_remember" )%>%glimpse


vcolnames = colnames(l_$`3147`)
ll = lapply(l_,function(dff)dff[vcolnames]%>%sjlabelled::remove_all_labels()) 


lapply(l_,function(dff)dff[vcolnames]%>%select(d_edu)%>%table) 

df = bind_rows(ll)%>%as_tibble()

#Check which parties we have
df$vote_n %>%table
unique(df$vote_n)%>% sort
df$vote_party = case_when(df$vote_n %in% c("compromís-podemos-eupv","en comú podem",
                            "unidas podemos","en marea","unidos podemos") ~ "P",
           df$vote_n %in% c("c's","ciudadanos") ~ "C",
           df$vote_n %in% c("eaj-pnv") ~ "PNV",
           df$vote_n %in% c("eh bildu") ~ "BILDU",
           df$vote_n %in% c("psoe") ~ "PSOE",
           df$vote_n %in% c("pp") ~ "PP",
           df$vote_n %in% c("vox") ~ "VOX",
           df$vote_n %in% c("en blanco","voto nulo","no votó",'nulo') ~ "Not Vote",
           df$vote_n %in% c("convergència (cdc)","jxcat") ~ "CIU",
           df$vote_n %in% c("erc") ~ "ERC",
           df$vote_n %in% c("otros", 'otros partidos') ~ "other",
           df$vote_n %in% c("no tenía derecho a voto" , "no tenía edad para votar","no tenía edad") ~ "too_young"
           ) 

table(df$vote_n,df$vote_party)
#Check which one remain uncoded
df$vote_n[is.na(df$vote_party)] %>%unique

df = select(df, -vote_n)
# 4.2. Education ####
#    #   #   #  # # # # 
#Code   #Meaning#                                                    #%
#0                                                      No procede    3
#1                               Menos de 5 años de escolarización    2
#2                                              Educación primaria    9
#3      Cualificación profesional grado inicial (FP grado inicial)    1
#4                                  Educación secundaria inferior    24
#5                                               FP de grado medio    5
#6                                                   Bachillerato    15
#7                                           FP de grado superior     9
#8                                 Estudios universitarios medios     9
#9                             Estudios universitarios superiores    16
#10                     Estudios oficiales de posgrado y doctorado    3
#11                                                          Otros    0

df$d_edu_cat = "10000"
df$d_edu_cat[df$d_edu <3 ] <- "0 Primary"
df$d_edu_cat[df$d_edu %in%c(3) ] <- "1 Lower sec."
df$d_edu_cat[df$d_edu %in%c(4,5,7) ] <- "2 Upper sec."
df$d_edu_cat[df$d_edu %in%c(6) ] <- "3 Tertiary"
df$d_edu_cat[is.na(df$d_edu) ] <- NA
#Check
(table(df$d_edu_cat) /length(df$d_edu_cat)) %>%round(digits = 4)*100


# 4.3 Activity ####

df$d_active_cat = "10000"
df$d_active_cat[df$d_active %in% c(1) ] <- "1 Working"
df$d_active_cat[df$d_active %in% c(2,3) ] <- "2 Retired"
df$d_active_cat[df$d_active %in% c(4,5) ] <- "3 Unemployed"
df$d_active_cat[df$d_active %in% c(6,7,8) ] <- "4 Inactive"
df$d_active_cat[is.na(df$d_active)] <- NA
#Check
(table(df$d_active_cat) /length(df$d_active_cat)) %>%round(digits = 4)*100


# 4.4. Year of birth and age ####

fyear_recode = function(vbirth,bottom_y, top_y,interval_y ){
  #Description: takes a vector of years, and interval, and an initial year
  #Returns:    years relabelled as characters. 
  #vbirth: vector of years to be recoded
  #bottom_y: bottom year of second interval (first is bottom residual)
  #interval_y: size of the interval
  
  #Copy the vector
  vbirth_bis = vbirth#will work to subset from vbirth
  #Generate:
  # top of the last interval generated at end of vector
  #top_y     = max(vbirth_bis,na.rm = TRUE)
  #Vector of bottom and top categories
  vbottom   = seq(bottom_y,top_y,interval_y)
  vtop      = lead(vbottom); vtop[is.na(vtop)] <- top_y 
  #Vector of categories
  vcat      = paste(vbottom+1, vtop, sep = "-")
  
  #Recode
  #The bottom category
  vbirth[vbirth_bis<bottom_y+1] <- paste0(bottom_y,"/pre ")
  vbirth[vbirth_bis>top_y]      <- paste0(top_y+1,"/post")
  #Iterate through the rest
  for(iter in 1:length(vcat)){
    vbirth[vbirth_bis %in% (vbottom[iter]+1):vtop[iter]] = vcat[iter]
  }
  return(vbirth)
}
#Recode in age and birth    
df$d_birth_cat = fyear_recode(df$d_birth, 1942,1995,12 )
df$d_age_cat = fyear_recode(df$d_age, 25,72,12 ) 

# 4.5. House ownership - only since 2013 #####

#1             En propiedad por compra y está totalmente pagada
#2   En propiedad por compra y con pagos o hipotecas pendientes
#3                                                  En alquiler
#4                         En propiedad por herencia o donación
#5 Cedida gratis o a bajo precio por un/a familiar, la empresa,
#6                                                   Otra forma


df$d_houseowner = df$d_houseowner %>% as.character()%>%
  recode( `1`= "Owner, fully paid",
          `2` = "Owner, partly paid",
          `3` = "Tenant, pays rent",
          `4` = "Owner, fully paid ",
          `5` = "Tenant, rent free",
          `6` = "Other") 


# 4.6. Ideology #######

#Recode 1-10 scale as two groups
#Note: those who identify around 5 are modal by far. 
df$ideology_3 = NA
df$ideology_3[df$ideology<5] = "1 Left"
df$ideology_3[df$ideology>5] = "3 Right"
df$ideology_3[df$ideology %in% c(5)] = "2 Center"
df$ideology_3[is.na(df$ideology) ] = "2 None"

# 5. Recode the data types ####

df_icc_party = mutate_at(df,
                vars(starts_with("it_"),
                     d_income_merged,d_density, d_edu_cat, month, year,d_age_cat ,d_birth_cat,ideology_3), 
                function(x)factor(x,ordered = TRUE)) %>%
  mutate_at(vars(d_prov, d_ccaa, d_muni,d_woman, vote_party, d_houseowner ),factor) %>%
  mutate(it_unem_around_n = it_unem_around_n %>%as.character() %>%as.double()) %>%
  as_tibble()


#6 Save the data frame #####

save(df_icc_party,file = "data/0_readclean_icc_party.Rdata")

