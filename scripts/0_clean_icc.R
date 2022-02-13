#Clean consumer confidence data #
#Luis M. Guirola # Octavio Medina
#Nov 2021                 #

#Description: This file cleans and saves the 'fichero integrado' of the ICC
#This is the version of the CIS file that includes all individual surveys. 
#More comfortable than individual files, but does not include the Vote variables.
#

##0. Load data and package
library(sjlabelled)
library(tidyverse)
library(haven)
library(ggthemes)
library(forcats)



df_icc <- read_sav('data/FID_nov21.sav')

### 1. Select variables and recode NA #####
##

df = df_icc %>%
    transmute(
        ESTUDIO, year = AÑO, month =  MES, 
        #Items
        it_sit_eco_hh_current =    if_else(is.na(B.2.2), B.2.1,B.2.2),
        it_sit_eco_hh_current =    na_if(x = it_sit_eco_hh_current, 8 ) %>% na_if(9),
        it_sit_eco_hh_retro =      if_else(is.na(B.3.2), B.3.1,B.3.2),
        it_sit_eco_hh_retro =      na_if(x = it_sit_eco_hh_retro, 8 ) %>% na_if(9),
        it_unem_around_n =         na_if(x =  B.4, 999 ),
        it_unem_around_retro =     na_if(x = B.5, 8 ) %>% na_if(9) ,
        it_unem_findjob_retro =    na_if(x = B.6, 8 ) %>% na_if(9),
        it_sit_eco_general_retro = na_if(x = B.7, 8 ) %>% na_if(9),
        it_inflation_pro =         na_if(x = D.1, 8 ) %>% na_if(9),
        it_interest_pro =          if_else(is.na(D.2.2), D.2.1,D.2.2),
        it_interest_pro =          na_if(x = it_interest_pro, 8 ) %>% na_if(9) ,
        it_house_prices_pro =      if_else(is.na(D.3.2), D.3.1,D.3.2),
        it_house_prices_pro =      na_if(x = it_house_prices_pro, 8 ) %>% na_if(9) ,
        it_house_buy_pro =         na_if(x = D.4, 8 ) %>% na_if(9) ,
        it_unem_findjob_pro =      na_if(x = C.1, 8 ) %>% na_if(9) ,
        it_durable_pro_1y =        na_if(x = C.2, 8 ) %>% na_if(9) ,
        it_saving_pro_1y =         na_if(x = C.3, 8 ) %>% na_if(9) ,
        it_sit_eco_hh_pro =        if_else(is.na(C.4.2), C.4.1,C.4.2),
        it_sit_eco_hh_pro =        na_if(x = it_sit_eco_hh_pro, 8 ) %>% na_if(9),
        it_sit_eco_general_pro =   na_if(x = C.5, 8 ) %>% na_if(9),
        #Political variables#
        ideology =                 na_if(x = F.1, 99) %>%  na_if(98),
        #Covariates#
        d_income_merged =          if_else(is.na(G.6.1), G.6.2, G.6.1),
        d_income_merged =          na_if(x = d_income_merged, 9 ),
        d_ccaa =                   A.1,     
        d_prov =                   A.5,
        d_muni =                   A.6,
        d_capital =                A.7,
        d_density =                A.2,
        d_woman =                  if_else(A.3 ==2,1,0),
        d_age =                    na_if(x = A.4, 99 ),
        d_edu =                    na_if(x =E.2.1, 99)%>%na_if(98),
        d_edu_2 =                  na_if(x =E.2.2, 99) %>%na_if(98),
        d_houseowner =             na_if(x = G.8, 8) %>%  na_if(9),#only since 201304 
        d_active =                 na_if(G.7, 9),
        d_birth =                  year - d_age,
        d_nationality  =           na_if(A.8.1,9),
        d_nationality_origin  =    na_if(A.8.3,999),
        #Consumption#
        c_motor =                  as.integer( B.1.1.1_1 + B.1.1.1_2 + B.1.2.1_1 + B.1.2.1_2 > 0),
        c_motor_na =               as.integer( B.1.1.1_4 + B.1.1.1_5 + B.1.2.1_4 + B.1.2.1_5 > 0),
        c_furniture =              as.integer( B.1.1.2_1 + B.1.1.2_2 + B.1.2.2_1 + B.1.2.2_2 > 0),
        c_furniture_na =           as.integer( B.1.1.2_4 + B.1.1.2_5 + B.1.2.2_4 + B.1.2.2_5 > 0),
        c_electro_large =          as.integer( B.1.1.3_1 + B.1.1.3_2 + B.1.2.3_1 + B.1.2.3_2 > 0),
        c_electro_large_na =       as.integer( B.1.1.3_4 + B.1.1.3_5 + B.1.2.3_4 + B.1.2.3_5 > 0),
        c_electro_small =          as.integer( B.1.1.4_1 + B.1.1.4_2 + B.1.2.4_1 + B.1.2.4_2 > 0),
        c_electro_small_na =       as.integer( B.1.1.4_4 + B.1.1.4_5 + B.1.2.4_4 + B.1.2.4_5 > 0)
    )
#Recode NA for consumption manually
df = df %>%
    mutate(c_furniture = ifelse( c_furniture_na > 0, NA, c_furniture),
           c_motor = ifelse( c_motor_na > 0, NA, c_motor),
           c_electro_large = ifelse( c_electro_large_na > 0, NA, c_electro_large),
           c_electro_small = ifelse( c_electro_small_na > 0, NA, c_electro_small)) %>%
    select(-ends_with("_na"))

### 2. Recode individual variables ######
#                                       #
#                                       #

# 2.1. Education ####
#    #   #   #  # # # # 
#    For edu (before 3295 study)
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
df$d_edu_cat[df$d_edu %in% 0:3 ] <- "0 Primary"
df$d_edu_cat[df$d_edu %in%c(4) ] <- "1 Lower sec."
df$d_edu_cat[df$d_edu %in%c(5,6,7,11) ] <- "2 Upper sec."
df$d_edu_cat[df$d_edu %in%c(8:10) ] <- "3 Tertiary"
df$d_edu_cat[is.na(df$d_edu) ] <- NA
df$d_edu_cat%>%table
#Desde septiembre 2020 
# 0                                                         N.P.
#1                            Menos de 5 años de escolarización
#2 Educación Primaria (Educación Primaria de LOGSE, 5º curso de
#3  Cualificación profesional grado inicial (FP grado inicial).
#4 Educación secundaria (ESO, EGB. Graduado Escolar. Certificad
#5 FP de Grado Medio (ciclo/módulo formativo de FP (grado medio
#6 Bachillerato (Bachillerato LOGSE, BUP, Bachillerato Superior
#7 FP de Grado Superior (ciclo/módulo formativo de FP (grado su
#8   Arquitectura/Ingeniería Técnica (aparejador/a; peritos/as)
#9 Diplomatura (ATENCIÓN: sólo diplomaturas oficiales, no codif
#10 Grado (estudios de grado, enseñanzas artísticas equivalentes
#11 Licenciatura (titulaciones con equivalencia oficial: 2º cicl
#12                                      Arquitectura/Ingeniería
#13 Máster oficial universitario (especialidades médicas o equiv
#14                                                    Doctorado
#15        Títulos propios de posgrado (máster no oficial, etc.)
#16                                               Otros estudios
#98                                             N.S./No recuerda
#99                                                         N.C.
df$d_edu_cat[df$d_edu_2 %in% 0:3 ] <- "0 Primary"
df$d_edu_cat[df$d_edu_2 %in%c(4) ] <- "1 Lower sec."
df$d_edu_cat[df$d_edu_2 %in%c(5,6,7,16) ] <- "2 Upper sec."
df$d_edu_cat[df$d_edu_2 %in%c(8:15) ] <- "3 Tertiary"

df$d_edu_2%>%table

#Check
(table(df$d_edu_cat) /length(df$d_edu_cat)) %>%round(digits = 4)*100

# 2.2.  Activity #####
#



#1        Trabaja                                  46
#2        Jubilado/a o pensionista (ha trabajado)  20
#3        Pensionista (no ha trabajado)            3.2
#4        Parado/a y ha trabajado antes            12.6
#5        Parado/a y busca su primer empleo        .75
#6        Estudiante                               8.36
#7        Trabajo doméstico no remunerado          8.13
#8        Otra situación                           


df$d_active_cat = "10000"
df$d_active_cat[df$d_active %in% c(1) ] <- "1 Working"
df$d_active_cat[df$d_active %in% c(2,3) ] <- "2 Retired"
df$d_active_cat[df$d_active %in% c(4,5) ] <- "3 Unemployed"
df$d_active_cat[df$d_active %in% c(6,7,8) ] <- "4 Inactive"
df$d_active_cat[is.na(df$d_active)] <- NA
#Check
(table(df$d_active_cat) /length(df$d_active_cat)) %>%round(digits = 4)*100

# 2.3. Year of birth and age ####

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
df$d_age_cat = fyear_recode(df$d_age%>%as.numeric, 25,72,12 ) 

# 2.4. House ownership - only since 2013#####

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


# 2.5. Ideology####

#Recode 1-10 scale as two groups
#Note: those who identify around 5 are modal by far. 
df$ideology_3 = NA
df$ideology_3[df$ideology<5] = "1 Left"
df$ideology_3[df$ideology>5] = "3 Right"
df$ideology_3[df$ideology %in% c(5)] = "2 Center"
df$ideology_3[is.na(df$ideology) ] = "2 None"

# Recode the date variables

df$date = paste0(df$year,"-",
                  if_else(nchar(df$month)==1,
                          true =  paste0("0",df$month),
                          false = as.character(df$month)),
                  "-15") %>% as.Date()

# 3. Recode the data types ####

dff = remove_all_labels(df)

dff = mutate_at(dff,
          vars(starts_with("it_"),
               d_income_merged,d_density, d_edu_cat, month, year,d_age_cat ,d_birth_cat,ideology_3), 
          function(x)factor(x,ordered = TRUE)) %>%
    mutate_at(vars(d_prov, d_ccaa, d_muni,d_woman ),factor) %>%
    mutate(it_unem_around_n = it_unem_around_n %>%as.character() %>%as.double()) %>%
    as_tibble()



# 4. Save the data ####

save(dff, file = "data/0_clean_icc.Rdata")
save(df, file = "data/0_clean_icc_withlabels.Rdata")


