# Missing data imputation- ICC#
#September 2019
#Luis M. Guirola

# 0. Load and packages

library(tidyverse)
library(haven)
library(forcats)
library(mice)
library(parallel)
load("data/0_clean_icc.Rdata")
# 1. Select the variables of interest #####
#All items, those that were recoded in categories, consumption items, and also basic demographics.
df_na =  dff %>% 
    select(ideology_3,
           starts_with("it_"), starts_with("c_"),
           ends_with("cat"),date,
           d_ccaa,d_prov, d_muni, d_density, d_woman, d_edu_cat ,d_income_merged,d_houseowner ,-d_birth_cat) 


# 2. Start the imputation ####
###########
set.seed(12)
l_df_na = df_na %>% group_split(date)

f_impute = function(dff){
    vmeth = mice(dff,maxit = 0)$method
    vmeth['d_houseowner'] = 'pmm'
    vmeth['d_edu_cat'] = 'pmm'
    vmeth['d_age_cat'] = 'pmm'
    vmeth['it_sit_eco_hh_retro'] = 'pmm'

    mice(dff, m=3,nnet.MaxNWts = 3000,remove.collinear = F, method = vmeth)
}

l_imputed_df = list()
for(iiter in seq_along(l_df_na)){
    print(iiter)
    l_imputed_df[[iiter]] =  f_impute(l_df_na[[iiter]])
    
}


# 3. Store the data #######

#Save the multiply imputed datasets
#save(l_imputed_df,file = 'data/1_missing_icc_full.Rdata')
save(l_imputed_df,file = 'data/1_missing_icc_full_aug20.Rdata')
#load("data/1_missing_icc_full.Rdata")
#Save the complete dataset
l_icc_complete = lapply(l_imputed_df, complete)  
save(l_icc_complete,file = 'data/1_missing_icc_complete_aug20.Rdata')


bind_rows(l_icc_complete)%>%filter(is.na(d_ccaa))%>%select(d_ccaa, d_prov)

