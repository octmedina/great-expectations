
library(tidyverse)
library(data.table)

df%>%glimpse

vfiles = list.files('data/epa_files/');
vzip_files =vfiles[str_detect(vfiles, 'zip')]

my_zipfile = vzip_files[42]

temp <- tempfile()
unzip(paste0('data/epa_files/',my_zipfile),exdir = temp)


ffile_csv = list.files(paste0(temp,'/CSV/'))

df = fread(paste0(temp,'/CSV/',ffile_csv)) 


unlink(temp)





#TAOI		AOI
#Código 	Descripción	
#03	Ocupados subempleados por insuficiencia de horas	
#04	Resto de ocupados	
#05	Parados que buscan primer empleo	
#06	Parados que han trabajado antes	
#07	Inactivos 1 (desanimados)	
#08	Inactivos 2 (junto con los desanimados forman los activos potenciales)	
#09	Inactivos 3 (resto de inactivos)	



#What do we want to postrat on?
#Education; NFORMA
#sex: SEXO1
#age (how many categories?) EDAD5 pre 2021 EDAD1 after 2021
# com aut CCAA
#province  PROV
#Employment status AOI 

