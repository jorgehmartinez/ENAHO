# Nombre:     Script de manipulación y unión ENAHO
# Versión:    1.0
# Autores:    Decision Lab PUCP
# Objetivos:  Cargar las bases de datos de la ENAHO, unirlas en una sola
#             y construir las variables para el análisis  
# Fecha:      01/06/21

library(tidyverse)
library(haven)

#Cargamos las bases de datos de la ENAHO 2021 con la función read_sav()
hogar     <- read_sav("Enaho01-2020-200.sav")
educacion <- read_sav("Enaho01A-2020-300.sav")
empleo    <- read_sav("Enaho01A-2020-500.sav")

# Realizamos la unión mediante los identificadores CONGLOME:CODPERSO
data <- inner_join(hogar,educacion,by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) 
data <- inner_join(data,empleo,by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO"))

# Descartamos las bases de datos que ya no son necesarias para ahorrar memoria 
rm(hogar,educacion,empleo)

# Renombramos las variables de interés 
data <- data %>% 
  rename(Par_jefe = P203,Sexo = P207,Edad = P208A,Lengua = P300A,
         Nivel_estudios = P301A.x,Trabajo = P501,Quehaceres = P549) 

# Transformamos los valores de las variables de interés
data <- data %>% 
  mutate(Sexo = case_when(
    Sexo == 1 ~ "Hombre",
    Sexo == 2 ~ "Mujer")) %>% 
  mutate(Par_jefe = case_when( 
    Par_jefe == 1 ~ "Jefe(a)",
    Par_jefe == 2 ~ "Esposo(a)",
    Par_jefe == 3 ~ "Hijo(a)/Hijastro(a)",
    Par_jefe == 4 ~ "Yerno/Nuera",
    Par_jefe == 5 ~ "Nieto(a)",
    Par_jefe == 6 ~ "Padres/Suegros",
    Par_jefe == 7 ~ "Otros parientes",
    Par_jefe == 8 ~ "Trabajador Hogar",
    Par_jefe == 10 ~ "Otros no parientes",
    Par_jefe == 11 ~ "Hermano(a)")) %>% 
  mutate(UBIGEO = case_when( 
    UBIGEO < 20000 ~ "Amazonas",
    UBIGEO < 30000 ~ "Ancash",
    UBIGEO < 40000 ~ "Apurímac",
    UBIGEO < 50000 ~ "Arequipa",
    UBIGEO < 60000 ~ "Ayacucho",
    UBIGEO < 70000 ~ "Cajamarca",
    UBIGEO < 80000 ~ "Callao",
    UBIGEO < 90000 ~ "Cusco",
    UBIGEO < 100000 ~ "Huancavelica",
    UBIGEO < 110000 ~ "Huanuco",
    UBIGEO < 120000 ~ "Ica",
    UBIGEO < 130000 ~ "Junín",
    UBIGEO < 140000 ~ "La Libertad",
    UBIGEO < 150000 ~ "Lambayeque",
    UBIGEO < 160000 ~ "Lima",
    UBIGEO < 170000 ~ "Loreto",
    UBIGEO < 180000 ~ "Madre de Dios",
    UBIGEO < 190000 ~ "Moquegua",
    UBIGEO < 200000 ~ "Pasco",
    UBIGEO < 210000 ~ "Piura",
    UBIGEO < 220000 ~ "Puno",
    UBIGEO < 230000 ~ "San Martín",
    UBIGEO < 240000 ~ "Tacna",
    UBIGEO < 250000 ~ "Tumbes",
    TRUE ~ "Ucayali")) %>% 
  mutate(Lengua = case_when( 
    Lengua == 1 ~ "Quechua",
    Lengua == 2 ~ "Aimara",
    Lengua == 4 ~ "Castellano",
    Lengua == 6 ~ "Portugués",
    Lengua == 8 ~ "No escucha/No habla",
    Lengua == 9 ~ "Lengua de señas peruanas",
    Lengua == 10 ~ "Ashaninka",
    Lengua == 11 ~ "Awajún/Aguaruna",
    Lengua == 12 ~ "Shipibo–Konibo",
    Lengua == 13 ~ "Shawi/Chayahuita",
    Lengua == 14 ~ "Ashaninka",
    Lengua == 15 ~ "Achuar")) %>% 
  mutate(Nivel_estudios = case_when( 
    Nivel_estudios == 1 ~ "Sin Nivel",
    Nivel_estudios == 2 ~ "Educacion Inicial",
    Nivel_estudios == 3 ~ "Primaria incompleta",
    Nivel_estudios == 4 ~ "Primaria completa",
    Nivel_estudios == 5 ~ "Secundaria incompleta",
    Nivel_estudios == 6 ~ "Secundaria completa",
    Nivel_estudios == 7 ~ "Sup. no universitaria incompleta",
    Nivel_estudios == 8 ~ "Sup. no universitaria completa",
    Nivel_estudios == 10 ~ "Sup. universitaria incompleta",
    Nivel_estudios == 11 ~ "Sup. universitaria completa",
    Nivel_estudios == 12 ~ "Basica especial"))   

# Generamos la base de datos final solo con las variables necesarias
data <- data %>% 
  select(UBIGEO,Par_jefe,Sexo,Edad,Lengua,Nivel_estudios,Trabajo,Quehaceres)

#Exportamos la base de datos final como archivo csv
write.csv(data, "Enaho_anual")