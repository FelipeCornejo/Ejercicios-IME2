library(MASS)
library(dplyr)
library ( tidyverse )
datos <- birthwt
peso_raza <- datos %>% select(bwt,race)
peso_raza <- peso_raza %>% arrange(race)
