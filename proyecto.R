setwd(r'{C:\Users\migue\Documents\Curso DA BEDU\Modulo 2\proyecto}')
library(dplyr)
library(ggplot2)
library(tidyverse)

runnin2018 <- read.csv('RUNNIN2018.csv', sep = ';')
runnin2018 <- runnin2018 %>%
                select(DATE, RUNNING_HOURS) %>%
                mutate(DATE = as.Date(DATE, '%d/%m/%Y')) %>%
                rename(FECHA = DATE) 

df2018 <- read.csv('FALLAS2018.csv', sep = ';')
df2018 <- df2018 %>%
            mutate(PARO = tolower(PARO),
                   ï..FECHA = as.Date(ï..FECHA, '%d/%m/%Y'),
                   DIA = strftime(ï..FECHA, '%d'),
                   SEMANA = strftime(ï..FECHA, '%V'),
                   AÑO = strftime(ï..FECHA, '%Y')) %>%
            rename(FECHA = ï..FECHA) %>%
            subset(PARO == 'si') %>%
            drop_na(FECHA) %>%
            select(FECHA, AÑO, SEMANA, DIA,  ENCARGADO:PARO)

df2018 <- merge(df2018, runnin2018, by = 'FECHA')


#Distribucion del tiempo
ggplot(df2018, aes(x = TIEMPO)) +
  geom_histogram(fill = 'lightblue', color = 'darkblue') +
  labs(x='Tiempo', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')

ggplot(df2018, aes(y = TIEMPO)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue') +
  labs(x='Tiempo', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')


# Tiempo promedio de Falla por equipo
equipo_tiempo <- df2018 %>% 
                  group_by(EQUIPO) %>%
                  summarise(TIEMPO_PROM = mean(TIEMPO))  

ggplot(equipo_tiempo, aes(x = EQUIPO, y = TIEMPO_PROM)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Tiempo', title = 'Tiempo promedio de Falla por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

equipo_freq <- df2018 %>% 
                count(EQUIPO, name = 'FREQ')
                
# Fallas por equipo
ggplot(equipo_freq, aes(x = reorder(EQUIPO, -FREQ), y = FREQ)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Frecuencia', title = 'Fallas por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

# Indicadores
fallas_semana_2018 <- df2018 %>%
                        count(SEMANA, name = 'FALLAS')

indicadores2018 <- df2018 %>%
                     group_by(SEMANA, DIA) %>%
                     summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                     group_by(SEMANA) %>%
                     summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                     add_column(MBTF = .$RUNNING_HOURS / fallas_semana_2018$FALLAS, 
                               MTTR = .$TIEMPO_DE_FALLAS / fallas_semana_2018$FALLAS, 
                               BREAKDOWN = (.$TIEMPO_DE_FALLAS/60) / .$RUNNING_HOURS)


df2018[df2018$EQUIPO == 'BRCODCR - Encoder',]
summary(df2018$TIEMPO)
str(df2018)

mean(df2018$TIEMPO)

list.files('data', full.names = TRUE, pattern = 'FALLAS*')
