wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

library(dplyr)
library(ggplot2)
library(tidyverse)

df_runnin <- lapply(list.files('data', full.names = TRUE, pattern = 'RUNNIN*'), read.csv, sep = ';')
df_runnin <- do.call(rbind, df_runnin)


#runnin2018 <- read.csv('RUNNIN2018.csv', sep = ';')
df_runnin <- df_runnin %>%
                select(DATE, RUNNING_HOURS) %>%
                mutate(DATE = as.Date(DATE, '%d/%m/%Y')) %>%
                rename(FECHA = DATE)

df_fallas <- lapply(list.files('data', full.names = TRUE, pattern = 'FALLAS*'), read.csv, sep = ';')
df_fallas <- do.call(rbind, df_fallas)

#df2018 <- read.csv('FALLAS2018.csv', sep = ';')
df_fallas <- df_fallas %>%
            mutate(PARO = tolower(PARO),
                   ï..FECHA = as.Date(ï..FECHA, '%d/%m/%Y'),
                   DIA = strftime(ï..FECHA, '%d'),
                   SEMANA = strftime(ï..FECHA, '%V'),
                   AÑO = strftime(ï..FECHA, '%Y')) %>%
            rename(FECHA = ï..FECHA) %>%
            subset(PARO == 'si') %>%
            drop_na(FECHA) %>%
            select(FECHA, AÑO, SEMANA, DIA,  ENCARGADO:PARO)

df_fallas <- merge(df_fallas, df_runnin, by = 'FECHA')


#Distribucion del tiempo
ggplot(df_fallas, aes(x = TIEMPO)) +
  geom_histogram(fill = 'lightblue', color = 'darkblue') +
  labs(x='Tiempo', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')

ggplot(df_fallas, aes(y = TIEMPO)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue') +
  labs(x='Tiempo', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')


# Tiempo promedio de Falla por equipo
equipo_tiempo <- df_fallas %>% 
                  group_by(EQUIPO) %>%
                  summarise(TIEMPO_PROM = mean(TIEMPO))  

ggplot(equipo_tiempo, aes(x = EQUIPO, y = TIEMPO_PROM)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Tiempo', title = 'Tiempo promedio de Falla por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

equipo_freq <- df_fallas %>% 
                count(EQUIPO, name = 'FREQ')
                
# Fallas por equipo
ggplot(equipo_freq, aes(x = reorder(EQUIPO, -FREQ), y = FREQ)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Frecuencia', title = 'Fallas por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

# Indicadores
fallas_semana <- df_fallas %>%
                        group_by(AÑO, SEMANA) %>%
                        tally(name = 'FALLAS')

indicadores <- df_fallas %>%
                     group_by(AÑO, SEMANA, DIA) %>%
                     summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                     group_by(AÑO, SEMANA) %>%
                     summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                     add_column(MBTF = .$RUNNING_HOURS / fallas_semana$FALLAS,
                               MTTR = .$TIEMPO_DE_FALLAS / fallas_semana$FALLAS,
                               BREAKDOWN = (.$TIEMPO_DE_FALLAS/60) / .$RUNNING_HOURS)

indicadores <- indicadores %>%
                replace(is.na(.) | . == Inf, 0)



