wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(modeest)

df_runnin <- lapply(list.files('data', full.names = TRUE, pattern = 'RUNNIN*'), read.csv, sep = ';')
df_runnin <- do.call(bind_rows, df_runnin)

df_runnin <- df_runnin %>%
                select(DATE, RUNNING_HOURS) %>%
                mutate(DATE = as.Date(DATE, '%d/%m/%Y')) %>%
                rename(FECHA = DATE)


head(df_runnin,(15))
tail(df_runnin)

df_fallas <- lapply(list.files('data', full.names = TRUE, pattern = 'FALLAS*'), read.csv, sep = ';')
df_fallas <- do.call(rbind, df_fallas)

df_fallas <- df_fallas %>%
              mutate(PARO = tolower(PARO),
                     ENCARGADO = tolower(ENCARGADO),
                     ï..FECHA = as.Date(ï..FECHA, '%d/%m/%Y'),
                     DIA = strftime(ï..FECHA, '%d'),
                     SEMANA = strftime(ï..FECHA, '%V'),
                     AÑO = strftime(ï..FECHA, '%Y')) %>%
              rename(FECHA = ï..FECHA) %>%
              subset(PARO == 'si') %>%
              drop_na(FECHA) %>%
              select(FECHA, AÑO, SEMANA, DIA,  ENCARGADO:PARO)
              
df_fallas <- df_fallas[df_fallas$TIEMPO > 10, ]

df_fallas$ENCARGADO <- df_fallas$ENCARGADO %>%
                        replace(. == 'humberto barraza', 'humberto barraza villanueva') %>%
                        replace(. == 'isay sã¡nchez mejã­a', 'isay sanchez mejia') %>%
                        str_to_title(.)

head(df_fallas)
tail(df_fallas)
names(df_fallas)

df_fallas <- merge(df_fallas, df_runnin, by = 'FECHA')




# Indicadores
fallas_semana <- df_fallas %>%
                        group_by(AÑO, SEMANA) %>%
                        tally(name = 'FALLAS')

indicadores <- df_fallas %>%
                     group_by(AÑO, SEMANA, DIA) %>%
                     summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                     group_by(AÑO, SEMANA) %>%
                     summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                     add_column(FALLAS = fallas_semana$FALLAS,
                               MTBF = .$RUNNING_HOURS / fallas_semana$FALLAS,
                               MTTR = .$TIEMPO_DE_FALLAS / fallas_semana$FALLAS,
                               BREAKDOWN = (.$TIEMPO_DE_FALLAS/60) / .$RUNNING_HOURS)


indicadores_turno <- df_fallas %>%
                      group_by(AÑO, SEMANA, DIA, TURNO) %>%
                      summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                      group_by(AÑO, SEMANA, TURNO) %>%
                      summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                      merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
                      add_column(MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)


indicadores_encargado <- df_fallas %>%
                      group_by(AÑO, SEMANA, DIA, ENCARGADO) %>%
                      summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                      group_by(AÑO, SEMANA, ENCARGADO) %>%
                      summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                      merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
                      add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
                                 MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_encargado <- indicadores_encargado %>%
  replace(is.na(.) | . == Inf, 0)

indicadores_equipo <- df_fallas %>%
  group_by(AÑO, SEMANA, DIA, ENCARGADO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO, SEMANA, ENCARGADO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
  add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
             MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)


encargado_prom <- indicadores_encargado %>%
                  group_by(AÑO, ENCARGADO) %>%
                  summarise(PROM = mean(MTTR))


indicadores <- indicadores %>%
                replace(is.na(.) | . == Inf, 0)



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

ggplot(equipo_tiempo, aes(x = reorder(EQUIPO, -TIEMPO_PROM), y = TIEMPO_PROM)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Tiempo', title = 'Tiempo promedio de Falla por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

qqnorm(equipo_tiempo$TIEMPO_PROM,main="QQ plot of normal data",pch=19)
qqline(equipo_tiempo$TIEMPO_PROM)

equipo_freq <- df_fallas %>% 
  count(EQUIPO, name = 'FREQ')

# Fallas por equipo
ggplot(equipo_freq, aes(x = reorder(EQUIPO, -FREQ), y = FREQ)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Frecuencia', title = 'Fallas por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

# MTTR por encargado
ggplot(indicadores_encargado, aes(x = ENCARGADO, y = MTTR, fill = ENCARGADO)) +
  geom_boxplot() +
  labs(x='Encargado', y = 'MTTR', title = 'Tiempo medio de falla por encargado') +
  theme(axis.text.x = element_text(angle = 90,
        vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

# MTBF y MTTR histogramas
ggplot(indicadores, aes(x = MTBF)) +
  geom_histogram() +
  facet_wrap(~AÑO) +
  labs(x='MTBF', y = 'Frecuencia', title = 'MTBF') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

ggplot(indicadores, aes(x = MTTR)) +
  geom_histogram() +
  facet_wrap(~AÑO) +
  labs(x='MTTR', y = 'Frecuencia', title = 'MTTR') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')


# MTTR Semanal
ggplot(indicadores, aes(x = SEMANA, y = MTTR)) +
  geom_point() +
  facet_wrap(~AÑO) +
  labs(x='Semana', y = 'MTTR', title = 'MTTR anual') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

# Histograma del Breakdown
ggplot(indicadores, aes(x = BREAKDOWN)) +
  geom_histogram() +
  facet_wrap(~AÑO) +
  labs(x='Breakdown', y = 'Frecuencia', title = 'Breakdown') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

df2020 <- indicadores[indicadores$AÑO == '2018', ]

indicadores<- as.data.frame(indicadores)
class(indicadores)
head(indicadores)
tail(indicadores)
dim(indicadores)

######SERIES DE TIEMPO MULTIPLE######

library(TSA)

str(indicadores)
head(indicadores)
tail(indicadores)

MBTF.ts <- ts(indicadores[,5], start = 2016, freq = 51)
MTTR.ts <- ts(indicadores[, 6], start = 2016, freq = 51)
BREAKDOWN.ts <- ts(indicadores[,7], start = 2016, freq = 51)

MBTF <- MBTF.ts
MTTR <- MTTR.ts
BREAKDOWN <- BREAKDOWN.ts

plot(cbind(MBTF,MTTR, BREAKDOWN), 
     main = "INDICADORES DE MANTENIMIENTO", 
     xlab = "Tiempo",
     sub = "Enero de 2016- Diciembre de 2020")

#DescomposiciÃ³n de series (BREAKDOWN)


BREAKDOWN.ts <- ts(indicadores[, 3], start = 2016, freq = 51)

#Modelo Aditivo

BREAKDOWN.decom.A <- decompose(BREAKDOWN.ts)

plot(BREAKDOWN.decom.A, xlab = "Tiempo", 
     sub = "DescomposiciÃ³n del BREAKDOWN")

Componentes

Tendencia <- BREAKDOWN.decom.A$trend
Estacionalidad <- BREAKDOWN.decom.A$seasonal
Aleatorio <- BREAKDOWN.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de BREAKDOWN", 
        ylab = "TIEMPO PRODUCTIVO USADO EN FALLAS", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[100] + Estacionalidad[100] + Aleatorio[100]
BREAKDOWN.ts[100]

####PREDICCION Y ANALISIS DE BREAKDOWN####

install.packages("forecast")
install.packages("tseries")
library(tseries)
library(forecast)

head(indicadores)

BREAKDOWN.ts <- ts(indicadores[,7], start = 2016, freq = 51)
adf.test(BREAKDOWN.ts)     #Segun la prueba de Dickey-Fuller el p-value esta por debajo de 0.05, por lo que se puede especular que es un modelo estacionario
ndiffs(BREAKDOWN.ts)       #Aun asi se sugiere hacer una diferenciacion para acercarnos mas a un modelo estacionario.

plot(BREAKDOWN.ts, 
     main = "BREAKDOWN", 
     xlab = "Tiempo",
     sub = "Enero de 2016- Diciembre de 2020")      

seasonplot(BREAKDOWN.ts, col = rainbow(5), year.labels = TRUE,
           main = "Comparacion del BREAKDOWN por aÃ±o",
           xlab = "Semanas",
           ylab = "Tiempo productivo usado en fallas")

#FUNCION DE AUTOCORRELACION

acf(BREAKDOWN.ts)     #Se puede apreciar en el correlograma que no es del todo estacionaria

#Usar diferenciacion (Primera diferencia del BREAKDOWN)

seriedif<- diff(BREAKDOWN.ts)   
plot(seriedif)
acf(seriedif)
ndiffs(seriedif)                  #Ya es estacionaria, debido a que no sugiere hacer otra diferenciacion
adf.test(seriedif, alternative= "stationary") #Esto se confirma al realizar de nuevo la prueba de Dickey-Fuller y obtener un p-value de 0.01

#MODELO ARIMA

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(seriedif)
pacf(seriedif)
acf(ts(seriedif, frequency=1))             #Presenta 2 media moviles (rezagos)              
pacf(ts(seriedif, frequency=1))            #Presenta 6 autoregresivos
dev.off()

modelo1<- arima(BREAKDOWN.ts,order=c(3,2,2))     #Ajustamos nuestro modelo de acuerdo a los valores obtenidos anteriormente
summary(modelo1)
tsdiag(modelo1)                                  #Existe ruido blanco, ya que el p-value en la prueba Ljung Box es mayor a 0.5, por lo que existe ruido blanco
Box.test(residuals(modelo1),type="Ljung-Box")   #Comprobamos que el p-value>0.05


error<- residuals(modelo1)
plot(error)                                     #La media del error es 0, por lo que se comprueba la verasimilidad  de la arima.

#Pronosticos Arima

install.packages("quantmod")
library(quantmod)

pronostico<- forecast::forecast(modelo1, h=51)
pronostico
plot(pronostico)
