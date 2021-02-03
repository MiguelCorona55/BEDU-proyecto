wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(modeest)


# Limpieza de datos
df_runnin <- lapply(list.files('data', full.names = TRUE, pattern = 'RUNNIN*'), read.csv, sep = ';')
df_runnin <- do.call(bind_rows, df_runnin)

df_runnin <- df_runnin %>%
                select(DATE, RUNNING_HOURS) %>%
                mutate(DATE = as.Date(DATE, '%d/%m/%Y')) %>%
                rename(FECHA = DATE)

<<<<<<< HEAD

head(df_runnin)
tail(df_runnin)

=======
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510
df_fallas <- lapply(list.files('data', full.names = TRUE, pattern = 'FALLAS*'), read.csv, sep = ';')
df_fallas <- do.call(rbind, df_fallas)

df_fallas <- df_fallas %>%
              mutate(PARO = tolower(PARO),
                     ENCARGADO = tolower(ENCARGADO),
<<<<<<< HEAD
                     ï..FECHA = as.Date(ï..FECHA, '%d/%m/%Y'),
                     DIA = strftime(ï..FECHA, '%d'),
                     SEMANA = strftime(ï..FECHA, '%V'),
                     AÑO = strftime(ï..FECHA, '%Y')) %>%
              rename(FECHA = ï..FECHA) %>%
=======
                     TIPO_FALLA = str_to_sentence(TIPO_FALLA),
                     ?..FECHA = as.Date(?..FECHA, '%d/%m/%Y'),
                     DIA = strftime(?..FECHA, '%d'),
                     SEMANA = strftime(?..FECHA, '%V'),
                     A?O = strftime(?..FECHA, '%Y')) %>%
              rename(FECHA = ?..FECHA) %>%
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510
              subset(PARO == 'si') %>%
              drop_na(FECHA) %>%
              select(FECHA, AÑO, SEMANA, DIA,  ENCARGADO:PARO)
              
df_fallas <- df_fallas[df_fallas$TIEMPO > 10, ]

df_fallas$ENCARGADO <- df_fallas$ENCARGADO %>%
                        replace(. == 'humberto barraza', 'humberto barraza villanueva') %>%
                        replace(. == 'isay s??nchez mej??a', 'isay sanchez mejia') %>%
                        str_to_title(.)

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

<<<<<<< HEAD

indicadores_turno <- df_fallas %>%
                      group_by(A?O, SEMANA, DIA, TURNO) %>%
                      summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                      group_by(A?O, SEMANA, TURNO) %>%
                      summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                      merge(fallas_semana, by = c('A?O', 'SEMANA')) %>%
                      add_column(MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

=======
indicadores <- indicadores %>%
  replace(is.na(.) | . == Inf, 0)
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510

indicadores_encargado <- df_fallas %>%
                      group_by(AÑO, SEMANA, DIA, ENCARGADO) %>%
                      summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                      group_by(AÑO, SEMANA, ENCARGADO) %>%
                      summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                      merge(fallas_semana, by = c('A?O', 'SEMANA')) %>%
                      add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
                                 MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_encargado <- indicadores_encargado %>%
  replace(is.na(.) | . == Inf, 0)

<<<<<<< HEAD
indicadores_equipo <- df_fallas %>%
  group_by(A?O, SEMANA, DIA, ENCARGADO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(A?O, SEMANA, ENCARGADO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('A?O', 'SEMANA')) %>%
  add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
             MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

=======
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510

encargado_prom <- indicadores_encargado %>%
                  group_by(A?O, ENCARGADO) %>%
                  summarise(PROM = mean(MTTR))

indicadores_mtto <- df_fallas %>%
                    group_by(A?O, SEMANA, DIA, TIPO_MTTO) %>%
                    summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                    group_by(A?O, SEMANA, TIPO_MTTO) %>%
                    summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                    merge(fallas_semana, by = c('A?O', 'SEMANA')) %>%
                    add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
                               MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_mtto <- indicadores_mtto %>%
  replace(is.na(.) | . == Inf, 0)

indicadores_turno <- df_fallas %>%
                    group_by(A?O, SEMANA, DIA, TURNO) %>%
                    summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
                    group_by(A?O, SEMANA, TURNO) %>%
                    summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
                    merge(fallas_semana, by = c('A?O', 'SEMANA')) %>%
                    add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
                               MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_turno <- indicadores_turno %>%
  replace(is.na(.) | . == Inf, 0)


# Hipotesis de turno
indicadores_turno <- indicadores_turno %>%
                      filter(TURNO %in% c('1', '3')) %>%
                      filter(MTTR < 24)

boxplot.stats(indicadores_turno$MTTR)
ggplot(indicadores_turno, aes(x = TURNO, y = MTTR)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue') +
  labs(x='Turno', y = 'Minutos', title = 'Distribucion del MTTR por turno')

shapiro.test(indicadores_turno$MTBF)
wilcox.test(indicadores_turno$MTBF ~ indicadores_turno$TURNO)

# Hipotesis de mantenimiento
indicadores_mtto <- indicadores_mtto %>%
                    filter(TIPO_MTTO %in% c("Correctivo curativo", "Correctivo paulatino")) %>%
                    filter(MTBF < 16)


wilcox.test(indicadores_mtto$MTBF ~ indicadores_mtto$TIPO_MTTO, alternative='greater')
shapiro.test(indicadores_mtto$MTB)

ggplot(indicadores_mtto, aes(x = MTBF), ) +
  facet_wrap(~TIPO_MTTO) +
  geom_histogram(fill = 'lightblue', color = 'darkblue') +
  labs(x='Horas', y = 'Frecuencia', title = 'Distribucion del MTBF por tipo de mantenimiento')

ggplot(indicadores_mtto, aes(x = TIPO_MTTO, y = MTBF)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue') +
  labs(x='Tipo de mantenimiento', y = 'Horas', title = 'Distribucion del MTBF por tipo de mantenimiento')

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


# Fallas por equipo
equipo_freq <- df_fallas %>% 
  count(EQUIPO, name = 'FREQ')

ggplot(equipo_freq, aes(x = reorder(EQUIPO, -FREQ), y = FREQ)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Equipo', y = 'Frecuencia', title = 'Fallas por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

# MTTR por encargado
ggplot(indicadores_encargado, aes(x = ENCARGADO, y = MTTR, fill = ENCARGADO)) +
  geom_boxplot() +
  labs(x='Encargado', y = 'Minutos', title = 'Tiempo medio de respuesta (MTTR) por encargado') +
  theme(axis.text.x = element_text(angle = 90,
        vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

# MTBF y MTTR histogramas
ggplot(indicadores, aes(x = MTBF, fill = A?O)) +
  geom_histogram() +
<<<<<<< HEAD
  facet_wrap(~A?O) +
  labs(x='MTBF', y = 'Frecuencia', title = 'MTBF') +
=======
  facet_wrap(~A?O) +
  labs(x='Horas', y = 'Frecuencia', title = 'MTBF Anual') +
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

ggplot(indicadores, aes(x = MTTR, fill = A?O)) +
  geom_histogram() +
<<<<<<< HEAD
  facet_wrap(~A?O) +
  labs(x='MTTR', y = 'Frecuencia', title = 'MTTR') +
=======
  facet_wrap(~A?O) +
  labs(x='Minutos', y = 'Frecuencia', title = 'MTTR Anual') +
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')


# MTTR Semanal
ggplot(indicadores, aes(x = SEMANA, y = MTTR, color = A?O)) +
  geom_point() +
<<<<<<< HEAD
  facet_wrap(~A?O) +
  labs(x='Semana', y = 'MTTR', title = 'MTTR anual') +
=======
  facet_wrap(~A?O) +
  labs(x='Semana', y = 'Minutos', title = 'MTTR anual') +
>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

# Histograma del Breakdown
ggplot(indicadores, aes(x = BREAKDOWN)) +
  geom_histogram() +
  facet_wrap(~A?O) +
  labs(x='Breakdown', y = 'Frecuencia', title = 'Breakdown') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

<<<<<<< HEAD
df2020 <- indicadores[indicadores$A?O == '2018', ]
=======
# Top tipo de falla
tipo_falla <- df_fallas %>%
              count(TIPO_FALLA, name = 'FREQ') %>%
              filter(TIPO_FALLA != 'NA')

ggplot(tipo_falla, aes(x = reorder(TIPO_FALLA, -FREQ), y = FREQ)) +
  geom_col(fill = 'lightblue', color = 'darkblue') +
  labs(x='Falla', y = 'Frecuencia', title = 'Tipos de falla') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))



>>>>>>> 892be40c03c639c06a2816a26c40f68b327b4510

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

MBTF.ts <- ts(indicadores[,6], start = 2016, freq = 52)
MTTR.ts <- ts(indicadores[, 7], start = 2016, freq = 52)
BREAKDOWN.ts <- ts(indicadores[,8], start = 2016, freq = 52)

MBTF <- MBTF.ts
MTTR <- MTTR.ts
BREAKDOWN <- BREAKDOWN.ts

plot(cbind(MBTF,MTTR, BREAKDOWN), 
     main = "INDICADORES DE MANTENIMIENTO", 
     xlab = "Tiempo",
     sub = "Enero de 2016- Diciembre de 2020")

#Descomposición de series (BREAKDOWN)

MBTF.ts <- ts(indicadores[,6], start = 2016, freq = 52)
MTTR.ts <- ts(indicadores[, 7], start = 2016, freq = 52)
BREAKDOWN.ts <- ts(indicadores[, 8], start = 2016, freq = 52)

#Modelo Aditivo

BREAKDOWN.decom.A <- decompose(BREAKDOWN.ts)
MBTF.decom.A <- decompose(MBTF.ts)
MTTR.decom.A <- decompose(MTTR.ts)

plot(BREAKDOWN.decom.A, xlab = "Tiempo", 
     sub = "Descomposición del BREAKDOWN")

plot(MBTF.decom.A, xlab = "Tiempo", 
     sub = "Descomposición del MBTF")

plot(MTTR.decom.A, xlab = "Tiempo", 
     sub = "Descomposición del MTTR")

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

BREAKDOWN.ts <- ts(indicadores[,8], start = 2016, freq = 52)
adf.test(BREAKDOWN.ts)     #Segun la prueba de Dickey-Fuller el p-value esta por debajo de 0.05, por lo que se puede especular que es un modelo estacionario
ndiffs(BREAKDOWN.ts)      #Aun asi se sugiere hacer una diferenciacion para acercarnos mas a un modelo estacionario.

plot(BREAKDOWN.ts, 
     main = "BREAKDOWN", 
     xlab = "Tiempo",
     sub = "Enero de 2016- Diciembre de 2020")      

seasonplot(BREAKDOWN.ts, col = rainbow(5), year.labels = TRUE,
           main = "Comparacion del BREAKDOWN por año",
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
acf(ts(seriedif, frequency=1))             #Presenta 1 media moviles (rezagos)              
pacf(ts(seriedif, frequency=1))            #Presenta 3 autoregresivos
dev.off()

modelo1<- arima(BREAKDOWN.ts,order=c(3,2,1))     #Ajustamos nuestro modelo de acuerdo a los valores obtenidos anteriormente
summary(modelo1)

tsdiag(modelo1)                                  #Existe ruido blanco, ya que el p-value en la prueba Ljung Box es mayor a 0.5, por lo que existe ruido blanco

Box.test(residuals(modelo1),type="Ljung-Box")   #Comprobamos que el p-value>0.05


error<- residuals(modelo1)
plot(error)                                     #La media del error es 0, por lo que se comprueba la verasimilidad  de la arima.

#Pronosticos Arima

install.packages("quantmod")
library(forecast)

pronostico<- forecast::forecast(modelo1, h=51)
pronostico
plot(pronostico)

####HCONTRASTE DE HIPOTESIS###

indicadores_turno <- df_fallas %>%
  group_by(AÑO, SEMANA, DIA, TURNO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO, SEMANA, TURNO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
  add_column(MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

head(indicadores_turno)
tail(indicadores_turno)

indicadores_turno<-indicadores_turno %>%
  select(TURNO, MTTR) %>%
  filter(TURNO %in% c("1","3"), MTTR<24)
  
boxplot.stats(indicadores_turno$MTTR)


ggplot(indicadores_turno, aes(TURNO,MTTR, fill = TURNO, color = TURNO)) +
  geom_boxplot(alpha=0.4)

ggplot(indicadores_turno, aes(MTTR, fill = TURNO, color = TURNO)) +
  geom_density(alpha=0.2)

qqnorm(indicadores_turno$MTTR)
qqline(indicadores_turno$MTTR, col = "red")

shapiro.test(indicadores_turno$MTTR)

######################

indicadores_fallas <- df_fallas %>%
  group_by(AÑO, SEMANA, DIA, TIPO_MTTO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO, SEMANA, TIPO_MTTO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
  add_column(MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

head(indicadores_MTTO)
tail(indicadores_MTTO)

indicadores_MTTO<-indicadores_MTTO %>%
  select(TIPO_MTTO, MTTR) %>%
  filter(TIPO_MTTO %in% c("Correctivo curativo","Correctivo paulatino"))

boxplot.stats(indicadores_MTTO$MTTR)

ggplot(indicadores_MTTO, aes(TIPO_MTTO,MTBF, fill = TIPO_MTTO, color = TIPO_MTTO)) +
  geom_boxplot(alpha=0.4)

ggplot(indicadores_MTTO, aes(MTTR, fill = TIPO_MTTO, color = TIPO_MTTO)) +
  geom_density(alpha=0.2)

shapiro.test(indicadores_MTTO$MTTR)

rm(indicadores_falla)
