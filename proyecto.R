wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

install.packages("tidytext")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(modeest)


# Limpieza de datos
df_runnin <- lapply(list.files('data', full.names = TRUE, pattern = 'RUNNIN*'), read.csv, sep = ';')
df_runnin <- do.call(bind_rows, df_runnin)

df_runnin <- df_runnin %>%
  select(DATE, RUNNING_HOURS) %>%
  mutate(DATE = as.Date(DATE, '%d/%m/%Y')) %>%
  rename(FECHA = DATE)

df_fallas <- lapply(list.files('data', full.names = TRUE, pattern = 'FALLAS*'), read.csv, sep = ';')
df_fallas <- do.call(rbind, df_fallas)

df_fallas <- df_fallas %>%
  mutate(PARO = tolower(PARO),
         ENCARGADO = tolower(ENCARGADO),
         TIPO_FALLA = str_to_sentence(TIPO_FALLA),
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

df_fallas <- merge(df_fallas, df_runnin, by = 'FECHA')

# Indicadores al año
fallas_año <- df_fallas %>%
  group_by(AÑO) %>%
  tally(name = 'FALLAS')

indicadores_año<- df_fallas %>%
  group_by(AÑO, SEMANA, DIA) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  add_column(FALLAS = fallas_año$FALLAS,
             MTBF = .$RUNNING_HOURS / fallas_año$FALLAS,
             MTTR = .$TIEMPO_DE_FALLAS / fallas_año$FALLAS,
             BREAKDOWN = ((.$TIEMPO_DE_FALLAS/60) / .$RUNNING_HOURS)*100)

indicadores_año <- indicadores_año %>%
  replace(is.na(.) | . == Inf, 0)

MBTF <- ts(indicadores_año[,5], start = 2016, freq = 1)
MTTR<- ts(indicadores_año[,6], start = 2016, freq = 1)
BREAKDOWN <- ts(indicadores_año[,7], start = 2016, freq = 1)


plot(cbind(MBTF,MTTR, BREAKDOWN), type='o', col = "#2B5293",
     main = "INDICADORES DE MANTENIMIENTO ANUALES", 
     xlab = "AÑOS",
     sub = "2016- 2020",
     cex.main = 2.25, cex.lab=1.5, cex.axis=1.75
)



# Indicadores SEMANALES
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

indicadores <- indicadores %>%
  replace(is.na(.) | . == Inf, 0)

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


encargado_prom <- indicadores_encargado %>%
  group_by(AÑO, ENCARGADO) %>%
  summarise(PROM = mean(MTTR))

indicadores_mtto <- df_fallas %>%
  group_by(AÑO, SEMANA, DIA, TIPO_MTTO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO, SEMANA, TIPO_MTTO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
  add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
             MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_mtto <- indicadores_mtto %>%
  replace(is.na(.) | . == Inf, 0)

indicadores_turno <- df_fallas %>%
  group_by(AÑO, SEMANA, DIA, TURNO) %>%
  summarise(HORAS = mean(RUNNING_HOURS), TIEMPO_DE_FALLAS = sum(TIEMPO)) %>%
  group_by(AÑO, SEMANA, TURNO) %>%
  summarise(RUNNING_HOURS = sum(HORAS), TIEMPO_DE_FALLAS = sum(TIEMPO_DE_FALLAS)) %>%
  merge(fallas_semana, by = c('AÑO', 'SEMANA')) %>%
  add_column(MTBF = .$RUNNING_HOURS / .$FALLAS,
             MTTR = .$TIEMPO_DE_FALLAS / .$FALLAS)

indicadores_turno <- indicadores_turno %>%
  replace(is.na(.) | . == Inf, 0)


# Hipotesis de turno
indicadores_turno <- indicadores_turno %>%
  filter(TURNO %in% c('1', '3')) %>%
  filter(MTTR < 24)

boxplot.stats(indicadores_turno$MTTR)       #Eliminamos los outliers para un mejor analisis de las distribuciones

ggplot(indicadores_turno, aes(x = MTTR), ) +
  facet_wrap(~TURNO) +
  geom_histogram(fill = '#038C7F', color = '#3034BC') +
  labs(x='Horas', y = 'Frecuencia', title = 'Distribucion del MTTR por turno')


ggplot(indicadores_turno, aes(x = TURNO, y = MTTR)) +
geom_boxplot(fill = '#2B5293', color = '#038C7F') +
  labs(x='Turno', y = 'Minutos', title = 'Distribución del MTTR por turno')+
  theme(plot.title = element_text(face="bold", hjust = 0.5, vjust=1.5, size=rel(3))) +
  theme(axis.title.x = element_text(face="bold", vjust=1, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=2.5, size=rel(2.5)))+
  theme (axis.text.x = element_text(size=rel(2.7)))+
  theme (axis.text.y = element_text( size=rel(2.7)))

shapiro.test(indicadores_turno$MTTR)      #Distribucion no es normal, p-value<.05

wilcox.test(indicadores_turno$MTTR ~ indicadores_turno$TURNO)    #Test para distribuciones no normales

# Hipotesis de mantenimiento
indicadores_mtto <- indicadores_mtto %>%
  filter(TIPO_MTTO %in% c("Correctivo curativo", "Correctivo paulatino")) %>%
  filter(MTBF < 16)

boxplot.stats(indicadores_mtto$MTBF)                  #Eliminamos los outliers para un mejor analisis de las distribuciones

ggplot(indicadores_mtto, aes(x = MTBF), ) +
  facet_wrap(~TIPO_MTTO) +
  geom_histogram(fill = '#2B5293', color = '#038C7F') +
  labs(x='Horas', y = 'Frecuencia', title = 'Distribucion del MTBF por tipo de mantenimiento')

ggplot(indicadores_mtto, aes(x = TIPO_MTTO, y = MTBF)) +
  geom_boxplot(fill = '#2B5293', color = '#038C7F') +
  labs(x='Tipo de mantenimiento', y = 'Horas', title = 'Distribucion del MTBF por tipo de mantenimiento') +
  theme(plot.title = element_text(face="bold", hjust = 0.5, vjust=1.5, size=rel(3))) +
  theme(axis.title.x = element_text(face="bold", vjust=1, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=2.5, size=rel(2.5)))+
  theme (axis.text.x = element_text(size=rel(2.7)))+
  theme (axis.text.y = element_text( size=rel(2.7)))


shapiro.test(indicadores_mtto$MTB)                          #Distribucion no es normal, p-value<.05

wilcox.test(indicadores_mtto$MTBF ~ indicadores_mtto$TIPO_MTTO, alternative='greater')  #Test para distribuciones no normales
#Distribucion del tiempo

ggplot(df_fallas, aes(x = TIEMPO)) +
  geom_histogram(fill = '#038C7F', color = '#3034BC') +
  labs(x='Tiempo (min)', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')

ggplot(df_fallas, aes(y = TIEMPO)) +
  geom_boxplot(fill = '#038C7F', color = '#3034BC') +
  labs(x='Tiempo (min)', y = 'Frecuencia', title = 'Distribucion del tiempo de fallas')


# Tiempo promedio de Falla por equipo
equipo_tiempo <- df_fallas %>% 
  group_by(EQUIPO) %>%
  summarise(TIEMPO_PROM = mean(TIEMPO))  

ggplot(equipo_tiempo, aes(x = reorder(EQUIPO, -TIEMPO_PROM), y = TIEMPO_PROM)) +
  geom_col(fill = '#038C7F', color = '#3034BC') +
  labs(x='Equipo', y = 'Tiempo (min)', title = 'Tiempo promedio de Falla por equipo') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

# Fallas por equipo
equipo_freq <- df_fallas %>% 
  count(EQUIPO, name = 'FREQ')

ggplot(equipo_freq, aes(x = reorder(EQUIPO, -FREQ), y = FREQ)) +
  geom_col(fill = '#038C7F', color = '#3034BC') +
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
ggplot(indicadores, aes(x = MTBF, fill = AÑO)) +
  geom_histogram() +
  facet_wrap(~AÑO) +
  labs(x='Horas', y = 'Frecuencia', title = 'MTBF Anual') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

ggplot(indicadores, aes(x = MTTR, fill = AÑO)) +
  geom_histogram() +
  facet_wrap(~AÑO) +
  labs(x='Minutos', y = 'Frecuencia', title = 'MTTR Anual') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')


# MTTR y MTBF Semanal
ggplot(indicadores, aes(x = SEMANA, y = MTTR, color = AÑO)) +
  geom_point() +
  facet_wrap(~AÑO) +
  labs(x='Semana', y = 'Minutos', title = 'MTTR anual') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

ggplot(indicadores, aes(x = SEMANA, y = MTBF, color = AÑO)) +
  geom_point() +
  facet_wrap(~AÑO) +
  labs(x='Semana', y = 'Minutos', title = 'MTBF anual') +
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

# Top tipo de falla
tipo_falla <- df_fallas %>%
  count(TIPO_FALLA, name = 'FREQ') %>%
  filter(TIPO_FALLA != 'NA')

ggplot(tipo_falla, aes(x = reorder(TIPO_FALLA, -FREQ), y = FREQ)) +
  geom_col(fill = '#038C7F', color = '#3034BC') +
  labs(x='Falla', y = 'Frecuencia', title = 'Tipos de falla') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))




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
MTTR.ts <- ts(indicadores[,7], start = 2016, freq = 52)
BREAKDOWN.ts <- ts(indicadores[,8], start = 2016, freq = 52)

MBTF <- MBTF.ts
MTTR <- MTTR.ts
BREAKDOWN <- BREAKDOWN.ts

plot(cbind(MBTF,MTTR, BREAKDOWN),, col = "#2B5293",
     main = "INDICADORES DE MANTENIMIENTO", 
     xlab = "TIEMPO",
     sub = "Enero de 2016- Diciembre de 2020",
     cex.main = 2.25, cex.lab=2.0, cex.axis=1.75
     )



#DescomposiciÃ³n de series (BREAKDOWN)


BREAKDOWN.ts <- ts(indicadores[,8], start = 2016, freq = 52)

#Modelo Aditivo

BREAKDOWN.decom.A <- decompose(BREAKDOWN.ts)

plot(BREAKDOWN.decom.A, col = "#025951",
     xlab = "Tiempo")

Componentes

Tendencia <- BREAKDOWN.decom.A$trend
Estacionalidad <- BREAKDOWN.decom.A$seasonal
Aleatorio <- BREAKDOWN.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), col = "#025951",
        xlab = "Tiempo", main = "BREAKDOWN", 
        ylab = "TIEMPO PRODUCTIVO USADO EN FALLAS", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[100] + Estacionalidad[100] + Aleatorio[100]
BREAKDOWN.ts[100]

####PREDICCION Y ANALISIS DE BREAKDOWN####

library(tseries)
library(forecast)

head(indicadores)

BREAKDOWN.ts <- ts(indicadores[,8], start = 2016, freq = 52)
adf.test(BREAKDOWN.ts)     #Segun la prueba de Dickey-Fuller el p-value esta por debajo de 0.05, por lo que se puede especular que es un modelo estacionario
ndiffs(BREAKDOWN.ts)       #Aun asi se sugiere hacer una diferenciacion para acercarnos mas a un modelo estacionario.

plot(BREAKDOWN.ts, col = "#030A8C",
     main = "BREAKDOWN", 
     xlab = "Años",
     ylab = "Tiempo productivo usado en fallas",
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
library(quantmod)

pronostico<- forecast::forecast(modelo1, h=52)
pronostico
plot(pronostico, col = '#2B5293',
     main = "Predicción del BREAKDOWN en 2021",
     xlab = "Tiempo",
     ylab = "BREAKDOWN", 
     cex.main = 2.25, cex.lab=29.5, cex.axis=1.75)




####  FRECUENCIA DE FALLA ANUAL

equipo_freq <- df_fallas[df_fallas$EQUIPO != '', ] %>%
  group_by(AÑO, EQUIPO) %>%
  count(EQUIPO, name = 'FREQ')

equipo_freq_top <- equipo_freq %>%
  group_by(AÑO) %>%
  top_n(10, FREQ)

equipo_freq_top <- equipo_freq_top[order(equipo_freq_top$AÑO, -equipo_freq_top$FREQ), ]
equipo_freq_top

ggplot(equipo_freq_top, aes(x = reorder_within(EQUIPO, -FREQ, AÑO), y = FREQ)) +
  geom_col(fill = '#2B5293') +
  facet_wrap(~AÑO, scale = 'free') +
  labs(x = 'EQUIPO', y = 'CANTIDAD DE FALLAS EN EL AÑO', title = 'TOP DE MÁQUINAS QUE FALLAN ANUALMENTE') +
  scale_x_reordered() +
  theme(
    text = element_text(size=12),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size=rel(1.3)
      ),
      plot.title = element_text(
        face="bold", 
        hjust = 0.5,vjust=1.5, 
        size=rel(2)
        ),
    axis.title.y = element_text(
      vjust=0.5, 
      size=rel(2)
      ),
    axis.title.x  = element_text(
      vjust=0.5, 
      size=rel(2))
      )
maquinas2020<-as.data.frame(table(equipo_freq_top$EQUIPO)) #Hacemos un conteo de frecuencia para saber qué máquinas han persistido fallando a través de los años

maquinas2020 <- maquinas2020 %>% #Seleccionamos las cinco máquinas con más repetición
   top_n(5, Freq)

ggplot(maquinas2020,aes(x=Freq,y = reorder(Var1, Freq)))+ #Graficamos
  geom_col(fill = '#2B5293')+
  labs(x = 'VECES QUE HA ESTADO EN EL TOP DE FALLAS ANUAL', y = 'EQUIPO', title = 'EQUIPOS QUE CONTINÚAN EN EL TOP ANUAL') +
  scale_y_reordered()+
  theme(
    text = element_text(size=12),
    axis.text.x = element_text(
      vjust = 0.5,
      hjust = 1,
      size=rel(1.8)
    ),
    axis.text.y = element_text(
      vjust = 0.5,
      hjust = 1,
      size=rel(1.8)
    ),
    plot.title = element_text(
      face="bold", 
      hjust = 0.5,
      vjust=1.5, size=rel(2)
      ),
    axis.title.y = element_text(
      vjust=0.5, size=rel(2)
      ),
    axis.title.x  = element_text(
      vjust=0.5, 
      size=rel(2))
  )


#### TIEMPO DE FALLA ANUAL

equipo_tiempo <- df_fallas %>% 
  group_by(AÑO, EQUIPO) %>%
  summarise(tiempof = mean(TIEMPO))

equipo_tiempo_top <- equipo_tiempo %>%
  group_by(AÑO) %>%
  top_n(10)

ggplot(equipo_tiempo_top, aes(x = reorder_within(EQUIPO, -tiempof, AÑO), y = tiempof)) +
  geom_col(fill = '#2B5293') +
  facet_wrap(~AÑO, scale = 'free') +
  labs(x='EQUIPO', y = 'TIEMPO (min)', title = 'TIEMPO PROMEDIO DE FALLA POR EQUIPO') +
  scale_x_reordered() +
  theme(
    text = element_text(size=12),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size=rel(1.3)
    ),
    plot.title = element_text(face="bold", hjust = 0.5,vjust=1.5, size=rel(2)),
    axis.title.y = element_text(vjust=0.5, size=rel(2)),
    axis.title.x  = element_text(vjust=0.5, size=rel(2))
  )
maquina_tiempo<-as.data.frame(table(equipo_tiempo_top$EQUIPO)) #Hacemos un conteo de frecuencia para saber qué máquinas siguen teniendo las fallas más largas

maquina_tiempo <- maquina_tiempo %>% #Seleccionamos las cinco máquinas con más repetición
  top_n(5, Freq)

ggplot(maquina_tiempo,aes(x=Freq,y = reorder(Var1, Freq)))+ #Graficamos
  geom_col(fill = '#2B5293')+
  labs(x = 'VECES QUE HA ESTADO EN EL TOP ANUAL', y = 'EQUIPO', title = 'EQUIPOS QUE CONTINÚAN EN EL TOP ANUAL') +
  scale_y_reordered()+
  theme(
    text = element_text(size=12),
    axis.text.x = element_text(
      vjust = 0.5,
      hjust = 1,
      size=rel(1.8)
    ),
    axis.text.y = element_text(
      vjust = 0.5,
      hjust = 1,
      size=rel(1.8)
    ),
    plot.title = element_text(
      face="bold", 
      hjust = 0.5,
      vjust=1.5, size=rel(2)
    ),
    axis.title.y = element_text(
      vjust=0.5, size=rel(2)
    ),
    axis.title.x  = element_text(
      vjust=0.5, 
      size=rel(2))
  )

