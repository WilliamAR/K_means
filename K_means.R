#Lectura de datos####
apr14 <- read.csv(file = "uber-raw-data-apr14.csv",header = T,sep = ",")
may14 <- read.csv(file = "uber-raw-data-may14.csv",header = T,sep = ",")
jun14 <- read.csv(file = "uber-raw-data-jun14.csv",header = T,sep = ",")
jul14 <- read.csv(file = "uber-raw-data-jul14.csv",header = T,sep = ",")
aug14 <- read.csv(file = "uber-raw-data-aug14.csv",header = T,sep = ",")
sep14 <- read.csv(file = "uber-raw-data-sep14.csv",header = T,sep = ",")

#Juntar las bases#### concatenar por filas
library(dplyr)
data14 <- apr14
library(VIM)
aggr(data14) #Gráfica para ver proporción de valores faltantes

#Separar fechas
library(lubridate)
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

#Cluster con K_means####
set.seed(1)
cluster <- kmeans(data14[,2:3],5)
data14$Borough <- as.factor(cluster$cluster)


#Graficación con mapas###
install.packages("ggmap")
library(ggplot2)
library(ggmap)
library(ggmap)

NYCMap <- ggplot(data = data14,aes(x = Lat, y = Lon, color = as.factor(Borough)))
NYCMap + geom_point() + 
  geom_point(aes(x=cluster$centers[1], y=cluster$centers[6], color = "black")) +
  geom_point(aes(x=cluster$centers[2], y=cluster$centers[7], color = "black")) +
  geom_point(aes(x=cluster$centers[3], y=cluster$centers[8], color = "black")) +
  geom_point(aes(x=cluster$centers[4], y=cluster$centers[9], color = "black")) +
  geom_point(aes(x=cluster$centers[5], y=cluster$centers[10], color = "black")) 


