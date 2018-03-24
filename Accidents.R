setwd("D:/Dropbox/Daniel/Fisica/S8/AI/Movilidad/")

# Loading libraries:
# We check if each library is installed, then loads it
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")}
library(leaflet)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
library(plyr)
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")}
library(reshape2)

#__________________________________________________________________
# READING THE DATA
#__________________________________________________________________
# Getting the data from the drive repository:
url16 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQMIfSQcls5PxV6OhU3C0hjXb39Dv4l0s9Vtz1T7KLvF21pLjTZH4PDAjXrsdSL3R2eV_UjFQCJ0jFs/pub?gid=1693786413&single=true&output=csv"
# Making the data an R dataframe
data16 <- read.csv(url16, fileEncoding = "UTF-8")
rm(url16) # Removes link from memory

#__________________________________________________________________
# EXPLORING AND CLEANING THE DATA
#__________________________________________________________________
# Exploring the contents of the data:
colnames(data16)
str(data16)
head(data16)

#######################################################################
# MISSING DATA ANLYSIS
######################
# We need to set any empty entry as missing data:
for (i in 1:ncol(data16)){
    data16[,i] <- gsub("^$",NA,data16[,i]) 
# "^$" Is a regular expression indicating that the field begins and ends with
# nothing in it
}
rm(i) # Removing the indexer of the cycle
#data16[grep("In",data16$COMUNA),]
#data16[grep("AU",data16$COMUNA),]
#data16[grep("Corregimiento de San Cristóbal",data16$COMUNA),]

# Checking for missing values:
sum(is.na(data16))
# a lot of missing data

# Checking missing data per columns:
colSums(is.na(data16))

# Checking cases with missing data on column Barrio:
data16[which(is.na(data16$BARRIO)),]
# Most of these cases seem to occur in rural areas, however,
# Since there is a lot of data, we may eliminate these cases:
data16 <- data16[-which(is.na(data16$BARRIO)),]

# Checking cases with missing data on column Barrio:
data16[which(is.na(data16$CLASE)),]
# There doesn't seem to be any pattern in these data, so we may eliminate them:
data16 <- data16[-which(is.na(data16$CLASE)),]

# Checking cases with missing data on column Diseño:
data16[which(is.na(data16$DISENO)),]
##############################################################
# First interesting finding: all cases with missing values on column diseño
# correspond to gravedad MUERTO. So this raises the question, why isn't diseño
# registered in these cases?
# We may not eliminate these data
##############################################################

# Checking again for missing values:
sum(is.na(data16))
# These are the ones in the column DISENO
# No more to do for now with missing data

###########################################################
# EXPLORING AND CLEANING VARIABLES
########################
# Checking the contents of "OBJECTID" column
length(unique(data16$OBJECTID))
# Seem to be register indicators, not too useful

# Checking the contents of "RADICADO" column
length(unique(data16$RADICADO))
# Seem to be register indicators too, not too useful either

# Checking the contents of "PERIODO" column
length(unique(data16$PERIODO))
# Data is from only 2016

# We can see the columns "OBJECTID", "RADICADO", "DIRECCION_", "PERIODO"
# do not have valuable information, so they can be deleted
data16 <- subset(data16, select = -c(OBJECTID,RADICADO,PERIODO,DIRECCION_))



#############################
# Checking the variable "DIA"
dia <- as.data.frame(table(data16$DIA))
# barplot(sort(table(data16$DIA), decreasing = T), xlab = "Dia", ylab = "Frecuencia", main = "Frecuencia de accidentes por día 2016")

ggplot(dia,aes(x=reorder(Var1,-Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por día 2016",x="Dia", y = "Frecuencia") +
    geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)
chisq.test(dia$Freq)
# Clearly, there are less accidents on sundays
rm(dia)

###############################
# Checking the varaible "CLASE"
sort(table(data16$CLASE), decreasing = T)
# One class is repeated so let us join the classes "Caida de ocupante"
data16$CLASE <- sub("Caída de Ocupante", "Caida Ocupante", data16$CLASE)
clases <- as.data.frame(table(data16$CLASE))
clases$perc <- round(clases$Freq*100/nrow(data16),1)
ggplot(clases,aes(x=reorder(Var1,-Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por clase 2016",x="Clase", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), vjust=1.1, color="black", size=3.5)
rm(clases)

##############################
# Checking the variable "TIPO_GEOCO"
head(sort(table(data16$TIPO_GEOCO), decreasing = T),20)
# Checking how many values this variable takes:
length(unique(data16$TIPO_GEOCO))
# Let us see the proportion of the most common value:
head(sort(table(data16$TIPO_GEOCO), decreasing = T),20)[1] / nrow(data16)
# This variable shows a very large variability, with many values
# taking very low frequencies, so this doesn't seem very useful 
data16 <- subset(data16,select = -TIPO_GEOCO)


#################################
# Checking the variable "DIRECCION"
# Let's see the frequencies of the most common addresses:
head(sort(table(data16$DIRECCION), decreasing = T),30)
# Let's see how many different addresses there are:
length(unique(data16$DIRECCION))
# With such a high variability, it may be better to try to use geolocalization
# instead of addresses to analyze localization of accidents
data16 <- subset(data16,select = -DIRECCION)


#############################
# Checking the variable "CBML"
head(sort(table(data16$CBML), decreasing = T),30)
# According to the webpage of the mayor's office, the CBML code is made up of 
# 8 digits, so this codes seem to be wrong, so we may dismiss this variable
data16 <- subset(data16, select = -c(CBML))

#############################
# Checking the variable "GRAVEDAD"
gravedad <- as.data.frame(table(data16$GRAVEDAD))
gravedad$perc <- round(gravedad$Freq*100/nrow(data16),1)
ggplot(gravedad,aes(x=reorder(Var1,-Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por gravedad 2016",x="Gravedad", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), vjust=1.1, color="black", size=4.5)
rm(gravedad)

##############################
# Checking the variable "BARRIO"
head(sort(table(data16$BARRIO), decreasing = T),30)
# Checking how many neighborhoods there are:
length(unique(data16$BARRIO))
# Checking the most common neighborhoods:
barrio <- head(as.data.frame(sort(table(data16$BARRIO),decreasing = T)),20)
barrio$perc <- round(barrio$Freq*100/nrow(data16),1)
ggplot(barrio,aes(x=reorder(Var1,Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por barrio 2016",x="Barrio", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), hjust=1.1, color="black", size=4.5) + 
    coord_flip()
rm(barrio)

#############################
# Checking the variable "COMUNA"
head(sort(table(data16$COMUNA), decreasing = T),30)
# Checking most common comunas
comuna <- head(as.data.frame(sort(table(data16$COMUNA),decreasing = T)),20)
comuna$perc <- round(comuna$Freq*100/nrow(data16),1)
attach(comuna)
ggplot(comuna,aes(x=reorder(Var1,Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por comuna 2016",x="Comuna", y = "Frecuencia") +
    geom_text(aes(x=Var1,y=Freq,label=paste0(Freq," = ",perc,"%")), hjust=ifelse(Freq<1500,-0.1,1.1), color="black", size=4.5) +
    coord_flip()
detach(comuna)

##############################
# Checking the variable "DISEÑO"
diseno <- as.data.frame(sort(table(data16$DISENO), decreasing = T))
diseno$perc <- round(diseno$Freq*100/nrow(data16),1)
attach(diseno)
ggplot(diseno,aes(x=reorder(Var1,Freq),y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por diseño 2016",x="Diseño", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), hjust=ifelse(Freq<6100,-0.1,1.1), color="black", size=4.5) +
    coord_flip()
detach(diseno)
rm(diseno)
# Diseño doesn't seem very useful since the most common value represents 80% of the variable
data16 <- subset(data16, select = -DISENO)

###############################
# Exploring localization of accidents:
head(data16$X)
head(data16$Y)
# We need to change commas for periods in order to use these data as numeric:
data16$X <- gsub(",",".",data16$X)
data16$Y <- gsub(",",".",data16$Y)
data16$X <- as.numeric(data16$X)
data16$Y <- as.numeric(data16$Y)

# ggplot(data16, aes(x=X,y=Y, color=factor(GRAVEDAD))) + geom_point(alpha=0.1)

#ggplot(data=subset(data16,GRAVEDAD!="MUERTO"), aes(x=X,y=Y)) + 
#    geom_point(aes(colour=factor(GRAVEDAD)),alpha=0.3,size=0.8) +
#    geom_point(data=subset(data16,GRAVEDAD=="MUERTO"),
#               aes(X,Y,colour="MUERTO"),colour="Red") +
#    scale_color_manual(values=c("Blue", "Green"))



# Map for death accidents:
icon.m <- makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'black')
mapm <- leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=subset(data16,GRAVEDAD=="MUERTO")$X,
        lat=subset(data16,GRAVEDAD=="MUERTO")$Y,
        label='MUERTO',
        icon = icon.m,
        clusterOptions = markerClusterOptions())
mapm
rm(icon.m,mapm)

# Map for hurts and damages:
icon.h <- makeAwesomeIcon(icon = 'flag', markerColor = 'purple', iconColor = 'black')
icon.d <- makeAwesomeIcon(icon = 'flag', markerColor = 'blue')

map2 <- leaflet() %>% addTiles() %>%
    addAwesomeMarkers(
        lng=subset(data16,GRAVEDAD=="HERIDO")$X,
        lat=subset(data16,GRAVEDAD=="HERIDO")$Y,
        label='HERIDO',
        icon = icon.h,
        clusterOptions = markerClusterOptions())
map2 <- map2 %>% addTiles() %>%
    addAwesomeMarkers(
        lng=subset(data16,GRAVEDAD=="SOLO DAÑOS")$X,
        lat=subset(data16,GRAVEDAD=="SOLO DAÑOS")$Y,
        label='DAÑOS',
        icon = icon.d,
        clusterOptions = markerClusterOptions())
map2
rm(icon.h,icon.d,map2)


##################################################################
# Cleaning and working with dates:
# Checking contents of column "FECHA":
head(data16$FECHA)

# We see there is an error, the data have strings that are not part of dates
# Let us check if the pattern repeats in all cases:
sum(grepl("T00:00:00.000Z",data16$FECHA))

# So the pattern repeats exactly in every entry, and can be removed
# Let us clean the dates:
data16$FECHA <- sub("T00:00:00.000Z","",data16$FECHA)

# Let us check the distribution of the dates variable
# Counting how many times every date appears:
dates.t <- as.data.frame(table(data16$FECHA))
str(dates.t)
dates.t$Var1 <- as.POSIXct(dates.t$Var1, format="%Y-%m-%d")
# Let us see on average how many accidents are there per day:
mean(dates.t$Freq)

# Let us plot the whole year:
ggplot(dates.t,aes(x=Var1,y=Freq)) + geom_line() +
    labs(title="Frecuencia de accidentes por fecha 2016",x="Fecha", y = "Frecuencia")

# Since there seems to be some periodicity, let's zoom on a month to check:
ggplot(dates.t[31:60,],aes(x=Var1,y=Freq)) + geom_line() +
    labs(title="Frecuencia de accidentes febrero 2016",x="Fecha", y = "Frecuencia")
# Evidently the periodicity corresponds to the low point of accidents on sundays
rm(dates.t)

# Now let us join this column with the hour to make a date-time column:
data16$DATE_TIME <- paste(data16$FECHA,data16$HORA)
head(data16$DATE_TIME)
tail(data16$DATE_TIME)

# Now let us convert these dates into date format:

data16$DATE_TIME <- ymd_hm(data16$DATE_TIME, tz="America/Bogota")
data16$HORA <- hour(data16$DATE_TIME)

# Let us explore distribution by hours
horas <- as.data.frame(table(data16$HORA))
horas$perc <- round(horas$Freq*100/nrow(data16),1)
# Grouping by pairs of hours:
horas2 <- data.frame(Var1=paste0(horas$Var1[1],"-",horas$Var1[2]),
                     Freq=horas$Freq[1]+horas$Freq[2])
for (i in seq(3,23,2)){
    horas2 <- rbind(horas2, data.frame(Var1=paste0(horas$Var1[i],"-",horas$Var1[i+1]),
                                       Freq=horas$Freq[i]+horas$Freq[i+1]))
}
rm(i)
horas2$perc <- round(horas2$Freq*100/nrow(data16),1)

attach(horas2)
ggplot(horas2,aes(x=Var1,y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por hora 2016",x="Hora", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), hjust=ifelse(Freq<500,-0.1,1.05), color="black", size=4) +
    coord_flip()
detach(horas2)
rm(horas,horas2)

# Plotting how the frequencies behave per month:
data16$MES <- month(data16$DATE_TIME,label = T)
mes <- as.data.frame(table(data16$MES))
mes$perc <- round(mes$Freq*100/nrow(data16),1)
levels(mes$Var1) <- rev(levels(mes$Var1))
ggplot(mes,aes(x=Var1,y=Freq)) + geom_bar(stat = "identity",fill="steelblue") +
    labs(title="Frecuencia de accidentes por mes 2016",x="Mes", y = "Frecuencia") +
    geom_text(aes(label=paste0(Freq," = ",perc,"%")), hjust=1.1, color="white", size=4) +
    coord_flip()
chisq.test(mes$Freq)
#○ removing december from test:
mes <- subset(mes, Var1!="Dec")
mes <- subset(mes, Var1!="Jul")
chisq.test(mes$Freq)
rm(mes)


# data16$DATE_TIME <- as.POSIXct(data16$DATE_TIME, format="%Y-%m-%d %I:%M %p")





#_______________________________________________________________
# BIVARIATE EXPLORATION
#_______________________________________________________________

# Explore: Comuna and neighborhood by gravedad of accident
# Explore: hours of accidentality by gravedad of accident:
#   Might do a bar plot type with one-hour bins

# We need to build contingency tables to explore the distribution of gravedad by month

# Exploring gravedad by several variables
###############
# MES # for log scale: scale_y_log10() +
# Muertes
M_G_M <- count(subset(data16,GRAVEDAD=="MUERTO"), c("MES","GRAVEDAD"))
M_G_M$perc <- round(M_G_M$freq*100/sum(M_G_M$freq),1)
# M_G$perc <- round(M_G$freq*100/nrow(data16),2) # count percents
ggplot(data = M_G_M, aes(x=MES,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, color="white", size=4) +
  labs(title="Frecuencia de muertes en accidentes por mes 2016",x="Mes", y = "Frecuencia") +
  coord_flip()
chisq.test(M_G_M$freq)
rm(M_G_M)

# No muertes
M_G_NM <- count(subset(data16,GRAVEDAD!="MUERTO"), c("MES","GRAVEDAD"))
M_G_NM$perc <- round(M_G_NM$freq*100/sum(M_G_NM$freq),1)
attach(M_G_NM)
ggplot(data = M_G_NM, aes(x=MES,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, vjust=ifelse(GRAVEDAD=="HERIDO",1.1,-0.4), color="white", size=3) +
  labs(title="Frecuencia de heridos y da?os en accidentes por mes 2016",x="Mes", y = "Frecuencia") +
  coord_flip()
detach(M_G_NM)
M_G_NM_test <- dcast(M_G_NM, MES~GRAVEDAD,value.var = "freq")
M_G_NM_test <- subset(M_G_NM_test, select = -MES)
chisq.test(M_G_NM_test)
rm(M_G_NM,M_G_NM_test)


###############
# DIA
# Muertes
D_G_M <- count(subset(data16,GRAVEDAD=="MUERTO"), c("DIA","GRAVEDAD"))
D_G_M$perc <- round(D_G_M$freq*100/sum(D_G_M$freq),1)
# M_G$perc <- round(M_G$freq*100/nrow(data16),2) # count percents
ggplot(data = D_G_M, aes(x=DIA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, color="white", size=4) +
  labs(title="Frecuencia de muertes en accidentes por dia 2016",x="Dia", y = "Frecuencia") +
  coord_flip()
chisq.test(D_G_M$freq)
sum(D_G_M$freq)
rm(D_G_M)

#No muertes
D_G_NM <- count(subset(data16,GRAVEDAD!="MUERTO"), c("DIA","GRAVEDAD"))
D_G_NM$perc <- round(D_G_NM$freq*100/sum(D_G_NM$freq),1)
attach(D_G_NM)
ggplot(data = D_G_NM, aes(x=DIA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, vjust=ifelse(GRAVEDAD=="HERIDO",1.3,-0.5), color="white", size=3.5) +
  labs(title="Frecuencia de heridos y da?os en accidentes por dia 2016",x="Dia", y = "Frecuencia") +
  coord_flip()
detach(D_G_NM)
D_G_NM_test <- dcast(D_G_NM, DIA~GRAVEDAD,value.var = "freq")
D_G_NM_test <- subset(D_G_NM_test, select = -DIA)
chisq.test(D_G_NM_test)
rm(D_G_NM)


###############
# HORA
# Muertes
H_G_M <- count(subset(data16,GRAVEDAD=="MUERTO"), c("HORA","GRAVEDAD"))
H_G_M$perc <- round(H_G_M$freq*100/sum(H_G_M$freq),1)

H_G_M2 <- data.frame(HORA=paste0(H_G_M$HORA[1],"-",H_G_M$HORA[2]),
                     freq=H_G_M$freq[1]+H_G_M$freq[2])
for (i in seq(3,23,2)){
  H_G_M2 <- rbind(H_G_M2, data.frame(HORA=paste0(H_G_M$HORA[i],"-",H_G_M$HORA[i+1]),
                                     freq=H_G_M$freq[i]+H_G_M$freq[i+1]))
}
rm(i)
H_G_M2$perc <- round(H_G_M2$freq*100/sum(H_G_M2$freq),1)
H_G_M2$GRAVEDAD <- rep("MUERTO",nrow(H_G_M2))
# M_G$perc <- round(M_G$freq*100/nrow(data16),2) # count percents
ggplot(data = H_G_M2, aes(x=HORA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, color="white", size=4) +
  labs(title="Frecuencia de muertes en accidentes por hora 2016",x="Hora", y = "Frecuencia") +
  coord_flip()
rm(H_G_M,H_G_M2)

#No muertes
H_G_NM <- count(subset(data16,GRAVEDAD!="MUERTO"), c("HORA","GRAVEDAD"))
H_G_NM$perc <- round(H_G_NM$freq*100/sum(H_G_NM$freq),1)

H_G_NM2 <- data.frame(HORA=paste0(H_G_NM$HORA[1],"-",H_G_NM$HORA[3]),
                     freq=H_G_NM$freq[1]+H_G_NM$freq[3],
                     GRAVEDAD=H_G_NM$GRAVEDAD[1])
for (i in c(2,5,6,9,10,13,14,17,18,21,22,25,26,29,30,33,34,37,38,41,42,45,46)){
  H_G_NM2 <- rbind(H_G_NM2, data.frame(HORA=paste0(H_G_NM$HORA[i],"-",H_G_NM$HORA[i+2]),
                                       freq=H_G_NM$freq[i]+H_G_NM$freq[i+2],
                                       GRAVEDAD=H_G_NM$GRAVEDAD[i]))
}
rm(i)
H_G_NM2$perc <- round(H_G_NM2$freq*100/sum(H_G_NM2$freq),1)

attach(H_G_NM2)
ggplot(data = H_G_NM2, aes(x=HORA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=ifelse(freq<350,-0.1,1.05), vjust=ifelse(GRAVEDAD=="HERIDO",0.9,-0.3), color=ifelse(freq<350,"black","white"), size=3.5) +
  labs(title="Frecuencia de heridos y da?os en accidentes por hora 2016",x="Hora", y = "Frecuencia") +
  coord_flip()
detach(H_G_NM2)
rm(H_G_NM,H_G_NM2)


###############
# COMUNA
# Muertes
C_G_M <- count(subset(data16,GRAVEDAD=="MUERTO"), c("COMUNA","GRAVEDAD"))
C_G_M$perc <- round(C_G_M$freq*100/sum(C_G_M$freq),1)
C_G_M <- arrange(C_G_M,desc(-freq))
C_G_M <- subset(C_G_M,COMUNA!="In")
C_G_M$COMUNA <- factor(C_G_M$COMUNA, levels = C_G_M$COMUNA)

attach(C_G_M)
ggplot(data = C_G_M, aes(x=COMUNA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=ifelse(freq<8,-0.1,1.05), color=ifelse(freq<8,"black","white"), size=4) +
  labs(title="Frecuencia de muertes en accidentes por comuna 2016",x="Comuna", y = "Frecuencia") +
  coord_flip()
detach(C_G_M)
rm(C_G_M)

#No muertes
C_G_NM <- count(subset(data16,GRAVEDAD!="MUERTO"), c("COMUNA","GRAVEDAD"))
C_G_NM$perc <- round(C_G_NM$freq*100/sum(C_G_NM$freq),1)
C_G_NM <- subset(C_G_NM,COMUNA!="In")
C_G_NM <- subset(C_G_NM,COMUNA!="0")
C_G_NM <- subset(C_G_NM,COMUNA!="AU")

attach(C_G_NM)
ggplot(data = C_G_NM, aes(x=COMUNA,y=freq,fill=GRAVEDAD)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  #geom_text(aes(label=paste0(freq," = ",perc,"%")), hjust=1.1, vjust=ifelse(GRAVEDAD=="HERIDO",1.2,-0.5), color="white", size=3.5) +
  labs(title="Frecuencia de heridos y daños en accidentes por comuna 2016",x="Comuna", y = "Frecuencia") +
  coord_flip()
detach(C_G_NM)
rm(C_G_NM)


bins <- as.data.frame(as.numeric(as.integer(runif(42000,1,12))))
names(bins) <- "n"
ggplot(data=bins, aes(bins$n)) + geom_bar()
bins <- table(bins$n)
chisq.test(bins)
