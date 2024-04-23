install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('maps')
install.packages('mapproj')
install.packages('treemap')

library("ggplot2")
library("RColorBrewer")
library('maps')
library('mapproj')
library('treemap')

set.seed(0)

#Saving dataset into dataframe
mydata <- read.csv("C:/Users/eddie/Desktop/EM-622/archive/US_Accidents_Dec21_updated.csv")

#Initial analysis
dim(mydata) #Dimension of dataframe
head(mydata) #First few rows of dataframe
str(mydata) #Structure of dataframe

#Data Preperation
mydata1 <- mydata[sample(1:nrow(mydata), 20000), ] #Creating a subset using 20,000 random rows

#Initial analysis
dim(mydata1) #Dimension of dataframe
head(mydata1) #First few rows of dataframe
str(mydata1) #Structure of dataframe

#Removing unwanted columns
mydata2 = subset(mydata1, select = -c(ID,Start_Time, End_Time, Number, Street, Side, Country, Timezone, Description,
                                      Start_Lat, Start_Lng, End_Lat, End_Lng, Airport_Code, Weather_Timestamp, 
                                      Sunrise_Sunset, Nautical_Twilight, Astronomical_Twilight, Wind_Direction))

dim(mydata2) #Dimension of dataframe
#Identfying sum of NAs in each remaining columns
colSums(is.na(mydata2))

#replacing NA's from each row with mean of its column
mydata2$Temperature.F.[is.na(mydata2$Temperature.F.)] <- mean(mydata2$Temperature.F.,na.rm=TRUE)
mydata2$Wind_Chill.F.[is.na(mydata2$Wind_Chill.F.)] <- mean(mydata2$Wind_Chill.F.,na.rm=TRUE)
mydata2$Humidity...[is.na(mydata2$Humidity...)] <- mean(mydata2$Humidity...,na.rm=TRUE)
mydata2$Pressure.in.[is.na(mydata2$Pressure.in.)] <- mean(mydata2$Pressure.in.,na.rm=TRUE)
mydata2$Visibility.mi.[is.na(mydata2$Visibility.mi.)] <- mean(mydata2$Visibility.mi.,na.rm=TRUE)
mydata2$Wind_Speed.mph.[is.na(mydata2$Wind_Speed.mph.)] <- mean(mydata2$Wind_Speed.mph.,na.rm=TRUE)
mydata2$Precipitation.in.[is.na(mydata2$Precipitation.in.)] <- mean(mydata2$Precipitation.in.,na.rm=TRUE)

dim(mydata2) #Dimension of dataframe
#Identfying sum of NAs in each remaining columns
colSums(is.na(mydata2))

#Histogram of States
ggplot(data = mydata2) + geom_bar(aes(x = State)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Frequency of Accidents per State", y = "Number of Accidents") #Hist of States
ggplot(data = mydata2) + geom_bar(aes(x = Weather_Condition)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Frequency of Accidents per Weather Condition", y = "Number of Accidents") #Hist of Weather Conditions

#Generating Geographic Heatmap based off Traffic Severity per County
mydata3 <- mydata2
mypallete <-brewer.pal(4,"Reds") #color pallete of 4 reds
mydata3$colorBuckets <- as.numeric(cut(mydata3$Severity,c(0,1,2,3,4))) #creating new column for severity colors
leg.txt <- c("1", "2","3","4") #legend text
mydata3$colorCode <- mypallete[mydata3$colorBuckets] #color codes from colorbucket
dev.off()
map(database = "county", col = mydata3$colorCode, fill = TRUE, resolution = 0, lty = 0) #map of county by severity
map(database = "state", col = "white", fill = FALSE, add = TRUE, lty = 12) #mapping state lines
legend("bottomright", legend=leg.txt, fill = mypallete, title = "Severity Levels") #legend
title(main = "Traffic Severity by U.S. Counties", font.main = 1, cex.main = 2.3, col.main = "darkred") #title


#Scatterplot
ggplot(data = mydata2, aes(x = Severity, y = Amenity)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Amenity vs. Severity",x = "Severity Level", y = "Amenity Available")#Amenity vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Bump)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Traffic Bumps vs. Severity",x = "Severity Level", y = "Traffic Bump?")#Bump vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Crossing)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Crossing vs. Severity",x = "Severity Level", y = "Crossing?")#Crossing vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Give_Way)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Give Ways vs. Severity",x = "Severity Level", y = "Give Way?")#Give Way vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Junction)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Junctions vs. Severity",x = "Severity Level", y = "Junction?")#Junction vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = No_Exit)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "No Exits vs. Severity",x = "Severity Level", y = "No Exit?")#No Exit vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Roundabout)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Roundabout vs. Severity",x = "Severity Level", y = "Roundabout?")#Roundabout vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Stop)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Stop vs. Severity",x = "Severity Level", y = "Stop?")#Stop vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Traffic_Calming)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Traffic Calming vs. Severity",x = "Severity Level", y = "Traffic Calming?")#Traffic Calming vs. Severity
ggplot(data = mydata2, aes(x = Severity, y = Traffic_Signal)) + geom_jitter(color = "Blue", alpha = 0.2) +
  labs(title = "Traffic Signal vs. Severity",x = "Severity Level", y = "Traffic Signal?")#Traffic Signal vs. Severity

#Table Heatmap
  #Reducing columns to mean by rows values of Severity 
df1 <- aggregate( Temperature.F. ~ Severity, mydata2, mean)
df2 <- aggregate( Wind_Chill.F. ~ Severity, mydata2, mean)
df3 <- aggregate( Humidity... ~ Severity, mydata2, mean)
df4 <- aggregate( Pressure.in. ~ Severity, mydata2, mean)
df5 <- aggregate( Visibility.mi. ~ Severity, mydata2, mean)
df6 <- aggregate( Wind_Speed.mph. ~ Severity, mydata2, mean)
df7 <- aggregate( Precipitation.in. ~ Severity, mydata2, mean)
  #Merging all df by Severity
finaldf <- merge(df1, df2, by="Severity")
finaldf <- merge(finaldf, df3, by='Severity')
finaldf <- merge(finaldf, df4, by='Severity')
finaldf <- merge(finaldf, df5, by='Severity')
finaldf <- merge(finaldf, df6, by='Severity')
finaldf <- merge(finaldf, df7, by='Severity')

row.names(finaldf) <- c("Severity 1", "Severity 2", "Severity 3", "Severity 4")
newfinaldf <- finaldf[,2:8]
mode(newfinaldf) = "numeric"
heatmap(newfinaldf)

testing <- matrix(c(69.74009,61.93234,62.68719,59.13546,
                    69.17714,60.06554,57.60626,55.37892,
                    52.19572,64.23811,63.11638,66.20650,
                    28.94172,29.46454,29.50796,29.56418,
                    9.345933,9.090735,9.382335,9.122256,
                    8.309153,7.318743,8.941434,7.901538,
                    0.005336730,0.006276389,0.008730160,0.007590474), nrow=4,ncol=7)
colnames(testing) <- c("Temp(F)","WindChill(F)","Humidity%","Press(in)","Visib(mi)","WindSpd(mph)","Precip(in)")
rownames(testing) <- c("Severity 1", "Severity 2", "Severity 3", "Severity 4")
heatmap(testing)


#Treemap using index of State and Civil Twilight, variable size using Distance.mi.
treemap(mydata2, index = c("State","Severity"), vSize = "Distance.mi.", vColor = "Civil_Twilight", type = 'index', 
        fontsize.labels = c(15,10), align.labels = list(c("left", "top"), c("left", "bottom")), title = "Severity and Traffic Distance per State")

        