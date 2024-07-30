df<-read.csv(
  "E:/Business Intelligence/CIS6008-December 2023-Dataset/Question-(a)/Fertilizer_Soil_Data.csv"
)


df$Crop_Type <- as.numeric(factor(df$Crop_Type, levels = c
                                  ("Barley", "Corn", "Oats", "Potato", "Rice","Soybean","Wheat")))

summary(df)

is.null(df)
missing_values <- colSums(is.na(df))
print(missing_values)

dim(df) 



#Scatter Plot for crop yield & temperature
plot(df$Crop_Yield~df$Temperature,main="Crop Yield Vs Temparature",xlab="Temparature"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & temperature
cor(df$Temperature, df$Crop_Yield)
cor.test(df$Temperature,df$Crop_Yield)



#Scatter Plot for crop yield & Humidity
plot(df$Crop_Yield~df$Humidity,main="Crop Yield Vs Humidity",xlab="Humidity"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Humidity
cor(df$Humidity, df$Crop_Yield)
cor.test(df$Humidity,df$Crop_Yield)



#Scatter Plot for crop yield & Rainfall
plot(df$Crop_Yield~df$Rainfall,main="Crop Yield Vs Rainfall",xlab="Rainfall"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Rainfall
cor(df$Rainfall, df$Crop_Yield)
cor.test(df$Rainfall,df$Crop_Yield)



#Scatter Plot for crop yield & Soil pH
plot(df$Crop_Yield~df$Soil_pH,main="Crop Yield Vs Soil pH",xlab="soil pH"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Soil pH
cor(df$Soil_pH, df$Crop_Yield)
cor.test(df$Soil_pH,df$Crop_Yield)



#Scatter Plot for crop yield & Soil moisture
plot(df$Crop_Yield~df$Soil_Moisture,main="Crop Yield Vs Soil Moisture",xlab="Soil Mositure"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Soil Moisture
cor(df$Soil_Moisture, df$Crop_Yield)
cor.test(df$Soil_Moisture,df$Crop_Yield)



#Scatter Plot for crop yield & Nitrogen level
plot(df$Crop_Yield~df$Nitrogen_Level,main="Crop Yield Vs Nitrogen level",xlab="Nitrogen level"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Nitrogen level
cor(df$Nitrogen_Level, df$Crop_Yield)
cor.test(df$Nitrogen_Level,df$Crop_Yield)



#Scatter Plot for crop yield & Phosphorus level
plot(df$Crop_Yield~df$Phosphorus_Level,main="Crop Yield Vs Phosphorus Level",xlab="Phosphorus Level"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Phosphorus level
cor(df$Phosphorus_Level, df$Crop_Yield)
cor.test(df$Phosphorus_Level,df$Crop_Yield)



#Scatter Plot for crop yield & Potassium level
plot(df$Crop_Yield~df$Potassium_Level,main="Crop Yield Vs Potassium Level",xlab="Potassium Level"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Potassium level
cor(df$Potassium_Level, df$Crop_Yield)
cor.test(df$Potassium_Level,df$Crop_Yield)



#Scatter Plot for crop yield & Fertilizer Amount
plot(df$Crop_Yield~df$Fertilizer_Amount,main="Fertilizer Amount Vs Crop Yield",
     xlab="Fertilizer Amount",ylab="Crop Yield",col="blue")

#correlation for crop yield & Fertilizer Amount
cor(df$Fertilizer_Amount, df$Crop_Yield)
cor.test(df$Fertilizer_Amount,df$Crop_Yield)



#Scatter Plot for crop yield & Crop type
plot(df$Crop_Yield~df$Crop_Type,main="Crop Yield Vs Crop type",xlab="Crop Type"
     ,ylab="Crop Yield",col="blue")

#correlation for crop yield & Crop type
cor(df$Crop_Type, df$Crop_Yield)
cor.test(df$Crop_Type,df$Crop_Yield)



#Task B

#Regression Scatter Plot for crop yield & temperature
plot(df$Crop_Yield~df$Temperature,main="Crop Yield Vs Temparature",xlab="Temparature"
     ,ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Temperature),col="red")

CSPD <- lm(df$Crop_Yield~df$Temperature)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & humidity
plot(df$Crop_Yield~df$Humidity,main="Crop Yield Vs Humidity",xlab="Humidity"
     ,ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Humidity),col="red")

CSPD <- lm(df$Crop_Yield~df$Humidity)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & Rainfall
plot(df$Crop_Yield~df$Rainfall,main="Crop Yield Vs Rainfall",xlab="Rainfall"
     ,ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Rainfall),col="red")

CSPD <- lm(df$Crop_Yield~df$Rainfall)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & Soil pH
plot(df$Crop_Yield~df$Soil_pH,main="Crop Yield Vs Soil pH",xlab="soil pH"
     ,ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Soil_pH),col="red")

CSPD <- lm(df$Crop_Yield~df$Soil_pH)
CSPD
summary(CSPD)

#Regression Scatter Plot for crop yield & Soil Moisture
plot(df$Crop_Yield~df$Soil_Moisture,main="Crop Yield Vs Soil Moisture",xlab="soil Moisture"
     ,ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Soil_Moisture),col="red")

CSPD <- lm(df$Crop_Yield~df$Soil_Moisture)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & Nitrogen Level
plot(df$Crop_Yield~df$Nitrogen_Level,main="Crop Yield Vs Nitrogen Level",
     xlab="Nitrogen Level",ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Nitrogen_Level),col="red")

CSPD <- lm(df$Crop_Yield~df$Nitrogen_Level)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & Phosphorus Level
plot(df$Crop_Yield~df$Phosphorus_Level,main="Crop Yield Vs Phosphorus Level",
     xlab="Phosphorus Level",ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Phosphorus_Level),col="red")

CSPD <- lm(df$Crop_Yield~df$Phosphorus_Level)
CSPD
summary(CSPD)

#Regression Scatter Plot for crop yield & Potassium Level
plot(df$Crop_Yield~df$Potassium_Level,main="Crop Yield Vs Potassium Level",
     xlab="Potassium Level",ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Potassium_Level),col="red")

CSPD <- lm(df$Crop_Yield~df$Potassium_Level)
CSPD
summary(CSPD)


#Regression Scatter Plot for crop yield & Fertilizer Amount
plot(df$Crop_Yield~df$Fertilizer_Amount,main="Crop Yield Vs Fertilizer Amount",
     xlab="Fertilizer Amount",ylab="Crop Yield",col="blue")
abline(lm(df$Crop_Yield~df$Fertilizer_Amount),col="red")

CSPD <- lm(df$Crop_Yield~df$Fertilizer_Amount)
CSPD
summary(CSPD)



install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(df[, c("Temperature", "Humidity", "Rainfall", "Soil_pH","Soil_Moisture",
          "Nitrogen_Level", "Phosphorus_Level", "Potassium_Level","Fertilizer_Amount","Crop_Type", "Crop_Yield")])
print(correlation_matrix)

corrplot(correlation_matrix, method = "color", tl.cex =1 , cl.cex = 1, addCoef.col = "White")

#Linear Model
install.packages("caTools")
library(caTools)
split <- sample.split(df[,"Crop_Yield"], SplitRatio =0.8)
train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

lm_model <- lm(Crop_Yield ~ Temperature + Humidity + Rainfall+ Soil_pH + Soil_Moisture + Nitrogen_Level +
                 Phosphorus_Level + Potassium_Level + Fertilizer_Amount + Crop_Type, data = df)

summary(lm_model)


