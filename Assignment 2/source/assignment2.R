install.packages('dplyr')
trdata<-read.csv(file.choose())
trdata


#(a)
colSums(is.na(trdata))
trdata<-data.frame(trdata)

kmdriven_median<-median(trdata$Kilometers_Driven, na.rm = TRUE)
kmdriven_median
trdata$Kilometers_Driven<- ifelse(is.na(trdata$Kilometers_Driven), kmdriven_median,trdata$Kilometers_Driven)

trdata$Seats
seatsmedian<-median(trdata$Seats, na.rm= TRUE)
trdata$Seats<- ifelse(is.na(trdata$Seats), seatsmedian,trdata$Seats
                      
#b) Remove the units from ..

trdata$Mileage <- as.character(trdata$Mileage)
trdata$Power<- as.character(trdata$Power)
trdata$Engine<- as.character(trdata$Engine)
trdata$New_Price<-as.character(trdata$New_Price)
trdata$Mileage
trdata$Mileage <- sapply(strsplit(trdata$Mileage, " "), function(x) x[1])
trdata$Power <- sapply(strsplit(trdata$Power, " "), function(x) x[1])
trdata$Engine <- sapply(strsplit(trdata$Engine, " "), function(x) x[1])
trdata$New_Price<- sapply(strsplit(trdata$New_Price, " "), function(x) x[1])
head(trdata)


#c
install.packages(caret)
library(caret)

# Sample dataframe
df <- data.frame(trdata$Fuel_Type)
df<-data.frame(trdata$Transmission)

# Create dummy variables
dummies <- dummyVars("~ .", data = df)
df
# Apply to dataframe
df_encoded <- predict(dummies, newdata = df)

df_encoded

# Convert to a dataframe
df_final <- as.data.frame(df_encoded)

#Combine with original data
df_final <- cbind(df, df_encoded)

# Print the final dataframe
print(df_final)
#View(my_data)

trdata_latest<-cbind(trdata,df_final)
trdata_latest

head(trdata_latest)

#d
library(dplyr)
trdata <- mutate(trdata, Current_Age = as.numeric(format(Sys.Date(), "%Y")) - Year)
trdata

#e

#select
selected_df <- select(trdata, Name, Year, Price, Location, Owner_Type)
#filter
filtered_df <- filter(trdata, Year == 2018, Seats==5)
#rename
renamed_df <- rename(trdata, Updated_Price=New_Price)
#mutate has already been used
trdata <- mutate(trdata, Current_Age = as.numeric(format(Sys.Date(), "%Y")) - Year)

#arrange
arranged_data <- arrange(trdata_latest, Year)
#summary
summary_data <- trdata_latest %>%
  group_by(Transmission) %>%
  summarize(Avg_Price = mean(Price), Max_Year = max(Year))
trdata_latest
