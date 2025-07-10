#Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

cafedata <- read.csv('dirty_cafe_sales.csv')

glimpse(cafedata)

#Nulls
anyNA(cafedata)
colSums(is.na(cafedata))

##Empty cells
sapply(cafedata, function(x) sum(is.na(x) | x == "" | x == " "))

#Duplicated
sum(duplicated(cafedata))

summary(cafedata)

#Data cleaning

#1. Renaming columns
cafedata <- cafedata %>% 
  rename(
    Transaction_ID = Transaction.ID,
    Price_Per_Unit = Price.Per.Unit,
    Total_Spent = Total.Spent,
    Payment_Method = Payment.Method,
    Transaction_Date = Transaction.Date
  )


#2. Convert empty cells, 'ERROR', and 'UNKNOWN' values to NA
cafedata <- cafedata %>%
  mutate(across(
    everything(),  
    ~na_if(trimws(.), "")  
  )) %>%
  mutate(across(
    everything(), 
    ~replace(., . %in% c("ERROR", "UNKNOWN"), NA)
  ))

#3. Convert Transaction_Date to date format 
cafedata <- cafedata %>%
  mutate(
    Transaction_Date = as.Date(Transaction_Date)
  )

#4. Create separate columns for year, month, and day
cafedata <- cafedata %>%
  mutate(
    T_Year = year(Transaction_Date),
    T_Month = month(Transaction_Date),         
    T_Day = day(Transaction_Date)
  )


#5. Convert Quantity, Price per Unit, and Total Spent to numeric type
cafedata <- cafedata %>%
  mutate(
    Quantity = as.numeric(Quantity),
    Price_Per_Unit = as.numeric(Price_Per_Unit),
    Total_Spent = as.numeric(Total_Spent)
  )

#6. Identify and fill missing values for Quantity, Price per Unit, and Total Spent
cafedata <- cafedata %>%
  mutate(
    Quantity = coalesce(Quantity, Total_Spent / Price_Per_Unit),
    Price_Per_Unit = coalesce(Price_Per_Unit, Total_Spent / Quantity),
    Total_Spent = coalesce(Total_Spent, Quantity * Price_Per_Unit)
  )

#7. Fill missing values in Payment Method and Location with ‘Unknown’
cafedata <- cafedata %>%
  mutate(
    Payment_Method = replace_na(Payment_Method, "Unknown"),
    Location = replace_na(Location, "Unknown")
  )


#8. Remove rows with NA values in the following columns
cafedata <- cafedata %>%
  drop_na(Item, Transaction_Date, T_Year, T_Month, T_Day, Quantity, Price_Per_Unit, Total_Spent)

#Save the cleaned DataFrame
write_csv(cafedata, "D:/Curso R/PARTE 3/cafe_clean.csv")

