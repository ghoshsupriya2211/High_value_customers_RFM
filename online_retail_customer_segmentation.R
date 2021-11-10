# High Revenue Customers Identification for Online Retail Store - Exploratory Analysis
# Importing libraries 
library(readxl) # To read Excel files
library(dplyr) # Package for Data Manipulation in R 
library(ggplot2) #  Visualization Package
library(DT) # Package used to better display tables in the Rmarkdiwn
library(lubridate) # Pacakge specifically for date manipulation
library(scales) # Package for number formatting
# Loading dataset
Retail_Store <- read_excel("Ecommerce.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "numeric", "date", "numeric", "numeric", 
                                         "text"))
str(Retail_Store)
summary(Retail_Store)
# Showing the first 20 observations of the dataset
DT::datatable(head(Retail_Store, 20),
              rownames = FALSE,
              options = list(
                pageLength = 5))
# Preprocessing - Eliminating duplicates
# observations with duplicated values
duplicates <- which(duplicated(Retail_Store))
# Subsetting out the duplicated values
Retail_Store1 <- Retail_Store[-duplicates,]
# Preprocessing for Missing Values
Retail_Store_Final <- Retail_Store1[complete.cases(Retail_Store1),]
# Initial Exploratory Data Analysis
nrow(Retail_Store_Final) # 401603
table(Retail_Store_Final$Country == "United Kingdom") # 356727
table(Retail_Store_Final$Country != "United Kingdom") # 44876
prop.table(table(Retail_Store_Final$Country == "United Kingdom")) # 0.8882578
prop.table(table(Retail_Store_Final$Country != "United Kingdom")) # 0.1117422
#Dataset Summary
Retail_Store_Final %>%
  select(InvoiceNo, Quantity, InvoiceDate, UnitPrice) %>%
  summary()
# Transactions per Country
library(dplyr)
Transactions_per_Country <- Retail_Store_Final %>%
  group_by(Country) %>%
  summarise(Number_of_Transactions = n()) %>%
  arrange(-Number_of_Transactions)

DT::datatable((Transactions_per_Country),
              rownames = FALSE)
# Monthly Revenue
# Attaching variables for easy use moving forward
attach(Retail_Store_Final)
# Monthly Revenue - Multiplyng UnitPrice and Quantity to create a Revenue column
Retail_Store_Final <- Retail_Store_Final %>%
  mutate(Revenue = `UnitPrice`*`Quantity`)
# Grouping revenue by month to generate a plot
library(lubridate)
Monthly_Revenue <- Retail_Store_Final %>%
  select(InvoiceDate, Revenue) %>%
  group_by(Date=floor_date(InvoiceDate, "month")) %>% # Grouping by months in type POSIXct
  summarise(Monthly_Revenue = sum(Revenue))  # Aggregating by Revenue
# Plotting a graph line of Online Retailer Monthly Revenue
library(ggplot2)
Monthly_Revenue_Visz <- ggplot(data=Monthly_Revenue, aes(Date, Monthly_Revenue)) +
  geom_line(colour='darkblue') +
  geom_point() + 
  scale_y_continuous(labels = scales::dollar) + # Changing the Monthly_Revenue variable to Dollars
  labs(y='Revenue') + # Change y-axis name
  theme_light()
print(Monthly_Revenue_Visz)
# Revenue Percentage Change over Month
# Percentage Change 
# Calculating the percentage change 
Perc_Change <- Monthly_Revenue %>%
  mutate(Percent_Change = (Monthly_Revenue/lag(Monthly_Revenue) - 1))  # Percentage change 

# Plotting the percent change Monthly Revenue
Perc_Change_Visz <- ggplot(data=Perc_Change, aes(Date, Percent_Change)) +
  geom_line(colour='darkblue') +
  geom_point() +
  labs(y='% Change in Revenue') + # Changing y-axis name
  scale_y_continuous(labels = scales::percent)  + # Better display of numbers
  theme_light()
print(Perc_Change_Visz)
# Monthly Active Users - MAU
Unique_MU <- Retail_Store_Final %>%
  mutate(Date=floor_date(InvoiceDate, "month")) %>%
  group_by(Date)  %>%
  summarise(Unique_Active_Users=n_distinct(CustomerID)) # Selecting unique buyers a month 
# Plotting Unique Monthly Users
UM_line <- ggplot(data=Unique_MU, aes(Date, Unique_Active_Users)) +
  geom_line(colour='darkblue') +
  geom_point() +
  labs(y='Active Users') +
  scale_y_continuous(labels = scales::comma) +
  theme_light()
print(UM_line)
# Monthly Transactions
# Number of Unique Invoices a Month - Monthly Order Count
Monthly_Orders <- Retail_Store_Final %>%
  mutate(Date=floor_date(InvoiceDate, "month")) %>%
  group_by(Date)  %>%
  summarise(Invoices=n_distinct(InvoiceNo)) # Selecting unique Invoice Number a month

Monthly_Order_line <- ggplot(data=Monthly_Orders, aes(Date,Invoices)) +
  geom_line(colour='darkblue') +
  geom_point() +
  labs(y='Number of Transactions') +
  scale_y_continuous(labels = scales::comma) +
  theme_light()
print(Monthly_Order_line)
# Average Revenue per Order
# Average Revenue per Order by Month
Revenue_per_Order <- Retail_Store_Final %>%
  mutate(Date=floor_date(InvoiceDate, "month")) %>%
  group_by(Date) %>%
  summarise(Monthly_Revenue_Average = mean(Revenue))

Average_Revenue_per_Transc <- ggplot(Revenue_per_Order, aes(Date, Monthly_Revenue_Average))  +
  geom_text(aes(label = round(Monthly_Revenue_Average,2)), vjust = -0.3) +
  geom_bar(stat = 'identity')  +
  labs(y= 'Avg. Revenue per Order') +
  scale_y_continuous(labels = scales::dollar, limits = c(0,30)) +
  theme_light()
print(Average_Revenue_per_Transc)
# Revenue per Customer Type - NEW and EXISTING
First_purchase <- Retail_Store_Final %>%
  group_by(CustomerID) %>%
  mutate(Date=floor_date(InvoiceDate, "month"),First_Purchase= min(InvoiceDate), # New month column
         # and minimum purchase date column by user
         Type = case_when(InvoiceDate == First_Purchase ~'NEW', TRUE ~ 'EXISTING')) # New column 
# for NEW and EXISTING users based on date joined
New_vs_Existing_Revenue <- First_purchase %>%
  group_by(Date = floor_date(InvoiceDate, 'month'), Type) %>%
  summarise(Revenue = sum(Revenue))
# Ploting New vs Existing Revenue over month
ggplot(New_vs_Existing_Revenue, aes(Date, Revenue, col=Type)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::dollar, limits = c(0,900000), breaks = 
                       c(150000, 300000, 450000, 600000, 750000, 900000)) + theme_light()
# Monthly New Users Ratio
# Counting New Unique users per month
New_User_Ratio <-  First_purchase %>%
  filter(Type == 'NEW') %>%
  group_by(Date) %>%
  summarise(Number_New_Users = n_distinct(CustomerID))
All_Users_Month <- First_purchase %>%
  group_by(Date) %>%
  summarise(Total_Users= n_distinct(CustomerID))
New_Old_User_Ratio <- inner_join(New_User_Ratio, All_Users_Month) %>%
  mutate(Ratio = round((`Number_New_Users`/`Total_Users`)*100))
# Visualizing New Users Ratio
New_Users <- ggplot(New_Old_User_Ratio, aes(Date,Ratio))  +
  geom_text(aes(label = Ratio), vjust = -0.3) +
  geom_bar(stat = 'identity') +
  labs(y= 'User Acquisition Ratio %') +
  theme_light()
print(New_Users)
# Retention Rate Panel
# Creating a Cohort Analysis to estimate how well we retain customers/users
# Setting up a column for the year data to make it easier to focus on 2017
Retail_Store_Final$Year <- as.numeric(format(Retail_Store_Final$InvoiceDate, '%Y'))
# There is only two month previous to 2017
Cohorts_2017 <- Retail_Store_Final[Retail_Store_Final$Year==2017,]
# Selecting relevant features for the cohort
Cohorts_2017 <- Cohorts_2017 %>%
  select(CustomerID, InvoiceDate, Year)
Date_Joined <- Cohorts_2017 %>%
  group_by(CustomerID) %>%
  summarise(First_Purchase_Date = min(InvoiceDate))

# -------------------------------------------------------
# Below - Code referenced from Jerry Dormetus 
# Link to his project: https://rstudio-pubs-static.s3.amazonaws.com/365184_904c4369586e49fc8fa08adcae1d559d.html#preprocessing:_data_types

Date_Joined$Cohort <- as.numeric(format(Date_Joined$First_Purchase_Date, "%m"))
Cohorts_2017 <- merge(Cohorts_2017, Date_Joined)
# Number of days and months the customer has been active - (Making a purchase)
Cohorts_2017$Days_Active <- as.numeric(difftime(Cohorts_2017$InvoiceDate, Cohorts_2017$First_Purchase_Date, units = 'days'))
# Dividing the days by 30 to get the number of months
Cohorts_2017$Months_Active <- floor(Cohorts_2017$Days_Active/30)
# Dumping the day element from the join date column
Cohorts_2017$First_Purchase_Date <- format(Cohorts_2017$First_Purchase_Date, "%Y-%m")
# Now we remove the day element from the InvoiceDate data since
# this Cohort Analysis is based on monthly activity.
Cohorts_2017$InvoiceDate <- format(Cohorts_2017$InvoiceDate, "%Y-%m")
# Its important that we remove the day data so that we can remove
# extra observations in the months where customers have multiple
# observations in a single month due to having multiple orders in
# a single month. We'll use the function duplicated for this later

# We relabel the cohort column data to something more intuitive for the sake
# of the report consumers, then factor them since these are sequential
groups <- c("January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December")

for(i in 1:12){
  Cohorts_2017[Cohorts_2017$Cohort==i,"Cohort"] <- groups[i]
}

Cohorts_2017$Cohort <- factor(Cohorts_2017$Cohort,ordered = T,levels = groups)

# Montlhy Active Users Panel
# By excluding both columns Age_by_Day and Age_by_Month
# we're able to remove the extra monthly observations to
# avoid counting unique customer IDs multiple times in 
# any single month.

# The day and month Age variables keep us from removing
# duplicates which is why we need to exclude them both
duplicates2 <- which(duplicated(Cohorts_2017[,c(-6,-7)]))

# Removing the duplicate observations
Cohorts_2017 <- Cohorts_2017[-duplicates2,]

# Creating rows for each cohort group
# Creating columns for each value in the Age_by_Month column;0-11
# The default aggregation setup for dcast is, fun.aggregate = length
cohorts.wide <- reshape2::dcast(Cohorts_2017,Cohort~Months_Active,
                                value.var="CustomerID",
                                fun.aggregate = length)
# Cloning the output for retention and churn mixpanels
# to be used later
cw.retention <- cohorts.wide
cw.churn <- cohorts.wide
# Calculating the percentages. month number/join month number
# DT will handle the *100 and % formating.
# The sequence needs to be reversed because if the first
# column is worked on, everything else will be divided by 1.
# Instead of formatting column 0 to show 100% for each row, it seems
# more useful to leave this as the original count, showing how
# many new customers were acquired in its respective month. This
# is why the for loop ends right before column 0.
for (i in rev(3:ncol(cw.retention))){
  cw.retention[,i] <- round(cw.retention[,i]/cw.retention[,2],4)
}
rm(i)
# Cloning the retention mixpanel
retention.avgs <- cw.retention
# When calculating the column averages, 0 won't get ignored,
# which is a problem. Converting these 0 to NAs solves this issue.
retention.avgs[retention.avgs == 0.0000] <- NA
avgs.ret <- round(apply(retention.avgs[,-1],2,mean, na.rm=TRUE),4)
# We use the zero because this is a numerical vector
# Changing it after the merge can't happen due to the
# factoring of the Cohort labels
avgs.ret <- c(0,avgs.ret)
# Adding the averages row to the retention mixpanel
cw.retention <- rbind(cw.retention,avgs.ret)
# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks <- quantile(cw.retention[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155) } )

# The retention rate mixpanel
DT::datatable(cw.retention,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 13) ) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),2) %>% # We don't want column 0 in %
  formatStyle("1", fontWeight = 'bold') %>%
  formatStyle(names(cw.retention[c(-1,-2)]),color = 'white',fontWeight = 'bold', backgroundColor = styleInterval(breaks,colors))


























