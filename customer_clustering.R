# Retail customer segmentation
# Importing libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(scales)
library(plotly)
# Importing Data into R
OnlineRetailData1 <- read_excel("Ecommerce.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "date", "numeric", "numeric", 
                                             "text"))
# Removing Duplicate values
#duplicate <- which(duplicated(retail_data))
#retail_data <- retail_data[-duplicate,]
# Preprocessing - Eliminating duplicates
# observations with duplicated values
dupes <- which(duplicated(OnlineRetailData1))
# Subsetting out the duplicated values
OnlineRetailData2 <- OnlineRetailData1[-dupes,]
# Preprocessing for Missing Values
OnlineRetailData <- OnlineRetailData2[complete.cases(OnlineRetailData2),]
# Removing not useful datasets
rm(OnlineRetailData1, dupes)
rm(OnlineRetailData2)
# Showing the first 20 observations of the dataset
DT::datatable(head(OnlineRetailData, 20),
              rownames = FALSE,
              options = list(
                pageLength = 5))

# Filtering for top 10 countries by transaction
Transactions_per_Country <- OnlineRetailData %>%
  group_by(Country) %>%
  summarise('Number of Transcations' = n()) %>%
  arrange(desc(`Number of Transcations`)) %>%
  top_n(10)

names(Transactions_per_Country) <- c("Country", "Number of Transactions")
library(ggthemes)
Transaction_per_Country_Visz <- ggplot(head(Transactions_per_Country,10), aes(x=reorder(Country,-`Number of Transactions`), y=`Number of Transactions`)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = `Number of Transactions`), vjust = -0.5) +
  ggtitle('Top 10 Countries by Number of Transactions') +
  xlab('Countries') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_Country_Visz)
# Display table of transactions per Country
DT::datatable((Transactions_per_Country),
              rownames = FALSE,
              options = list(
                pageLength = 10))
# filter UK customers
UK_OnlineRetail <- OnlineRetailData %>%
  filter(Country == 'United Kingdom')
# Recency Statistics
# Lets dive into the statistics of how long customer have been inactive. 
# Creating a dataframe with unique CustomersID
Users_Recency <- UK_OnlineRetail %>% 
  group_by(CustomerID) %>%
  summarise(Last_Customer_Activity = max(InvoiceDate)) %>%
  mutate(Last_Invoice = max(Last_Customer_Activity)) 
# WORK ON RECENCY! 
Users_Recency$Recency<- round(as.numeric(difftime(Users_Recency$Last_Invoice, Users_Recency$Last_Customer_Activity , units = c("days"))))
Users_Recency <- Users_Recency %>%
  select(CustomerID, Recency)
# Recency refers to the amount of days that a customer has remained inactive. 
# From the moment of it's last purchase up to the moment of the last invoice in the dataset
# Build a histogram of Recency
print(summary(Users_Recency$Recency))
# Recency Histogram
ggplot(Users_Recency, aes(Recency)) +
  geom_histogram() +
  ylab('Number of Customers') +
  theme_minimal()
# Frequency Statistics
# Calculating the number of transaction per CustomerID
User_Frequency <- UK_OnlineRetail %>%
  group_by(CustomerID) %>%
  summarise(Frequency = n())
# Summary Statistics of Number of Purchases for each user
summary(User_Frequency$Frequency)
# Frequency Boxplot
# 1st Boxplot Graph - Customers below the 3rd Quartile
# 2nd Boxplot Graph - Customers in the 4th Quartile.
Below_3Q <- User_Frequency %>%
  filter(Frequency <= 99)
Outliers <- User_Frequency %>%
  filter(Frequency >= 500)
# Plotting first 3 Quartile
Below_3Q_Visz <- ggplot(Below_3Q, aes(CustomerID, Frequency)) +
  geom_boxplot() +
  ylab('Number of Purchases per Customer') +
  ggtitle('Purchase Frequency - First 3 Quartiles') +
  theme(axis.ticks.x = element_blank()) +
  theme_minimal() 
print(Below_3Q_Visz)
# Plotting last Quartile
Outliers_Visz <- ggplot(Outliers, aes(CustomerID, Frequency)) +
  geom_boxplot() +
  ylab('Number of Purchases per Customer') +
  scale_y_continuous(labels= scales::comma) + 
  ggtitle('Purchase Frequency - Outliers') +
  theme(axis.ticks.x = element_blank()) +
  theme_minimal()
print(Outliers_Visz)
# Monetary Value Statistics
# Calculate Revenue per CustomerID
Users_Monetary_Value <- UK_OnlineRetail %>%
  mutate(Revenue = Quantity * UnitPrice) %>%
  group_by(CustomerID) %>%
  summarise(Monetary_Value=sum(Revenue))
# Summary Statistics
summary(Users_Monetary_Value$Monetary_Value)
# Monetary Value Histograms
# 1st Histogram - Customers below the 3rd Quartile
# 2nd Histogram - Customers with revenue greater than 15k a year.
MV_3Q <- Users_Monetary_Value %>%
  filter(Monetary_Value <= 15000)
MV_Outliers <- Users_Monetary_Value %>%
  filter(Monetary_Value > 15000)
# Visualizing a histogram of revenue generated by user
MV_3Q_Visz <- ggplot(MV_3Q, aes(Monetary_Value)) +
  geom_histogram() +
  ggtitle('Revenue of Users - Below $15K') +
  ylab('Number of Users') +
  xlab('Revenue') +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma)
print(MV_3Q_Visz)
# Visualizing histogram of Revenue Outliers
Outliers_Visz <- ggplot(MV_Outliers, aes(Monetary_Value)) +
  geom_histogram() +
  ggtitle('High Revenue Users - Outliers') +
  ylab('Number of Users') +
  xlab('Revenue') +
  scale_x_continuous(labels = scales::dollar, breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000)) +
  scale_y_continuous(labels = scales::comma)
print(Outliers_Visz)
# Merging Recency, Frequency and Monetary Value. RFM
Users_RFM <- merge(Users_Recency, User_Frequency) # Merging Recency and Frequency
Users_RFM <- merge(Users_RFM, Users_Monetary_Value) # Merging Monetary Value
DT::datatable((Users_RFM),
              rownames = FALSE,
              options = list(
                pageLength = 10))
# Statistical Clustering - KMeans
# Creating Clusters based on the RFM Table using Unsupervised Statistical Learning
set.seed(415)
clusters <- kmeans(scale(Users_RFM[,2:4]), 3, nstart = 1) 
# Performing kmeans with RFM variables and creating 3 clusters. 
Users_RFM$Cluster <- as.factor(clusters$cluster) 
# Attaching the results to CustomersID to identify each customer's cluster
KMeans_Results <- Users_RFM %>%
  group_by(Cluster) %>%
  summarise('Number of Users' = n(),
            'Recency Mean' = round(mean(Recency)),
            'Frequency Mean' = scales::comma(round(mean(Frequency))),
            'Monetary Value Mean' = scales::dollar(round(mean(Monetary_Value))),
            'Cluster Revenue' = scales::dollar(sum(Monetary_Value))
  )
DT::datatable((KMeans_Results),
              rownames = FALSE) # Display cluster means to identify their value to the business
Cluster_size_visz <- ggplot(KMeans_Results, aes(Cluster, `Number of Users`)) +
  geom_text(aes(label = `Number of Users`), vjust = -0.3) +
  geom_bar(aes(fill=Cluster), stat='identity') +
  ggtitle('Number of Users per Cluster') + 
  xlab("Cluster Number") +
  theme_classic()
print(Cluster_size_visz)
# Extra Segmentation - Hiererchical Clustering
library(rpart)
library(rpart.plot)
Cluster_3_Tree <- Users_RFM %>%
  filter(Cluster == '3') %>%
  select(Frequency, Monetary_Value, Recency)

fit_tree <-rpart(Monetary_Value ~ ., 
                 data=Cluster_3_Tree,
                 method = 'anova', 
                 control= rpart.control(cp=0.0127102))
rpart.plot(fit_tree, type=1,extra=1, box.palette=c("gray","lightblue"))












