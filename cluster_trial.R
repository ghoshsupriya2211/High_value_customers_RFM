retail_data <- read.csv("Ecommerce.csv", stringsAsFactors = FALSE)
str(retail_data)
summary(retail_data)
# Date conversion into right format
retail_data$InvoiceDate <- as.Date(retail_data$InvoiceDate, "%d-%b-%y")
# missing data check







