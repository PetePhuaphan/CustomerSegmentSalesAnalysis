# install packages 
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("ggmosaic")
#install.packages("readr")
#install.packages("stringr")
#install.packages("dplyr")


# Load libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(stringr)
library(dplyr)

# Read csv files
trx_data <- fread(paste0("QVI_transaction_data.csv"))
cus_data <- fread(paste0("QVI_purchase_behaviour.csv"))

# Basic checking
summary(trx_data)
str(trx_data)
head(trx_data)


# Convert DATE col from integer to correct date format
trx_data$DATE <- as.Date(as.numeric(trx_data$DATE), origin = "1899-12-30")


# Calculate the count of transactions for each unique product name in the trx_data data.table. 
# The '.N' symbol is used to count the number of rows in each group formed by the 'PROD_NAME' column.
trx_data[, .N, PROD_NAME]


# Filter out transactions from trx_data where the product quantity (PROD_QTY) exceeds 10. 
# This step aims to remove outliers with exceptionally high quantities.
trx_data[PROD_QTY > 10]

# Remove the user from data
trx_data <- trx_data[LYLTY_CARD_NBR != 226000]

# outliers removed
summary(trx_data)



# Create a sequence of dates in a data.table, spanning from July 1, 2018, to June 30, 2019, inclusive. 
# It serves as a reference for checking if there are any missing dates in 'trx_data' during a later merge.
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")

allDates

# Aggregate the transaction data in 'trx_data' to count the number of transactions (.N) for each day.
daily <- trx_data[, .N, by = DATE][order(DATE)]
daily

# Merge the 'allDates' data.table, which contains the complete sequence of dates, with the aggregated transaction data 'trx_data'.
trx_by_day <- merge(allDates, trx_data[, .N, by = DATE], all.x = T)


# Filter the merged transaction data 'trx_by_day' to identify dates where the transaction count ('N') is either missing (NA) or zero.
# This operation helps in pinpointing days with no recorded transactions in the dataset.
trx_by_day[is.na(N) | N == 0]


# There were no transaction recorded on on December 25, 2018, which is Christmas Day.
trx_by_day[DATE=="2018-12-25"]


# Optional, to fill na with 0
#trx_by_day[is.na(N), N := 0]


# Plot line chart
ggplot(trx_by_day, aes(x = DATE, y = N)) + geom_line()


# Extract the size of the product from the 'PROD_NAME' column in 'trx_data' using regular expressions.
# This is achieved by identifying patterns that match a number followed by 'G' or 'g' (indicating grams).
# The 'G' or 'g' character is then removed from this extracted string.
# Finally, the result is converted to a numeric value and stored in a new column 'PROD_SIZE'.
trx_data[, PROD_SIZE := as.numeric(gsub("[Gg]","",str_extract(PROD_NAME, "\\d+[Gg]")))]

trx_data

trx_by_size <- trx_data[, .N, PROD_SIZE][order(PROD_SIZE)]

# Plot bar chart
ggplot(trx_by_size, aes(x = PROD_SIZE, y = N)) + geom_bar(stat = "identity", width = 3) + theme_minimal() + labs(title = "Number of Transactions by PROD_SIZE", x = "PROD_SIZE (g)", y = "No. of trx")

# Plot histogram
#hist(trx_data[, PROD_SIZE])


##########
#
# cus_data
#
##########

# Basic checking
summary(cus_data)
str(cus_data)
head(cus_data)


# Count the number of rows for each unique 'LIFESTAGE' in the 'cus_data' data.table. 
cus_data[, .N, LIFESTAGE][order(-N)]

# Merge 'trx_data' with 'cus_data' to enrich the transaction data with customer information.
data <- merge(trx_data, cus_data, all.x=T)

head(data)

# Aggregate the total sales (sum of 'TOT_SALES') and AVG_QTY in the 'data' data.table, grouped by both 'LIFESTAGE' and 'PREMIUM_CUSTOMER'.
sales <- data[, .(SALES = sum(TOT_SALES), AVG_QTY = sum(PROD_QTY) / uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

sales[order(SALES)]

# Plot Sales by Lifestage and Premium Customer Type
p <- ggplot(sales, aes(x = LIFESTAGE, y = SALES, fill = PREMIUM_CUSTOMER)) + 
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sales by Lifestage and Premium Customer Type",
       x = "Lifestage",
       y = "Sales",
       fill = "Premium Customer Type")

# Display chart
p


p <- ggplot(sales, aes(x = LIFESTAGE, y = AVG_QTY, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Average Quantity by Lifestage and Premium Customer Status",
       x = "Lifestage",
       y = "Average Quantity",
       fill = "Premium Customer Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display chart
p
