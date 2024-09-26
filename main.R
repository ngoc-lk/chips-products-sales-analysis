## Install required packages
install.packages("ggmosaic")
install.packages("data.table")


## Load required libraries and datasets
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(wordcloud)

### Read all datasets
transaction_data <- QVI_transaction_data
customer_data <- QVI_purchase_behaviour


## Exploratory data analysis
head(transaction_data)
str(transaction_data)
#### I found that 'DATE variable is in num format and 'PROD_NAME' is in chr. So, I need to change the first one to date format and the second one to numeric (by labelling).
head(customer_data)
str(customer_data)
#### I found that both of two datasets have 'LYLTY_CARD_NBR' column, so it gonna be primary key there. 'LIFESTAGE' and 'PREMIUM_CUSTOMER' should be converted to numeric by labelling.

### Examining transaction data
sum(duplicated(transaction_data)) 

#### This dataset has only one duplicated row. To make it balance, I have to remove the duplicated one. 
transaction_data <- unique(transaction_data)

#### Convert 'DATE' to date format
transaction_data$DATE <- as.Date(transaction_data$DATE, origin = "1899-12-30")

#### Examining 'PROD_NAME' variable (fix words, remove digits and special characters)
product_words <- unique(transaction_data$PROD_NAME)
product_words <- gsub("&", " ", product_words) 
product_words <- gsub("\\d+[gG]", " ", product_words)
product_words <- data.table(strsplit(product_words, " "))
product_words[, V1 := lapply(V1, function(x) x[x != ""])]
product_words <- data.table(unlist(product_words))
setnames(product_words, 'words')

#### Find the most common words 
product_words |>
  dplyr::count(words, sort = TRUE) |> 
  with(wordcloud(words, n, max.words = 100))

'''
After visualizing the total word count categorized by the unique PROD_NAME, it becomes evident that there is an excessive number of SALSA products present. 
Since these are not chips items, we must eliminate all rows associated with SALSA prior to conducting our analysis.
'''

#### Remove salsa products because we are interested in the chips category
transaction_data <- as.data.table(transaction_data)
transaction_data[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transaction_data <- transaction_data[SALSA == FALSE, ][, SALSA := NULL]

#### Summarize the data to check nulls, outliers and detect it.
summary(transaction_data)
boxplot(transaction_data$PROD_NBR)
boxplot(transaction_data$PROD_QTY)
boxplot(transaction_data$PROD_QTY)

'''
There are no nulls in the columns but product quantity appears to have an outlier.
There are only two transactions where 200 packets of chips are bought in one transaction
and both of these transactions were by the same customer. 
It looks like this customer has only had the two transactions over the year and is 
not an ordinary retail customer. 
The customer might be buying chips for commercial 
purposes instead. We will remove this loyalty card number from further analysis.
'''

transaction_data <- transaction_data[!(transaction_data$PROD_QTY == 200),]

#### Re-examine transaction data
summary(transaction_data)

##### Summarize transaction count by date
transaction_by_date <- transaction_data |>
  group_by(DATE) |>
  summarise(transaction_count = n())

'''
There is only 364 rows, meaning only 364 dates which indicates a missing date. 
Lets create a sequence of dates from 1 Jul 2018 to 30 Jun 2019 and use this to create a 
chart of number of transactions over time to find the missing date.
'''

##### Create a sequence of dates from 1 July 2018 to 30 June 2019, the merge 2 this columns and find the missing value
full_dates <- data.frame(DATE = seq.Date(from = as.Date("2018-07-01"),
                                         to = as.Date("2019-06-30"),
                                         by = "day"))
transaction_by_date <- full_dates |>
  left_join(transaction_by_date, by = "DATE") |>
  replace_na(list(transaction_count = 0))  

'''{r}
We found that the missing date is on 2018-12-25. It is the Christmas day. 
'''

#### Plot transaction over time
ggplot(transaction_by_date, aes(x = DATE, y = transaction_count)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

'''
We can see that the increase in sales occurs in the lead-up to Christmas and that 
there are zero sales on Christmas day itself. This is due to shops being closed on Christmas day.

Now that we are satisfied that the data no longer has outliers, we can move on to 
creating other features such as brand of chips or pack size from PROD_NAME. We will start with pack size.

'''

#### Create pack size
transaction_data[, PACK_SIZE := parse_number(PROD_NAME)]
transaction_data[, .(Total = .N), PACK_SIZE][order(-Total)]
ggplot(transaction_data, aes(x = factor(PACK_SIZE))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Transactions by PACK_SIZE",
       x = "Pack Size (g)",
       y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

'''
The largest pack size is 380g, while the smallest is 70g. 
Based on the total quantity purchased, grouped by PACK_SIZE, customers tend to buy more frequently in the 175g and 150g sizes,
with total purchases amounting to 66,389 and 40,203 units, respectively. 
In contrast, smaller pack sizes such as 125g (1,454) and 180g (1,468) are rarely purchased.
'''

#### Create brand name
transaction_data[, BRAND_NAME := sapply(strsplit(PROD_NAME, " "), `[`, 1)]
transaction_data[BRAND_NAME %in% c("Red", "RED"), BRAND_NAME := "RRD"]

### Examining customer data
sum(duplicated(customer_data))
summary(customer_data)
customer_data <- as.data.table(customer_data)

#### Set key column
setkey(customer_data, LYLTY_CARD_NBR)
key(customer_data)
setkey(transaction_data, TXN_ID)
key(transaction_data)

#### Merge transaction data to customer data
data <- merge(transaction_data, customer_data, by = "LYLTY_CARD_NBR", all.x = TRUE)
summary(data)

'''
Great, there are no nulls! So all our customers in the transaction data has been 
accounted for in the customer dataset.
'''

### Save this dataset as a csv
fwrite(data, paste0("C:/Users/PC/Desktop/","QVI_data.csv"))


## Data analysis on customer segments
#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER

data %>%
  group_by(LIFESTAGE) %>%
  summarise(Total_Sales = sum(TOT_SALES, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LIFESTAGE, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by LIFESTAGE",
       x = "LIFESTAGE",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(Total_Sales = sum(TOT_SALES, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(PREMIUM_CUSTOMER, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Total Sales by Customer segmentation",
       x = "Customer segmentation",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = interaction(LIFESTAGE, PREMIUM_CUSTOMER), y = TOT_SALES)) +
  geom_bar(stat = "summary", fun = "sum", fill = "#009999") +
  labs(title = "Total Sales by LIFESTAGE and PREMIUM_CUSTOMER",
       x = "LIFESTAGE and Premium Customer Status",
       y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

'''
Sales are coming mainly from Budget - older families, Mainstream - young 
singles/couples, and Mainstream - retirees
'''

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customer_counts <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Number_of_Customers = n_distinct(LYLTY_CARD_NBR)) %>%
  ungroup()
ggplot(customer_counts, aes(x = interaction(LIFESTAGE, PREMIUM_CUSTOMER), y = Number_of_Customers, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Customers by LIFESTAGE and PREMIUM_CUSTOMER",
       x = "LIFESTAGE and Premium Customer Status",
       y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

'''
There are more Mainstream - young singles/couples and Mainstream - retirees who buy
chips. This contributes to there being more sales to these customer segments but 
this is not a major driver for the Budget - Older families segment.
'''

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
average_units <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Average_Units = sum(PROD_QTY, na.rm = TRUE) / n_distinct(LYLTY_CARD_NBR)) %>%
  ungroup()
ggplot(average_units, aes(x = interaction(LIFESTAGE, PREMIUM_CUSTOMER), y = Average_Units, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Units per Customer by LIFESTAGE and PREMIUM_CUSTOMER",
       x = "LIFESTAGE and Premium Customer Status",
       y = "Average Units Purchased") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##### Older families and young families in general buy more chips per customer


#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
average_price_per_unit <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Total_Sales = sum(TOT_SALES, na.rm = TRUE),
            Total_Units = sum(PROD_QTY, na.rm = TRUE)) %>%
  mutate(Average_Price_Per_Unit = Total_Sales / Total_Units) %>%
  ungroup()
ggplot(average_price_per_unit, aes(x = interaction(LIFESTAGE, PREMIUM_CUSTOMER), y = Average_Price_Per_Unit, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price per Unit by LIFESTAGE and PREMIUM_CUSTOMER",
       x = "LIFESTAGE and Premium Customer Status",
       y = "Average Price per Unit ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

'''
Mainstream midage and young singles and couples are more willing to pay more per 
packet of chips compared to their budget and premium counterparts. This may be due 
to premium shoppers being more likely to buy healthy snacks and when they buy 
chips, this is mainly for entertainment purposes rather than their own consumption.
This is also supported by there being fewer premium midage and young singles and 
couples buying chips compared to their mainstream counterparts.

As the difference in average price per unit isnt large, we can check if this difference is statistically different.
'''

#### Perform an independent t-test between mainstream vs premium and budget, mid-age and young singles/couples
# Subset data for Mid-age and Young Singles/Couples
subset_data <- data %>%
  filter(LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")) %>%
  filter(PREMIUM_CUSTOMER %in% c("Mainstream", "Premium", "Budget"))
# Mainstream vs Premium (Mid-age Singles/Couples)
t_test_midage_mainstream_premium <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                           data = subset_data %>%
                                             filter(LIFESTAGE == "MIDAGE SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Mainstream", "Premium")))
# Mainstream vs Premium (Young Singles/Couples)
t_test_young_mainstream_premium <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                          data = subset_data %>%
                                            filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Mainstream", "Premium")))
# Mainstream vs Budget (Mid-age Singles/Couples)
t_test_midage_mainstream_budget <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                          data = subset_data %>%
                                            filter(LIFESTAGE == "MIDAGE SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Mainstream", "Budget")))
# Mainstream vs Budget (Young Singles/Couples)
t_test_young_mainstream_budget <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                         data = subset_data %>%
                                           filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Mainstream", "Budget")))
# Premium vs Budget (Mid-age Singles/Couples)
t_test_midage_premium_budget <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                          data = subset_data %>%
                                            filter(LIFESTAGE == "MIDAGE SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Premium", "Budget")))

# Premium vs Budget (Young Singles/Couples)
t_test_young_premium_budget <- t.test(TOT_SALES ~ PREMIUM_CUSTOMER, 
                                         data = subset_data %>%
                                           filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER %in% c("Premium", "Budget")))
'''
The t-test results in a p-value of the unit price for mainstream, 
young and mid-age singles and couples ARE significantly higher than 
that of budget or premium, young and midage singles and couples.
'''

## Deep dive into specific customer segments for insights
mainstream_young_singles <- data %>%
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES", PREMIUM_CUSTOMER == "Mainstream")
mainstream_young_singles %>%
  group_by(BRAND_NAME) %>%
  summarise(Purchase_Count = n()) %>%
  arrange(desc(Purchase_Count))
#### The most popular brand of chips by far is Kettle chips, followd by Pringles, Doritos, and Smiths.
sales_per_month <- mainstream_young_singles %>%
  mutate(Month = format(DATE, "%Y-%m")) %>%  # Chuyển đổi cột DATE thành định dạng tháng
  group_by(Month) %>%
  summarise(Total_Sales = sum(TOT_SALES, na.rm = TRUE)) %>%
  arrange(Month)
ggplot(sales_per_month, aes(x = Month, y = Total_Sales)) +
  geom_line(group = 1, color = "green") +
  geom_point(color = "blue") +
  labs(title = "Total Sales Per Month for Mainstream - Young Singles/Couples",
       x = "Month", y = "Total Sales ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#### We see a sharp peak in December, followed by a sharp drop. It is clear that December would correspond to Christmas and summer holidays, but we should also investigate the other peaks and compare this pattern with other segments.
