library(arrow)
library(lubridate)
library(dplyr)
library(ggplot2)
path = "C:/Users/f9tbqno/Downloads/fraud.parquet" #Set path of Parquet File downloaded from Github repo: https://github.com/lucasweyrich958/DATA622/blob/main/sales.parquet

tx_raw = read_parquet(path)

head(tx_raw) #Have a first look at data frame

#----EDA----
tx_raw$TX_DATETIME = as.POSIXct(tx_raw$TX_DATETIME, format="%Y-%m-%d %H:%M:%S") #Make datetime column a POSIX format
tx_raw$TX_DATE = as.Date(tx_raw$TX_DATETIME) #Extract Date
tx_raw$TX_HOUR <- hour(tx_raw$TX_DATETIME) #Extract Hour

#Plot aggregate distribution over day; very nice cosine!!!
ggplot(tx_raw, aes(x = TX_HOUR)) +
  geom_histogram(binwidth = 1, fill = "#8967B3", color = "#181C14") +
  labs(title = "Aggregate Transaction Volume Across Day",
       x = "Time (Hours)",
       y = "Number of Transactions") +
  theme_minimal()

#Filter for two weeks of transactions and summarize by day and hour
time_series_summary <- tx_raw %>%
  filter(TX_DATETIME >= '2018-03-31' & TX_DATETIME <= "2018-04-10") %>%
  group_by(TX_DATE, TX_HOUR) %>%
  summarise(occurrences = n(), .groups = 'drop')
  
time_series_summary$datetime <- as.POSIXct(paste(time_series_summary$TX_DATE, time_series_summary$TX_HOUR), 
                                           format="%Y-%m-%d %H") #combine day and hour into one column

#Plot two weeks of transactions
ggplot(time_series_summary, aes(x = datetime, y = occurrences)) +
  geom_bar(stat = "identity", fill = "#CB80AB", color = "#181C14") +
  labs(title = "Transaction Volumes from 03/31 - 04/10/2018",
       x = "Datetime",
       y = "Number of Transactions") +
  scale_x_datetime(limits = c(as.POSIXct('2018-03-31'), as.POSIXct("2018-04-10"))) +
  theme_minimal()

#Plot distribution of Transaction Amounts
ggplot(tx_raw, aes(x = TX_AMOUNT)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  xlim(0, 1200) +
  theme_minimal()

#----Customer Behavior----
library(zoo)
library(egg)
library(ggpubr)
#Using Recency-Frequency-Monetary approach to engineer rolling avgs for client ids

#Compute rolling avg of customer transaction count over 1, 7, and 30 days
tx_count <- tx_raw %>%
  group_by(CUSTOMER_ID, TX_TIME_DAYS) %>%
  summarise(TRANSACTIONS = n(), .groups = 'drop') %>%  # Count transactions per day per client
  arrange(CUSTOMER_ID, TX_TIME_DAYS) %>%
  group_by(CUSTOMER_ID) %>%
  mutate( #Calculate rolling avgs
    rolling_avg_1d = rollapply(TRANSACTIONS, width = 1, FUN = mean, align = "right", fill = NA, partial = TRUE),
    rolling_avg_7d = rollapply(TRANSACTIONS, width = 7, FUN = mean, align = "right", fill = NA, partial = TRUE),
    rolling_avg_30d = rollapply(TRANSACTIONS, width = 30, FUN = mean, align = "right", fill = NA, partial = TRUE)
  )

tx_raw <- tx_raw %>%
  left_join(tx_count, by = c("CUSTOMER_ID", "TX_TIME_DAYS"))

#Compute rolling avg of customer transaction amounts over 1, 7, and 30 days
tx_raw <- tx_raw %>%
  arrange(CUSTOMER_ID, TX_TIME_DAYS) %>%
  group_by(CUSTOMER_ID) %>%
  mutate( #Calculate rolling avgs
    rolling_avg_1d_amt = rollapply(TX_AMOUNT, width = 1, FUN = mean, align = "right", fill = NA, partial = TRUE),
    rolling_avg_7d_amt = rollapply(TX_AMOUNT, width = 7, FUN = mean, align = "right", fill = NA, partial = TRUE),
    rolling_avg_30d_amt = rollapply(TX_AMOUNT, width = 30, FUN = mean, align = "right", fill = NA, partial = TRUE)
  )

#Looking at distribution
rolling_avg_1d = ggplot(tx_raw, aes(x = rolling_avg_1d)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()
rolling_avg_7d = ggplot(tx_raw, aes(x = rolling_avg_7d)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()
rolling_avg_30d = ggplot(tx_raw, aes(x = rolling_avg_30d)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()
rolling_avg_1d_amt = ggplot(tx_raw, aes(x = rolling_avg_1d_amt)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()
rolling_avg_7d_amt = ggplot(tx_raw, aes(x = rolling_avg_7d_amt)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()
rolling_avg_30d_amt = ggplot(tx_raw, aes(x = rolling_avg_30d_amt)) +
  geom_histogram(fill = "#8967B3", color = "#181C14", bins = 50) +
  theme_minimal()

arranged_dist = ggarrange(rolling_avg_1d,rolling_avg_7d,rolling_avg_30d,rolling_avg_1d_amt,rolling_avg_7d_amt,rolling_avg_30d_amt)
annotate_figure(arranged_dist, top = text_grob("Distribution of Rolling Averages", 
                                      color = "#181C14", face = "bold", size = 14))

