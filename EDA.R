#----Large Dataset, Credit Card Fraud----
library(arrow)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggbreak)
library(lubridate)

set.seed(2024)
path = "C:/Users/f9tbqno/Downloads/creditcard.parquet" #Set path of Parquet File downloaded from Github repo: https://github.com/lucasweyrich958/DATA622/blob/main/sales.parquet

tx_raw = read_parquet(path)

summary(tx_raw)
tx_raw$Class = as.factor(tx_raw$Class) #Convert Class column to factor

tx_raw = tx_raw %>%
  mutate(datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seconds(Time)) #Make new column that shows datetime

ggplot(tx_raw, aes(x = Amount, fill = Class)) +
  geom_histogram(position = "dodge", bins = 60) +
  labs(title = "Histogram of Amounts by Class", x = "Amount ($)", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c('grey', 'green')) +
  xlim(0, 500)

tx_1 = tx_raw %>%
  filter(Class == 1)

ggplot(tx_1, aes(x = Amount)) +
  geom_histogram(position = "dodge", bins = 60) +
  labs(title = "Histogram of Amounts for Class Fraud", x = "Amount ($)", y = "Frequency") +
  theme_minimal()

#Correlation Heatmap
tx_raw_numeric = tx_raw %>%
  select(!c(Class, datetime))
cor_matrix = cor(tx_raw_numeric)
cor_matrix = melt(cor_matrix)

ggplot(data = cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable")

#Time-Series for Transactions
tx_transactions = tx_raw %>%
  mutate(datetime_hour = floor_date(datetime, "hour")) %>%
  group_by(datetime_hour, Class) %>%
  summarise(transaction_count = n())

tx_trans_1 = ggplot(tx_transactions, aes(x = datetime_hour, y = transaction_count, color = Class)) +
  geom_line() +
  theme_minimal() +
  labs(title = 'Fraud Txs',y = "Number of Transactions", x = "Time (Hourly)") +
  scale_y_continuous(limits = c(0, 50))

tx_trans_0 = ggplot(tx_transactions, aes(x = datetime_hour, y = transaction_count, color = Class)) +
  geom_line() +
  theme_minimal() +
  labs(title = 'Non-Fraud Txs', x = NULL, y = NULL, color = "Class") +
  scale_y_continuous(limits = c(1000, max(tx_transactions$transaction_count)))

# Combine the two plots
tx_transactions_plot = (tx_trans_0 / tx_trans_1) + plot_layout(heights = c(2, 1))
print(tx_transactions_plot)

