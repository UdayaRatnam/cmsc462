library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)

bread_basket <- read_excel("BreadBasket_DMS.xlsx")
bread_basket <- bread_basket[!(bread_basket$Item=="NONE"),]
bread_basket <- bread_basket[complete.cases(bread_basket), ]

bread_basket$Item = as.factor(bread_basket$Item)


summary(bread_basket$Transaction)
num_transactions <- nrow(bread_basket)
num_items <- length(unique(bread_basket$Item))
avg_items_per_transaction <- mean(table(bread_basket$Transaction))
item_counts <- table(bread_basket$Item)
top_items <- names(sort(item_counts, decreasing = TRUE)[1:10])

cat("Number of transactions:", num_transactions, "\n")
cat("Number of unique items:", num_items, "\n")
cat("Average items per transaction:", avg_items_per_transaction, "\n")
cat("Top 10 most popular items:\n")
cat(paste(top_items, collapse = "\n"))







#Converts character data to date. Store InvoiceDate as date in new variable
bread_basket$TransDate <- as.Date(bread_basket$Date)
#Extract time from InvoiceDate and store in another variable
bread_basket$TransTime<- format(bread_basket$Time,"%H:%M:%S")
#Convert and edit InvoiceNo into numeric
bread_basket$Transaction <- as.numeric(as.character(bread_basket$Transaction))

#get a glimpse of your data
glimpse(bread_basket)


#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(bread_basket,c("Transaction","Date"),
                         function(df1)paste(df1$Item, collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData

#set column InvoiceNo of dataframe transactionData  
transactionData$Transaction <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written

#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions("market_basket_transactions.csv", format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

tr
summary(tr)
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
#mar - A numeric vector of length 4, which sets the margin sizes in the 
#following order: bottom, left, top, and right. 
#The default is c(5.1, 4.1, 4.1, 2.1).
par(mar=c(0,10,1,.1))
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
association.rules <- apriori(tr, parameter = list(supp=0.0001, conf=0.5,maxlen=10,minlen=2))

SubRulesNoCoffee = subset(association.rules, subset = !(rhs %in% "Coffee"))
summary(association.rules)
inspect(association.rules[1:5])

summary(SubRulesNoCoffee)
inspect(SubRulesNoCoffee[1:5])


