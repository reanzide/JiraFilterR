source('./R/jira1.R')
library(ggplot2)
library(ggthemes)
library(plyr)
library(dplyr)

options(scipen = 999)  # turns of scientific notations like 1e+40

filterResults [, "Quantity"] <- 1
aggregateIssueByStatus <- aggregate(x=filterResults$Quantity, by=list(filterResults$status, filterResults$squad, filterResults$priority), sum)
colnames(aggregateIssueByStatus)[1] <- "Status"
colnames(aggregateIssueByStatus)[2] <- "Squad"
colnames(aggregateIssueByStatus)[3] <- "Priority"
colnames(aggregateIssueByStatus)[4] <- "Total"

p <- ggplot(aggregateIssueByStatus, aes(x = Priority, y = Total, fill = Status, label = Total)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + labs(title="Prioritywise Bar Chart",
                                                                   subtitle="Product Defect Status",
                                                                   caption="Source: Jira - sample tickets ")
p
