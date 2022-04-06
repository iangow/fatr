#import data (can use the menu import or the following code where XXXXX is the directory to the
folders where you have saved the dataset ‘fatdata1’)
fatdata1 <- read.csv("C:/XXXXXX/XXXXXXX/XXXXXX/fatdata1.csv")
#install relevant packages that we will be using
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
install.packages("writexl")
library("writexl")
## WEEKONE get descriptive stats for ROE and Return both for the sample as a whole and then by
size groups
##define quantiles of interest
q = c(.25, .5, .75)
ROESummary <- fatdata1  %>% ungroup () %>%  select (roe) %>%
  summarise(count = n(),mean = mean(roe), median =median(roe),
            min = min(roe),max = max(roe),
            quant25 = quantile(roe, probs = q[1]), quant75 = quantile(roe, probs = q[3]), sd = sd(roe))
ROESummarySize <- fatdata1  %>% ungroup () %>% group_by(size_group)%>% select (roe) %>%
  summarise(count = n(),mean = mean(roe), median =median(roe),
            min = min(roe),max = max(roe),
            quant25 = quantile(roe, probs = q[1]), quant75 = quantile(roe, probs = q[3]), sd = sd(roe))
ReturnSummary <- fatdata1  %>% ungroup () %>%  select (return) %>%
  summarise(count = n(),mean = mean(return), median =median(return),
            min = min(return),max = max(return),
            quant25 = quantile(return, probs = q[1]), quant75 = quantile(return, probs = q[3]), sd =
              sd(return))
ReturnSummarySize <- fatdata1  %>% ungroup () %>% group_by(size_group)%>% select (return)
%>%
  summarise(count = n(),mean = mean(return), median =median(return),
            min = min(return),max = max(return),
            quant25 = quantile(return, probs = q[1]), quant75 = quantile(return, probs = q[3]), sd =
              sd(return))
## Export results to excel
write_xlsx(ROESummary, "C:/Users/mpinnuck/OneDrive - The University of Melbourne/New
folder/My Documents/ACCT30001_FinancialAccounting/ROESummary")
write_xlsx(ROESummarySize, "C:/Users/mpinnuck/OneDrive - The University of Melbourne/New
folder/My Documents/ACCT30001_FinancialAccounting/ROESummarySize")
write_xlsx(ReturnSummary, "C:/Users/mpinnuck/OneDrive - The University of Melbourne/New
folder/My Documents/ACCT30001_FinancialAccounting/ReturnSummary")
write_xlsx(fatdata1, "C:/Users/mpinnuck/OneDrive - The University of Melbourne/New folder/My
Documents/ACCT30001_FinancialAccounting/fatdata1")
## WEEKONE get histogram and density plot for ROE and Return
## get histogram for roe, return
ggplot(fatdata1, aes(roe)) +
  geom_histogram(binwidth = 0.005)+
  ggtitle("Histogram of ROE")+
  xlab("ReturnonEquity") + ylab("Frequency")
ggplot(fatdata1, aes(return)) +
  geom_histogram(binwidth = 0.005)+
  ggtitle("Histogram of Returns")+
  xlab("Returns") + ylab("Frequency")
ggplot(fatdata1, aes(x=roe)) +
  geom_density()+
  ggtitle("Density of ROE")+
  xlab("ReturnonEquity") + ylab("Density")
ggplot(fatdata1, aes(x=return)) +
  geom_density() +
  ggtitle("Density of Return")+
  xlab("Return") + ylab("Density")
##by category
ggplot(data = fatdata1, aes (x=return)) +
  geom_density() +
  facet_wrap (~size_group, nrow =4)
ggplot(data = ROEData, aes (x=Return)) +
  geom_histogram(binwidth = 0.005) +
  facet_wrap (~GIC.Groups, nrow =5)
