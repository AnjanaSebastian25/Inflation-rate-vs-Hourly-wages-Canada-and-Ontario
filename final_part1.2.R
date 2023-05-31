getwd()
library(tidyverse)
library(dplyr)
mydata = read.csv("CPI_Ontario.csv")
typeof(mydata$REF_DATE)
mydata$REF_DATE <- as.Date(paste0("01-", mydata$REF_DATE), format = "%d-%b-%y")

df = data.frame(mydata$REF_DATE,mydata$GEO,mydata$Products.and.product.groups,mydata$VALUE)
df1 = df%>%filter(mydata.Products.and.product.groups =='All-items')
df1 = df1%>%filter(mydata.REF_DATE >'2018-02-01')
df1

df1 = df1 %>%  mutate(percent_change= ((df1$mydata.VALUE - lag(df1$mydata.VALUE,12))/lag(df1$mydata.VALUE,12))*100)
df1

plot(df1$mydata.REF_DATE,df1$change, main = "Inflation",type="o", col="red", xlab = "Date", ylab = "Value")

mydata2 = read.csv("hourlywages_ontario.csv")
typeof(mydata2$REF_DATE)
mydata2$REF_DATE <- as.Date(paste0("01-", mydata2$REF_DATE), format = "%d-%b-%y")
mydata2 = data.frame(mydata2$REF_DATE,mydata2$GEO,mydata2$North.American.Industry.Classification.System..NAICS.,mydata2$VALUE)

mydata2 = mydata2%>% filter(mydata2.North.American.Industry.Classification.System..NAICS.=="Total employees, all industries")
df2= mydata2%>% mutate(percent_change = ((mydata2$mydata2.VALUE-lag(mydata2$mydata2.VALUE,12))/lag(mydata2$mydata2.VALUE,12))*100 )
df2

# Load the ggplot2 library
library(ggplot2)



# Convert the data frames to tibbles
df1_tib <- tibble(REF_DATE = as.Date(df1$mydata.REF_DATE), percent_change = df1$percent_change)
df2_tib <- tibble(REF_DATE = as.Date(df2$mydata2.REF_DATE), percent_change = df2$percent_change)


ggplot() +
  geom_line(data = df1_tib, aes(x = REF_DATE, y = percent_change, color = "Inflation Rate"), size = 1) +
  geom_line(data = df2_tib, aes(x = REF_DATE, y = percent_change, color = "Hourly Wages"), size = 1) +
  scale_color_manual(name = "Legend", values = c("Inflation Rate" = "red", "Hourly Wages" = "blue")) +
  ylim(-6, 14) +
  xlim(as.Date("2019-01-01"), as.Date("2023-12-31")) +
  ggtitle("Inflation vs Avg.Hourly Wages-Ontario") +
  xlab("Date") +
  ylab("Percent_Change")+
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b-%d",limits = as.Date(c("2019-01-01", "2023-04-01")))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


