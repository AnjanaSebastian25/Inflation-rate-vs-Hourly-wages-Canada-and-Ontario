getwd()
library(tidyverse)
library(dplyr)

mydata = read.csv("hourlywages_canada.csv")
typeof(mydata$REF_DATE)
mydata$REF_DATE <- as.Date(paste0("01-", mydata$REF_DATE), format = "%d-%b-%y")
mydata = data.frame(mydata$REF_DATE,mydata$GEO,mydata$North.American.Industry.Classification.System..NAICS.,mydata$VALUE)

mydata = mydata%>% filter(mydata.North.American.Industry.Classification.System..NAICS.=="Total employees, all industries")
mydata
df = mydata%>% mutate(change= mydata$mydata.VALUE-lag(mydata$mydata.VALUE,12))
df
df= df%>% mutate(percent_change = ((mydata$mydata.VALUE-lag(mydata$mydata.VALUE,12))/lag(mydata$mydata.VALUE,12))*100 )
df

mydata2 = read.csv("CPI_Canada.csv")
typeof(mydata2$REF_DATE)
mydata2$REF_DATE <- as.Date(paste0("01-", mydata2$REF_DATE), format = "%d-%b-%y")
data= data.frame(mydata2$REF_DATE,mydata2$Products.and.product.groups,mydata2$VALUE)
df1 = data%>%filter(mydata2.REF_DATE >'2018-02-01')
df1
df1 = df1%>%filter(mydata2.Products.and.product.groups =='All-items')
df1
df1 = df1 %>%  mutate(percent_change= ((df1$mydata2.VALUE - lag(df1$mydata2.VALUE,12))/lag(df1$mydata2.VALUE,12))*100)
df1
library(ggplot2)

# Convert the data frames to tibbles
df2_tib <- tibble(REF_DATE = df$mydata.REF_DATE, percent_change = df$percent_change)
df3_tib <- tibble(REF_DATE = df1$mydata2.REF_DATE, percent_change = df1$percent_change)
jpeg(filename="Infaltion vs Avg.HourlyWages_Canada.jpeg", height=1000, width=1000,  
     bg="white")

ggplot() +
  geom_line(data = df2_tib, aes(x = REF_DATE, y = percent_change, color = "Hourly Wages"), size = 1) +
  geom_line(data = df3_tib, aes(x = REF_DATE, y = percent_change, color = "Inflation Rate"), size = 1) +
  scale_color_manual(name = "Legend", values = c("Inflation Rate" = "red", "Hourly Wages" = "blue")) +
  ylim(-6, 12) +
  xlim(as.Date("2019-01-01"), as.Date("2023-04-01")) +
  ggtitle("Inflation vs Hourly Wages-Canada") +
  xlab("Date") +
  ylab("Percent_Change")+
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b-%d",limits = as.Date(c("2019-01-01", "2023-04-01")))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()
