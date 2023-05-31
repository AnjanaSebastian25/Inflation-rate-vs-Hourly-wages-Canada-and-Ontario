getwd()
library(tidyverse)
library(dplyr)
df1 = read.csv("march_22.csv")
df2 = read.csv("march_23.csv")
df = bind_rows(df1, df2)
df = data.frame(df$REF_DATE,df$GEO,df$Characteristics,df$VALUE)
df= df%>% filter(df.Characteristics == "15 years and over")
df = df%>% mutate(change= df$df.VALUE-lag(df$df.VALUE))
df
df = df%>% mutate(percent_change = ((df$df.VALUE-lag(df$df.VALUE))/lag(df$df.VALUE))*100 )
df
df3 = read.csv("feb_2022.csv")
df4 = read.csv("feb_2023.csv")
df5 = bind_rows(df3,df4)
df5 = data.frame(df5$REF_DATE,df5$GEO,df5$Characteristics,df5$VALUE)
df5= df5%>% filter(df5.Characteristics == "15 years and over")
df5 = df5%>% mutate(change= df5$df5.VALUE-lag(df5$df5.VALUE))
df5
df5 = df5%>% mutate(percent_change = ((df5$df5.VALUE-lag(df5$df5.VALUE))/lag(df5$df5.VALUE))*100 )
df5



mydata = read.csv("consumerpriceindex.csv")
typeof(mydata$REF_DATE)
mydata$REF_DATE<- as.Date(paste0(mydata$REF_DATE, "-01"))
data1= data.frame(mydata$REF_DATE,mydata$Products.and.product.groups,mydata$VALUE)
df = data1%>%filter(mydata.REF_DATE >'2019-02-01')
df1 = df%>%filter(mydata.Products.and.product.groups =='All-items')
df1 = df1 %>%  mutate(change= ((df1$mydata.VALUE - lag(df1$mydata.VALUE))/lag(df1$mydata.VALUE))*100)
df1
df1
plot(df1$mydata.REF_DATE,df1$change, main = "Inflation",type="o", col="red", xlab = "Date", ylab = "Value")

mydata2 = read.csv("hourlywages1.csv")
typeof(mydata2$REF_DATE)
mydata2$REF_DATE <- as.Date(mydata2$REF_DATE, format = "%d-%m-%Y")
mydata2 = data.frame(mydata2$REF_DATE,mydata2$GEO,mydata2$North.American.Industry.Classification.System..NAICS.,mydata2$VALUE)

mydata2 = mydata2%>% filter(mydata2.North.American.Industry.Classification.System..NAICS.=="Total employees, all industries")
df2= mydata2%>% mutate(percent_change = ((mydata2$mydata2.VALUE-lag(mydata2$mydata2.VALUE))/lag(mydata2$mydata2.VALUE))*100 )
df2


# Load the ggplot2 library
library(ggplot2)

# Convert the data frames to tibbles
df1_tib <- tibble(REF_DATE = df1$mydata.REF_DATE, change = df1$change)
df2_tib <- tibble(REF_DATE = df2$mydata2.REF_DATE, percent_change = df2$percent_change)


ggplot() +
  geom_line(data = df1_tib, aes(x = REF_DATE, y = change, color = "Inflation Rate"), size = 1) +
  geom_line(data = df2_tib, aes(x = REF_DATE, y = percent_change, color = "Hourly Wages"), size = 1) +
  scale_color_manual(name = "Legend", values = c("Inflation Rate" = "red", "Hourly Wages" = "blue")) +
  ylim(-6, 6) +
  xlim(as.Date("2019-01-01"), as.Date("2023-12-31")) +
  ggtitle("Inflation vs Hourly Wages-Canada(Month_over_Month)") +
  xlab("Date") +
  ylab("Value")

  



