#import library
library(tidyverse)
library(datasets)
data(package=.packages(all.available=TRUE))
data <- longley  
head(data)
str(data)
summary(data)

summary_stats <- longley %>% 
  summarise(across(where(is.numeric), list(count = ~length(.), stdev = ~sd(., na.rm = TRUE))))

print(summary_stats)

#check if there is any missing data
sapply(data, function(x) sum(is.na(x)))

#scatter plot of national GNP by year
ggplot(data, aes(x=Year, y=GNP))+
  geom_point(color='blue')

#scatter plot comparing the Employment level and national GNP
ggplot(data, aes(x=GNP, y=Employed))+
  geom_point(color='blue')

#scatter plot of amount of Armed Forces each year
ggplot(data, aes(x=Year, y=Armed.Forces))+
  geom_point(color='black')


#scatter plot of the unemployed total by year
ggplot(data, aes(x=Year, y=Unemployed))+
  geom_point(color='blue')

#histogram to show the count of GNP levels
ggplot(data, aes(x=GNP))+
  geom_histogram(bins=5, fill='skyblue', color='black')
 
#correlation matrix of the variables 
cor_matrix <- cor(data %>% select_if(is.numeric))
print(cor_matrix)
as.table(cor_matrix)
ggplot(data=as.data.frame(as.table(cor_matrix)), aes(Var1, Var2, fill=Freq))+
  geom_tile(color="white")+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1))+
  geom_text(aes(label = round(Freq, 2)), color='black', size=3)+
  theme_minimal()
