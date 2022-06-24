#Retail Chain Store Sales Analysis
setwd("C:/AR Files/Retail")
d <- read_excel("BigMartSales.xlsx")
str(d)
#View(d)
colnames(d)
d2 <- d
str(d2)
colnames(d2) <- tolower(colnames(d2))
colnames(d2)

#dropping columns that are not necessary for analysis
d2$item_id <- NULL
d2$outlet_size <- NULL
str(d2)

colSums(is.na(d2)) 
#2410 missing values in Outlet_Size , can be dropped since other Outlet variables capture the data we need.
#1463 missing values in Item_Weight, Price is a function of weight. Therefore, needed for analysis and data is referenced through other columns.

#EDA AND DATA VIZZES

hist(d2$item_sales , breaks=300)
hist(log(d2$item_sales) , breaks=300)
d3$item_fat_content = toupper(d2$item_fat_content)
#boxplot(item_sales ~ item_visibility, data=d3)

library(ggplot2)
ggplot(d2, aes(x=item_sales, fill=item_type)) +
  geom_density(alpha = 0.3) +
  ggtitle("Item type distribution with Sales")
geom_smooth(color="red")

ggplot(d2, aes(x=item_sales, fill=outlet_type)) +
  geom_density(alpha = 0.2) +
  ggtitle("Outlet Type distribution with Sales")
geom_smooth(color="red")

ggplot(d2, aes(x=item_sales, fill=city_type)) +
  geom_density(alpha = 0.5) +
  ggtitle("City Type distribution with  Sales")
geom_smooth(color="red")

str(d2)

#factorizing char columns
d2$outlet_type <- factor(d2$outlet_type)
d2$item_fat_content <- factor(d2$item_fat_content)
d2$item_type <- factor(d2$item_type)
d2$outlet_id <- factor(d2$outlet_id)
d2$city_type <- factor(d2$city_type)


#Performance Analytics for correlation test
d4 <- d2[,-c(2,4,6,8,9)]
library(corrplot)
m <- cbind(d4)
cor(m)
corrplot(cor(m), method="number")                
pairs(d4)                                        
library("PerformanceAnalytics")
chart.Correlation(m)

summary(d2)

#checking the effect of only the outlet id on sales
mx <- lm(log(item_sales)~ outlet_id, data=d2)
summary(mx)

library(lme4)
#fixed effects model on outlet ID to check store effect
fixedm <- lm(log(item_sales)~ item_fat_content + item_visibility + item_type +item_mrp + outlet_year + city_type + outlet_type + outlet_id, data=d2)

#randomeffects model
randomem <- lmer(log(item_sales)~ item_fat_content + item_visibility + item_type +item_mrp + outlet_year + city_type + outlet_type +( 1 | outlet_id)  , data=d2, REML=FALSE)

library(stargazer)
stargazer(mx,fixedm,randomem, type="text", single.row=TRUE)

summary(randomem)
ranef(randomem)