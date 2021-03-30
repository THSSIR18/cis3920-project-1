## -----------------------------------------------------------

##Section I. Combining datasets:

## (#1) ## 
setwd("C:/Users/Sahil Rajapkar/Desktop/STA3920(R)FOLDER")
getwd()
data1 <- read.csv("2014Data.csv")
print(data1)
dim(data1)
names(data1)
head(data1)
features = c("id","sale","units","rating","product",
             "industry","country","return.client")
data1 = data1[,features]
names(data1)
data1$year = 2014
head(data1$year)

setwd("C:/Users/Sahil Rajapkar/Desktop/STA3920(R)FOLDER")
getwd()
data2 <- read.csv("2015Data.csv")
print(data2)
dim(data2)
names(data2)
head(data2)
features = c("id","sale","units","rating","product",
             "industry","country","return.client")
data2 = data2[,features]
names(data2)
data2$year = 2015
head(data2$year)

setwd("C:/Users/Sahil Rajapkar/Desktop/STA3920(R)FOLDER")
getwd()
data3 <- read.csv("2016Data.csv")
print(data3)
dim(data3)
names(data3)
head(data3)
features = c("id","sale","units","rating","product",
             "industry","country","return.client")
data3 = data3[,features]
names(data3)
data3$year = 2016
head(data3$year)
 
## (#2) ## 
dataCombo = rbind(data1,data2,data3)
dim(dataCombo)

## ----------------------------------------------------------- ##
##Section II. Examining the dataset:
str(dataCombo)

## (#1) ## 
# After using the str() function, it looks like we are working with nine variables and nine observations

## (#2) ## 
# Convert variables to data types that makes sense

dataCombo$id = as.character(dataCombo$id) 
class(dataCombo$id) #stays the same because id is a string of characters
levels(dataCombo$id)

dataCombo$sale = as.integer(dataCombo$sale) 
class(dataCombo$sale) #stays the same because sale is an integer and not numeric
levels(dataCombo$sale)

dataCombo$units = as.integer(dataCombo$units) 
class(dataCombo$units) #stays the same because units is an integer and not numeric 
levels(dataCombo$units)

dataCombo$rating = as.numeric(dataCombo$rating) 
class(dataCombo$rating) #stays the same because units is an integer and not numeric 
levels(dataCombo$rating)

dataCombo$product = as.factor(dataCombo$product) 
class(dataCombo$product)  #changed to factor data type because it is categorical 
levels(dataCombo$product)

dataCombo$industry = as.factor(dataCombo$industry) 
class(dataCombo$industry) #changed to factor data type because it is categorical 
levels(dataCombo$industry)

dataCombo$country = as.factor(dataCombo$country) 
class(dataCombo$country) #changed to factor data type because it is categorical 

dataCombo$return.client = as.factor(dataCombo$return.client) 
class(dataCombo$return.client) #changed to factor data type because it is categorical in the sense that a 0 can represent a returning client and a 1 can represent a non-returning client (done "by category")
levels(dataCombo$return.client)

dataCombo$year = as.integer(dataCombo$year) 
class(dataCombo$year) #changed to integer data type years do not have decimals and are continuous hence they are integers
levels(dataCombo$year)

## ----------------------------------------------------------- ##
##Section III. Recording values:
levels(dataCombo$country)

## (#1) ##
dataCombo$country = as.character(dataCombo$country) # changed to character so we can == "Switzerland, Switzerland"
dataCombo$country[dataCombo$country=="Switzerland, Switzerland"] = "Switzerland"

## (#2) ## 
countries_to_keep = c("Switzerland","United States","United Kingdom")
# select rows in country that are NOT in the countries to keep list, change them to "other"
dataCombo$country[!(dataCombo$country %in% countries_to_keep)] = "other"
dataCombo$country = as.factor(dataCombo$country) # convert back to factor type
levels(dataCombo$country)

## (#3) ## 
levels(dataCombo$industry)
sum(is.na(dataCombo$industry))
dataCombo$industry[dataCombo$industry=="999"] = NA 
sum(is.na(dataCombo$industry)) # count again to see how many NAs now exist.

## ----------------------------------------------------------- ##
##Section IV. Handling missing values:

## (#1) ##
missing1 = is.na(dataCombo$id)
sum(missing1)

missing2 = is.na(dataCombo$sale)
sum(missing2)

missing3 = is.na(dataCombo$units)
sum(missing3)

missing4 = is.na(dataCombo$rating)
sum(missing4)

missing5 = is.na(dataCombo$product)
sum(missing5)

missing6 = is.na(dataCombo$industry)
sum(missing6)

missing7 = is.na(dataCombo$return.client)
sum(missing7)
  
missing8 = is.na(dataCombo$year)
sum(missing8)

missing9 = is.na(dataCombo$country)
sum(missing9)

# the following is a way to show NA values by variable (sums up everything from above and is better to reduce redundancy)
for (i in names(dataCombo))
{
  print(paste(i,":",sum(is.na(dataCombo[i])),sep=" "))
}

# (#2) ## 
missing5 = is.na(dataCombo$product)
sum(missing5)
missing5[is.na(missing5)]="Delta"

## (#3) ## after removing the whole observation and its missing values, there are 0 observations left
dataCombo = na.omit(dataCombo)
sum(is.na(dataCombo)) 

dataCombo$id = na.omit(dataCombo$id)
sum(is.na(dataCombo$id)) 

dataCombo$sale = na.omit(dataCombo$sale)
sum(is.na(dataCombo$sale))

dataCombo$units = na.omit(dataCombo$units)
sum(is.na(dataCombo$units))

dataCombo$rating = na.omit(dataCombo$rating)
sum(is.na(dataCombo$rating))

dataCombo$product = na.omit(dataCombo$product)
sum(is.na(dataCombo$product))

dataCombo$industry = na.omit(dataCombo$industry)
sum(is.na(dataCombo$industry))

dataCombo$return.client = na.omit(dataCombo$return.client)
sum(is.na(dataCombo$return.client))

dataCombo$year = na.omit(dataCombo$year)
sum(is.na(dataCombo$year))

dataCombo$country = na.omit(dataCombo$country)
sum(is.na(dataCombo$country))

## ----------------------------------------------------------- ##
##Section V. Creating variables:
## (#1) ## 
dataCombo$sale.per.units = dataCombo$sale/dataCombo$units
summary(dataCombo$sale.per.units) 

## (#2) ## 
dataCombo$rating.level = dataCombo$rating # Create new variable rating_level

dataCombo$rating.level[dataCombo$rating.level>=5] = "excellent"
dataCombo$rating.level[dataCombo$rating.level<5 & dataCombo$rating.level>=4] = "satisfactory"
dataCombo$rating.level[dataCombo$rating.level<4] = "poor"

dataCombo$rating.level = as.factor(dataCombo$rating.level) # Convert to factor type
levels(dataCombo$rating.level)

## (#3) ## 
dataCombo$priority = dataCombo$priority
dataCombo$priority = 0
dataCombo$priority[dataCombo$rating.level=="poor" & dataCombo$return.client==1] = 1
dataCombo$priority = as.logical(dataCombo$priority) 

write.csv(dataCombo,"C:/Users/Sahil Rajapkar/Desktop/dataCombo.csv",row.names = FALSE)






