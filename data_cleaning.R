# Import data
data <- read.csv("Data_science_task_dataset.csv", header = TRUE, stringsAsFactors = TRUE, row.names = 1)

##create unique identifier after checking there were no duplicates
data <- tibble::rowid_to_column(data1, "ID")

# Check the structure of dataset
str(data)
glimpse(data)

#check missing values
sum(is.na(data))
# Find the columns with the NA values
colSums(is.na(data))
data %>% dplyr::summarize_all(funs(sum(is.na(.)) / length(.)))

#check <- data %>% filter(is.na(REGION))

## Replace the NA values with the Mode

#data$PREMIUM[is.na(data$PREMIUM)]<-names(median(table(data$PREMIUM)))
#data$DRIVERAGE[is.na(data$DRIVERAGE)]<-names(median(table(data$DRIVERAGE)))
#data$LICENCEYEARSHELD[is.na(data$LICENCEYEARSHELD)]<-(which.max(table(data$LICENCEYEARSHELD)))
#data$PREVIOUSCLAIMS [is.na(data$PREVIOUSCLAIMS )]<-names(median(table(data$PREVIOUSCLAIMS )))
data$REGION[is.na(data$REGION)]<-names(which.max(table(data$REGION)))
#data$VEHICLEVALUE[is.na(data$VEHICLEVALUE)]<-names(median(table(data$VEHICLEVALUE)))
data$VEHICLETYPE[is.na(data$VEHICLETYPE)]<-names(which.max(table(data$VEHICLETYPE)))




#data manipulation
data <- data %>%  mutate(SALE = ifelse(SALE == 1,'YES','NO'))

#no duplicates
#upper case and remove whitespace
#data$VEHICLETYPE<- mutate_all(data$VEHICLETYPE, toupper)
data$VEHICLETYPE<- toupper(data$VEHICLETYPE)
data$VEHICLETYPE <- str_replace_all(data$VEHICLETYPE, fixed(" "), "")

data <- na.omit(data)
#numeric
# Select only numeric predictors

#numdata$PREMIUM <- is.numeric(numdata$PREMIUM)


par(mfrow = c(2,4))
for( i in 1:7){
  numdata <- data %>%
    dplyr::select_if(is.numeric) %>% 
    dplyr::select(-SALE)
  hist(numdata[,i], main = colnames(numdata)[i],xlab =     colnames(numdata)[i], col = 'blue')
}

#From the above histograms, it is evident that all variables are highly skewed, we can analyze them in buckets.


##data wrangling

### categorise age

#check<-newdata %>% select(DRIVERAGE, NOCLAIMSDISCOUNT)
#ggplot(check, aes(x=DRIVERAGE, y=NOCLAIMSDISCOUNT)) + geom_point()+ geom_smooth()

data<-data %>% 
  mutate(CATAGE = case_when(
                            DRIVERAGE >= 16 & DRIVERAGE < 21 ~ '16-20 years',
                            DRIVERAGE >=21 & DRIVERAGE < 26 ~ '21-25 years', 
                            DRIVERAGE >=26 & DRIVERAGE < 31 ~ '26-30 years',
                            DRIVERAGE >=31 & DRIVERAGE < 35 ~ '31-35 years',
                            DRIVERAGE >=35 & DRIVERAGE < 41 ~ '36-40 years',
                            DRIVERAGE >=41 & DRIVERAGE < 45 ~ '41-45 years',
                            DRIVERAGE >=45 & DRIVERAGE < 51 ~ '46-50 years',
                            DRIVERAGE >=51 & DRIVERAGE < 55 ~ '51-55 years',
                            DRIVERAGE >=55 & DRIVERAGE < 61 ~ '56-60 years',
                            DRIVERAGE >=61 ~ '>60 years')) #%>% 
  dplyr::select(-DRIVERAGE)
data$CATAGE <-
  factor(data$CATAGE, levels = c("16-20 years", "21-25 years", "26-30 years",
                                                  "31-35 years", "36-40 years", "41-45 years",
                                                  "46-50 years", "51-55 years", '56-60 years', '>60 years'))
#ggplot(newdata, aes(x=VEHICLEVALUE)) + geom_histogram()
#premium looks very skewed
#check <- newdata %>% filter(PREMIUM>= 1500)
##MAKe this into a function
#ggplot(newdata, aes(x=PREMIUM, y=QUOTEDATE)) + geom_point()#+ geom_smooth()
##enough info onto premium

### categorise premium maybe dont need to as its not skewed
# data<-newdata %>% 
#   mutate(PREMIUM_CAT = case_when(
#     PREMIUM >= 300 & PREMIUM < 450 ~ '300-450',
#     PREMIUM >=450 & PREMIUM < 600 ~ '451-600', 
#     PREMIUM >=600 & PREMIUM < 750 ~ '601-750',
#     PREMIUM >=750 & PREMIUM < 900 ~ '751-900',
#     PREMIUM >=900 & PREMIUM < 1050 ~ '901-1050',
#     PREMIUM >1050 & PREMIUM < 1200 ~ '1051-1200',
#     PREMIUM >=1200 & PREMIUM < 1350 ~ '1201-1350',
#     PREMIUM >=1350 & PREMIUM < 1500 ~ '1351-1500',
#     PREMIUM >=1500 & PREMIUM < 1650 ~ '1501-1650',
#     PREMIUM >=1650 ~ '>1650'))
# 
# data$PREMIUM_CAT <-
#   factor(data$PREMIUM_CAT, levels = c("300-450", "451-600", "601-750",
#                                  "751-900", "901-1050", "1051-1200",
#                                  "1201-1350", "1351-1500", '1501-1650', '>1650'))
# 
# check<-data %>% filter(VEHICLEVALUE<500)
# ggplot(check, aes(x=PREMIUM, y=VEHICLEAGE)) + geom_point()#+ geom_smooth()
# boxplot(data$VEHICLEAGE)
#possibly categorize (binary) vehicle age, noclaimsdiscount, previousclaims
#ordinal vehicle value


###DATE

data$INCEPTIONDATE<-as.Date(data$INCEPTIONDATE, format = "%Y-%m-%d")
data$QUOTEDATE<-as.Date(data$QUOTEDATE, format = "%Y-%m-%d")

to.interval <- function(anchor.date, future.date, interval.days){
  round(as.integer(future.date - anchor.date) / interval.days, 0)}

data$INCEPTIONINT <- to.interval(as.Date('2021-01-01'),
                           data$INCEPTIONDATE, 1 )

data$QUOTEINT <- to.interval(as.Date('2020-11-26'),
                             data$QUOTEDATE, 1 )
data <- data %>% 
  mutate(#date = lubridate::ymd(INCEPTIONDATE),
         group = cut(INCEPTIONDATE, "14 days"),
         groupq = cut(QUOTEDATE, "14 days")) %>% 
  group_by(group) %>% 
  mutate(INCEPTIONDATECAT = min(INCEPTIONDATE)) %>%
  ungroup() %>% 
  group_by(groupq) %>% 
  mutate(QUOTEDATECAT = min(QUOTEDATE)) %>%
  ungroup() %>% 
  dplyr::select(-c(group, groupq, INCEPTIONDATE, QUOTEDATE))

data$INCEPTIONDATECAT <- as.character(data$INCEPTIONDATECAT)
data$QUOTEDATECAT <- as.character(data$QUOTEDATECAT)



#data$SALE <- as.numeric(data$SALE)



### after a quick google looks like learner car insurers can start saiving on no claims discount
## strange to have 16 /17 year olds with high amount NCD. Maybe they are secondary driver or maybe they have given incorrect information


  ### 3292/160000 2% of data will be removed
    
    
    data<- subset(data, !(
                            data$DRIVERAGE == 16 & data$NOCLAIMSDISCOUNT >0|
                            data$DRIVERAGE == 17 & data$NOCLAIMSDISCOUNT >1 |
                            data$DRIVERAGE == 18 & data$NOCLAIMSDISCOUNT >2 | 
                            data$DRIVERAGE == 19 & data$NOCLAIMSDISCOUNT >3 | 
                            data$DRIVERAGE == 20 & data$NOCLAIMSDISCOUNT >4 | 
                            data$DRIVERAGE == 21 & data$NOCLAIMSDISCOUNT >5 | 
                            data$DRIVERAGE == 22 & data$NOCLAIMSDISCOUNT >6 | 
                            data$DRIVERAGE == 23 & data$NOCLAIMSDISCOUNT >7 | 
                            data$DRIVERAGE == 24 & data$NOCLAIMSDISCOUNT >8 | 
                            data$DRIVERAGE == 25 & data$NOCLAIMSDISCOUNT >9))

    
    data<- subset(data, !(
      data$DRIVERAGE == 16 & data$LICENCEYEARSHELD >0|
        data$DRIVERAGE == 17 & data$NOCLAIMSDISCOUNT >1 |
        data$DRIVERAGE == 18 & data$NOCLAIMSDISCOUNT >2 | 
        data$DRIVERAGE == 19 & data$NOCLAIMSDISCOUNT >3 | 
        data$DRIVERAGE == 20 & data$NOCLAIMSDISCOUNT >4 | 
        data$DRIVERAGE == 21 & data$NOCLAIMSDISCOUNT >5 | 
        data$DRIVERAGE == 22 & data$NOCLAIMSDISCOUNT >6 | 
        data$DRIVERAGE == 23 & data$NOCLAIMSDISCOUNT >7 | 
        data$DRIVERAGE == 24 & data$NOCLAIMSDISCOUNT >8 | 
        data$DRIVERAGE == 25 & data$NOCLAIMSDISCOUNT >9))
    
    data1<- subset(data, (LICENCEYEARSHELD > (DRIVERAGE - 16)))
    check=data %>% mutate(flag = case_when(LICENCEYEARSHELD > (DRIVERAGE - 16)~'F')) %>% 
      filter(flag=='F')
    
 #imputation give incorrect values
    