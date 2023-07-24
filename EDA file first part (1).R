require(tidyverse)
require(stringr)
require(dplyr)
require(lubridate)
require(hms)
require(nycflights13)
require(devtools)
require(lvplot)
require(modelr)


setwd("C:/Users/Pooja Mahajan/OneDrive/Desktop/capstone")



# Reading H1B Visa Petition data from 2011-2016

df_2016<-read.csv('h1b_kaggle.csv')
print(head(df_2016))


print(sprintf("No of rows: %d", df_2016 %>% nrow()))
df_2016 %>% colnames()


### Tidying the data
##Seprating Worksite in to two clumns `CITY` and `STATE`

df_2016 <- df_2016 %>% separate(WORKSITE,c("WORKSITE_CITY","WORKSITE_STATE"),",")
df_2016 <- subset(df_2016, select = -c(X,lon,lat) )
df_2016$CASE_STATUS <- gsub("PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED","PENDING",df_2016$CASE_STATUS)
df_2016 <- df_2016[order(df_2016$WORKSITE_CITY),]
df_2016 <- df_2016[-(1:which(df_2016$WORKSITE_CITY == "ABBEVILLE")),]

### Removing NA Values
df_2016 <- na.omit(df_2016)


print(sprintf("No of rows: %d", df_2016 %>% nrow()))
df_2016 %>% colnames()


## Random Sampling of data
##Original dataset has more than 3 Million records, we need to sample the data to analyse it with more clarity. So below is the random sampling of data, it will reduce the size of actual dataset to 300 thousands.

h1b <- df_2016[sample(nrow(df_2016), nrow(df_2016)/10), ]



unique(df_2016$YEAR)
unique(df_2016$CASE_STATUS)


### Year wise Approval Analysis
ggplot(h1b %>% filter(CASE_STATUS == "CERTIFIED"),aes(YEAR)) + geom_bar()


## State vise applications

ggplot(h1b,aes(WORKSITE_STATE)) + geom_bar() + coord_flip()


### Top function

totalApplication_top <- function(data,colname,num){
  temp <- data[rev(order(data$n)),]
  
  top <- c()
  count <- 0
  for(index in 1:nrow(data))
  {
    if(colname == "WORKSITE_STATE"){
      if(!(temp$WORKSITE_STATE[index] %in% top)){
        top <- c(top,temp$WORKSITE_STATE[index])
        count<- count + 1
      }
    }
    if(colname == "WORKSITE_CITY"){
      if(!(temp$WORKSITE_CITY[index] %in% top)){
        top <- c(top,temp$WORKSITE_CITY[index])
        count <- count + 1
      }
    }
    if(colname == "JOB_TITLE"){
      if(!(temp$JOB_TITLE[index] %in% top)){
        top <- c(top,temp$JOB_TITLE[index])
        count <- count + 1
      }
    }
    
    if(colname == "EMPLOYER_NAME"){
      if(!(temp$EMPLOYER_NAME[index] %in% top)){
        top <- c(top,temp$EMPLOYER_NAME[index])
        count <- count + 1
      }
    }
    
    if(count == num){
      break
    }
  }
  return(top)
}
topStateVSJobTitle <- function(data){
  temp <- data[rev(order(data$n)),]
  return(temp[1:10,])  
}

### Top 5 state Applications

h1b_temp<- h1b %>% count(WORKSITE_STATE)

ggplot(h1b %>% filter(WORKSITE_STATE %in% totalApplication_top(h1b_temp,"WORKSITE_STATE",5)),aes(WORKSITE_STATE)) + geom_bar(aes(fill=CASE_STATUS)) + coord_flip() + facet_wrap(~YEAR)

################################# Top 5 state with full time VS part time positions
h1b_temp<- h1b %>% count(WORKSITE_STATE)
h1b_temp

ggplot(h1b %>% filter(CASE_STATUS=="CERTIFIED" , WORKSITE_STATE %in% totalApplication_top (h1b_temp,"WORKSITE_STATE",5)),aes(WORKSITE_STATE)) + geom_bar() + coord_flip() + facet_wrap(~FULL_TIME_POSITION)


### Top 5 WORKSITE_CITY Applications


h1b_temp<- h1b %>% count(WORKSITE_CITY)

ggplot(h1b %>% filter(WORKSITE_CITY %in% totalApplication_top(h1b_temp,"WORKSITE_CITY",5)),aes(WORKSITE_CITY)) + geom_bar(aes(fill=CASE_STATUS)) + coord_flip() + facet_wrap(~YEAR)

######## Top 5 city with full time VS part time positions
h1b_temp<- h1b %>% count(WORKSITE_CITY)

ggplot(h1b %>% filter(CASE_STATUS == "CERTIFIED" , WORKSITE_CITY %in% totalApplication_top (h1b_temp,"WORKSITE_CITY",5)),aes(WORKSITE_CITY)) + geom_bar() + coord_flip() + facet_wrap(~FULL_TIME_POSITION)

#### Top job applications
h1b_temp<- h1b %>% count(JOB_TITLE)
h1b_temp %>% map_if(is.factor, as.character) %>% as_data_frame -> h1b_temp

ggplot(h1b %>% filter(JOB_TITLE %in% totalApplication_top (h1b_temp,"JOB_TITLE",10)),aes(JOB_TITLE)) + geom_bar(aes(fill=CASE_STATUS)) + coord_flip()

################  Year Vise Certified job title application
ggplot(h1b %>% filter(CASE_STATUS == "CERTIFIED",JOB_TITLE %in% totalApplication_top(h1b_temp,"JOB_TITLE",10)),aes(JOB_TITLE)) + geom_bar() + coord_flip() + facet_wrap(~ YEAR)

### Top 5 job title vs top states
h1b_temp <- h1b %>% count(WORKSITE_STATE,JOB_TITLE)
h1b_temp <- h1b_temp[rev(order(h1b_temp$n)),]
h1b_temp %>% map_if(is.factor, as.character) %>% as_data_frame -> h1b_temp
h1b_temp_1 <- topStateVSJobTitle(h1b_temp)

h1b %>% filter(CASE_STATUS == "CERTIFIED",JOB_TITLE %in% h1b_temp_1$JOB_TITLE,WORKSITE_STATE %in% h1b_temp_1$WORKSITE_STATE) %>% ggplot(aes(JOB_TITLE)) + geom_bar() + coord_flip() + facet_wrap(~WORKSITE_STATE)

####### Top COMPANIES with  HIGHEST NUMBER OF H1B APPLICATIONS
h1b_temp <- h1b %>% count(EMPLOYER_NAME)
h1b_temp <- h1b_temp[rev(order(h1b_temp$n)),]
h1b_temp %>% map_if(is.factor, as.character) %>% as_data_frame -> h1b_temp

h1b %>% filter(EMPLOYER_NAME %in% totalApplication_top(h1b_temp,"EMPLOYER_NAME",5)) %>% ggplot(aes(EMPLOYER_NAME)) + geom_bar(aes(fill=CASE_STATUS)) + coord_flip()


###################### Pie Chart of Application Status

h1b_temp <- h1b %>% count(CASE_STATUS)
h1b_temp %>% mutate(percentage = n*100/sum(n)) %>% ggplot(aes(x="",y=percentage)) + geom_bar(aes(fill=CASE_STATUS),width=10,stat="identity") + coord_polar(theta="y")

h1b_temp <- h1b %>% filter(CASE_STATUS == "CERTIFIED") %>% count(CASE_STATUS,YEAR)

































