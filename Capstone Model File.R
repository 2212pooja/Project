## Importing libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(fitdistrplus)
library(mapproj)
library(magrittr)
library(caret)
library(InformationValue)
library(Hmisc)



setwd("C:/Users/Pooja Mahajan/OneDrive/Desktop/capstone")


## Reading H1B Visa Petition data from 2011-2016

h1b_data<-read.csv('h1b_kaggle.csv')

print(head(h1b_data))


## Data Science Vector

data_science_jobs <- c("DATA SCIENTIST", "BUSINESS ANALYST", "DATA ANALYST", "DATA ENGINEER")

data_science_df <- h1b_data %>% filter(JOB_TITLE %in% data_science_jobs)

data_science_fulltime <- data_science_df %>% 
  filter(FULL_TIME_POSITION == "Y")

data_science_parttime <- data_science_df %>% 
  filter(FULL_TIME_POSITION == "N")

head(data_science_df)




## Basic Probability operations

# Full Time Data Science
mean(data_science_fulltime$PREVAILING_WAGE)

median(data_science_fulltime$PREVAILING_WAGE)

range(data_science_fulltime$PREVAILING_WAGE)

quantile(data_science_fulltime$PREVAILING_WAGE)

var(data_science_fulltime$PREVAILING_WAGE)

sd(data_science_fulltime$PREVAILING_WAGE)


# Compare different datasets, use coefficient of variation
coefficient_of_variation_fulltime = (sd(data_science_fulltime$PREVAILING_WAGE) / mean(data_science_fulltime$PREVAILING_WAGE)) * 100 
coefficient_of_variation_fulltime

# Part Time Data Science
mean(data_science_parttime$PREVAILING_WAGE)

median(data_science_parttime$PREVAILING_WAGE)

range(data_science_parttime$PREVAILING_WAGE)

quantile(data_science_parttime$PREVAILING_WAGE)

var(data_science_parttime$PREVAILING_WAGE)

sd(data_science_parttime$PREVAILING_WAGE)
 

# Compare different datasets, use coefficient of variation
coefficient_of_variation_parttime = (sd(data_science_parttime$PREVAILING_WAGE) / mean(data_science_parttime$PREVAILING_WAGE)) * 100
coefficient_of_variation_parttime

## 

applications_11_16 <- data_science_df %>% 
  group_by(YEAR) %>% 
  summarise(noOfApplications = n())
   

applications_11_16%>%
  
  ggplot(mapping=aes(x= as.character(YEAR), y= noOfApplications)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("No of applications per year for Data Science jobs") +
  labs(y="Count of Applications", x = "Year") +
  theme_minimal()



## Top 10 Companies that provide Data Science Jobs

companies_ds_df <- data_science_df %>% 
  group_by(EMPLOYER_NAME) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))

companies_ds_df <- companies_ds_df[1:10,]

companies_ds_df%>%
  ggplot(mapping=aes(x= total, y=EMPLOYER_NAME)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Companies that provide the most H1B for Data Science jobs") +
  labs(y="Companies", x = "Total") +
  theme_minimal()


## Top 10 Companies 2011 - 2016

companies_df <- h1b_data %>% 
  group_by(EMPLOYER_NAME) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))

companies_df <- companies_df[1:10,]

companies_df%>%
  ggplot(mapping=aes(x=as.integer(total), y=EMPLOYER_NAME)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Companies that provide the most H1B") +
  labs(y="Companies", x = "Total") +
  theme_minimal()


## Status of Applications  

accepted_rejected_df <- h1b_data %>% 
  group_by(CASE_STATUS) %>% 
  summarise(total = n()) %>% 
  drop_na()


# pie-chart

ggplot(accepted_rejected_df, aes(x="", y=total, fill=CASE_STATUS)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("Status of applications")


### Probability Mass Fucntion (PMF)  

pmf_df <- h1b_data %>%
  filter(CASE_STATUS == "CERTIFIED") %>% 
  group_by(YEAR) %>%
  summarise(count=n(),  .groups = 'drop') %>%
  mutate(pmf = count/sum(count))


ggplot(pmf_df, aes(YEAR, pmf)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_bw() +
  ggtitle("PMF for Accepted applications from 2011-2016")+
  labs(x = 'No of accepted applications', y = 'PMF') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous("Year", labels = pmf_df$YEAR ,breaks = pmf_df$YEAR)


### Continious Distribution Fucntion (CDF) 

cdf_df <- data_science_df %>%
  filter(CASE_STATUS == "CERTIFIED") %>% 
  group_by(YEAR) %>%
  summarise(count=n(),  .groups = 'drop') %>%
  mutate(
    pmf = count/sum(count),
    cdf = cumsum(count)/sum(count) 
  )


ggplot(cdf_df, aes(x=as.character(YEAR), y=cdf))+
  geom_bar(stat="identity", fill="steelblue")+
  theme_bw() +
  ggtitle("CDF for Accepted applications from 2011-2016")+
  labs(x="Year", y="CDF of accepted applications")+
  scale_fill_brewer(palette="Spectral")


### Expected Value

expected_df <- data_science_df %>%
  filter(CASE_STATUS == "CERTIFIED") %>% 
  group_by(YEAR) %>%
  summarise(count=n(),  .groups = 'drop') %>%
  mutate(
    pmf = count/sum(count),
    cdf=cumsum(count)/sum(count),
    expected_val=weighted.mean(count,pmf)
  )

weighted.mean(expected_df$count,expected_df$pmf)



## Joint Probability

jp_df <- data_science_df %>%
  group_by(YEAR) %>%
  summarise(count = n()) %>%
  mutate(pickup_pmf = count/sum(count)) %>%
  mutate(pickup_cdf = cumsum(pickup_pmf)) 


jp_df

joint_freq <- outer(cdf_df$count, jp_df$count, FUN = "+") 
rownames(joint_freq) <- cdf_df$count
colnames(joint_freq) <- jp_df$count
joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),3) 
joint_prob

joint_df <- melt(joint_freq)
colnames(joint_df) <- c('Accepted_Applications', 'Total_Applications', 'frequency')

head(joint_df, 10)

ggplot(data = joint_df,aes(x=Accepted_Applications, y=Total_Applications)) +
  geom_point(aes(size = frequency, color = frequency)) +
  labs(x = 'Accepted Applications', y = 'Total Applications') +
  scale_x_continuous("Accepted_Applications", labels = as.character(joint_df$Accepted_Applications),breaks = joint_df$Accepted_Applications) + 
  scale_y_continuous("Total_Applications", labels = as.character(joint_df$Total_Applications),breaks = joint_df$Total_Applications)+
  theme_minimal()


h1b <- data.frame(lapply(h1b_data, function(v) {
  if (is.factor(v)) return(toupper(v))
  else return(v)
}))
h1b <- tbl_df(h1b)
glimpse(h1b)


## Histogram of Prevailing Wage

# Split WORKSITE column into two columns: city and state
h1b <- h1b %>%
  separate(WORKSITE, c("CITY", "STATE"), ", ")

# Change variables 'STATE' and 'YEAR' to ordered factors
h1b$STATE <- factor(h1b$STATE, ordered = TRUE)
h1b$YEAR <- factor(h1b$YEAR, ordered = TRUE)


# Keep only "CERTIFIED" H1B cases
certified_h1b <- h1b %>%
  filter(CASE_STATUS == "CERTIFIED")

# Generate a sample of wages and filter out NA values
set.seed(123)
wage_sample <- subset(certified_h1b[sample(1:nrow(certified_h1b), 300000), 7], 
                      !is.na(PREVAILING_WAGE))


# Histogram of sampled wages
ggplot(subset(wage_sample, 
              PREVAILING_WAGE >= quantile(PREVAILING_WAGE, 0.1) & 
                PREVAILING_WAGE <= quantile(PREVAILING_WAGE, 0.95)), 
       aes(x = PREVAILING_WAGE)) + 
  geom_histogram(binwidth = 1000,
                 col = "red", 
                 fill = "blue", 
                 alpha = .2) +
  scale_x_continuous(breaks = seq(min(wage_sample$PREVAILING_WAGE), 
                                  max(wage_sample$PREVAILING_WAGE),
                                  5000)) +
  labs(title = "Histogram of Prevailing Wage)", 
       x = "prevailing wage")



## Map for H1B petitions by state

# Import state data containing state abbreviations and locations

data(state)
states <- data.frame(state.abb, state.name, state.area, state.center,
                     state.division, state.region, state.x77)

row.names(states) <- NULL
states <- states %>%
  dplyr::select(state.abb, state.name, x, y, Population, Income, Illiteracy) 
states <- subset(states, ! state.abb %in% c("HI", "AK"))


# Count H1B petitions filed by each state
petition_by_state <- certified_h1b %>%
  dplyr::filter(STATE != "NA") %>%
  group_by(region = tolower(STATE)) %>%
  summarise(no_petitions = n()) %>%
  arrange(desc(no_petitions))


# Draw the map
us_states <- map_data("state")
petition_by_state <- inner_join(petition_by_state, us_states, by = "region")
plot_petition_by_state <- ggplot(petition_by_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(no_petitions, 7, dig.lab=6))) +
  geom_path(color = "gray", linestyle = 2) +
  coord_map() +
  geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL),
            size = 4, color = "white") +
  #scale_fill_brewer('Number of petitions', palette  = 'PuRd', label = scales::comma) +
  ggtitle("H1B petitions by state") +
  theme_minimal()

## Changes in quantity of H1B cases over five years

# Count H1B petitions filed in each year
case_quantity_per_year <- certified_h1b %>%
  group_by(YEAR) %>%
  summarise(all_occupations = n())
# Bar plot showing the H1B quantities in each year
ggplot(case_quantity_per_year, aes(y = all_occupations, x = YEAR, fill = YEAR)) + 
  geom_bar(stat = "identity", alpha = 0.7, width = 0.5) + 
  scale_y_continuous(limits = c(0, 570000), 
                     breaks = seq(0, 570000, 100000),
                     labels = scales::comma) + 
  ggtitle("Changes in quantity of H1B cases over five years")+
  theme(
    plot.title = element_text(size = rel(1.3)),
    panel.background = element_rect(fill = '#f0f0f0'),
    legend.position = "none"
  ) + 
  theme_minimal()


## Quantity of H1B cases in California, New York and Texas from 2011 to 2016

# Count H1B petitions filed in CA, NY and TX in each year
cnt_case_per_year <- certified_h1b %>%
  filter(STATE %in% c("CALIFORNIA", "NEW YORK", "TEXAS")) %>%
  group_by(YEAR, STATE) %>%
  summarise(count = n()) %>%
  arrange(YEAR, STATE) 

# Bar plot showing H1B quantities of each state in each year
ggplot(cnt_case_per_year, aes(x = YEAR, y = count, fill = STATE)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8,
           color = "grey") +
  ggtitle("Quantity of H1B cases in California, New York and Texas from 2011 to 2016") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.3)))+
  theme_minimal()

## Wages for H1B cases in top 10 companies

# Top 10 employers who filed the most H1B petitions
top_10_employers <- certified_h1b %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(num_apps = n()) %>%
  arrange(desc(num_apps)) %>%
  slice(1:10) %>%
  dplyr::select(EMPLOYER_NAME)
employers_boxplot_df <- certified_h1b %>%
  filter(EMPLOYER_NAME %in% top_10_employers$EMPLOYER_NAME)

# Box-plot showing the wage distribution of each employer
ggplot(employers_boxplot_df, aes(y = PREVAILING_WAGE, x = EMPLOYER_NAME, 
                                 fill = EMPLOYER_NAME, notch = TRUE, notchwidth = .3)) + 
  geom_boxplot(notch = TRUE) + 
  scale_y_continuous(limits = c(0, 150000), 
                     breaks = seq(0, 150000, 10000)) + 
  ggtitle("Wages for H1B cases in top 10 companies")+
  theme(
    plot.title = element_text(size = rel(1.3)),
    panel.background = element_rect(fill = '#f0f0f0'),
    axis.text.x=element_blank(),
    legend.position = "bottom", 
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.4)), 
    panel.grid.major = element_line(colour = '#f0f0f0'),
    panel.grid.major.x = element_line(linetype = 'blank'),
    panel.grid.minor = element_line(linetype = 'blank')  
  )

## Top 10 occupations with highest median prevailing wages

# Top 10 occupations with the highest wages
top_10_soc_with_highest_wage <- certified_h1b %>%
  group_by(SOC_NAME) %>%
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  slice(1:10) %>%
  dplyr::select(SOC_NAME, median_wage)


# Bar plot showing median wages for each occupation
ggplot(top_10_soc_with_highest_wage, aes(y = median_wage, x = reorder(SOC_NAME, median_wage))) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7, width = 0.7) + 
  
  #scale_y_continuous(limits = c(0, 150000), 
                 #breaks = seq(0, 150000, 5000)) + 
  ggtitle("Top 10 occupations with highest median prevailing wages") +
  coord_flip() +
  theme(
   
    plot.title = element_text(size = rel(1)),
    axis.text.x=element_text(size = rel(0.8)),
    legend.position = "bottom"
  ) +
  labs(x = "Occupational Name") +
  theme_minimal()


## Wages for certified & denied H1B cases

certified_denied_h1b <- h1b %>%
  filter(CASE_STATUS == "CERTIFIED" | CASE_STATUS == "DENIED")

# Box-plot of prevailing wage in these two categories
ggplot(certified_denied_h1b, aes(y = PREVAILING_WAGE, x = CASE_STATUS, 
                                 fill = CASE_STATUS, notch = TRUE, 
                                 notchwidth = .3)) + 
  geom_boxplot(notch = TRUE) + 
  scale_fill_manual(values = c("#29a329", "#ea4b1f"))+
  scale_y_continuous(limits = c(0, 150000), 
                     breaks = seq(0, 150000, 10000)) + 
  ggtitle("Wages for certified & denied H1B cases")+
  theme(
    plot.title = element_text(size = rel(1.3)),
    panel.background = element_rect(fill = 'light gray'),
    panel.grid.major = element_line(colour = '#f0f0f0'),
    panel.grid.major.x = element_line(linetype = 'blank'),
    panel.grid.minor = element_line(linetype = 'blank')
  ) +
  theme_minimal()


## Histograms of Prevailing Wages by year

# Randomly sample one tenth of total H1B petitions, excluding Null values in wages
wage_year_sample <- subset(certified_h1b[sample(1:nrow(certified_h1b), 300000), c(7, 8)], 
                           !is.na(PREVAILING_WAGE) & 
                             PREVAILING_WAGE <= quantile(certified_h1b$PREVAILING_WAGE, 0.99))

# Histograms of wages in each year
wage_dist_per_year <- ggplot(subset(wage_year_sample, 
                                    PREVAILING_WAGE >= quantile(PREVAILING_WAGE, 0.1) & 
                                      PREVAILING_WAGE <= quantile(PREVAILING_WAGE, 0.95)), 
                             aes(x = PREVAILING_WAGE)) + 
  geom_histogram(binwidth = 1000,
                 col = "red", 
                 fill = "blue", 
                 alpha = .2) +
  facet_wrap(~YEAR) +
  scale_x_continuous(breaks = seq(min(wage_sample$PREVAILING_WAGE), 
                                  max(wage_sample$PREVAILING_WAGE),
                                  10000)) +
  labs(title = "Histograms of Prevailing Wages by year", 
       x = "prevailing wage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()
wage_dist_per_year



## Increase in wage over the years
set.seed(123)
# Filter out the top 1% outliers in the prevailing_wage variable
wage_year_sample <- subset(certified_h1b[sample(1:nrow(certified_h1b), 300000), 
                                         c(7, 8)], 
                           !is.na(PREVAILING_WAGE) & 
                             PREVAILING_WAGE <= quantile(certified_h1b$PREVAILING_WAGE, 
                                                         0.99))
wage_year_sample <- wage_year_sample %>%
  group_by(YEAR) %>%
  mutate(mean_wage = mean(PREVAILING_WAGE),
         median_wage = median(PREVAILING_WAGE),
         '10th_percentile' = quantile(PREVAILING_WAGE, 0.1),
         '90th_percentile' = quantile(PREVAILING_WAGE, 0.9))

wage_year_stats <- wage_year_sample %>%
  distinct(mean_wage, median_wage, `10th_percentile`, `90th_percentile`,YEAR)

wage_year_stats <- wage_year_stats[order(wage_year_stats$YEAR), 
                                   c(ncol(wage_year_stats), 
                                     1:(ncol(wage_year_stats) - 1))]


# From wide to long format
wage_year_stats_long <- gather(wage_year_stats, statistics, values, 
                               mean_wage:`90th_percentile`,YEAR,
                               factor_key = TRUE)
new
# Trend of median, mean, 10th percentile and 90th percentile of wages
wage_trend <- ggplot(wage_year_stats, 
                     aes(x = YEAR, y = mean_wage)) +
  geom_line(aes(color = statistics), lineend = "round", size = 1) +
  expand_limits(y = 0) +
  scale_y_continuous(breaks = seq(0, 120000, 10000), labels = scales::comma) +
  ggtitle("Wage Trend from 2011 to 2016: Line Chart") +
  labs(y = "wage / $") +
  theme(plot.title = element_text(size = rel(1.3)),
        legend.position = "bottom")

#wage_trend
knitr::kable(wage_year_stats)

## Sampling Tests
## Total Population consist of Employees with Job Title related to Data Science.
## Sample Population consist of Employees with Job Title related to Data Science working in Microsoft Corporation.
## Assuming our entire dataset is the population we want to test if the mean wage of our sample data is equivalent to mean wage
employerSample <- sample_n(data_science_df, 1000)
employerMicrosoft <- data_science_df %>% dplyr::filter(EMPLOYER_NAME == 'MICROSOFT CORPORATION')



## One Sample Z-test

# The sample mean wage Mu is
mean(employerSample$PREVAILING_WAGE)

# And the population mean wage (Mu0) is

mean(data_science_df$PREVAILING_WAGE)


## X = R.V. of wage of a employee
## H0: Mu = 176240 
## Ha: Mu not equal 176240
z.test <- function(sample, pop){ 
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample)
  var = var(pop)
  z = (sample_mean - pop_mean) / (sqrt(var/(n))) 
  return(z)
}
z.test(employerSample$PREVAILING_WAGE, data_science_df$PREVAILING_WAGE)


## Conclusion: Since, the z value lies within (-1.96,1.96), we fail to reject null hypothesis and conclude that there is no significant difference between sample mean wage and population mean wage.


## One Sample t-test

# The sample mean wage (Mu) is
mean(employerMicrosoft$PREVAILING_WAGE)

# And the population mean wage (Mu0) is
mean(data_science_df$PREVAILING_WAGE)

## Thus, our null and alternate hypothesis are: 
## X = R.V. of wage of a employee
# H0: Mu = 84538.73
# Ha: Mu not equal 176240
t.test(employerMicrosoft$PREVAILING_WAGE, mu = mean(data_science_df$PREVAILING_WAGE))
## Two Tailed
t.test(employerMicrosoft$PREVAILING_WAGE,alternative = "two.sided" ,mu = mean(data_science_df$PREVAILING_WAGE))

## Left Tailed
t.test(employerMicrosoft$PREVAILING_WAGE,alternative = "greater" ,mu = mean(data_science_df$PREVAILING_WAGE))
## Right Tailed
t.test(employerMicrosoft$PREVAILING_WAGE,alternative = "less" ,mu = mean(data_science_df$PREVAILING_WAGE))

## Conclusion: As the p − value ≤ 0.05, we reject the null hypothesis and conclude that there is a significant difference between the sample mean wage of employee Microsoft and population mean wage.



## Two Sample Z-test

z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b)) 
  return(z)
}
cen_1 <- data_science_df[1:16000,] #select all columns and rows from 1 to 16000
cen_2 <- data_science_df[16001:32561,] #select all columns and rows from 16001 to 32561 
cen_1_sample <- sample_n(cen_1, 1000) #sample 1000 rows from the first population 
cen_2_sample <- sample_n(cen_2, 1000) #sample 1000 rows from the second population
## X1 = R.V. of wage of a employee from first sample 
## X2 = R.V. of wage of a employee from second sample
## The mean wage of sample 1 is therefore (Mu1)
mean(cen_1_sample$PREVAILING_WAGE)
## The mean wage of sample 2 is therefore (Mu2) 
mean(cen_2_sample$PREVAILING_WAGE)
## Thus, our null and alternate hypothesis are: 
## H0: Mu1 − Mu2 = 0
## H0: Mu1 − Mu2 not equal 0
z_test2(cen_1_sample$PREVAILING_WAGE, cen_2_sample$PREVAILING_WAGE, var(cen_1_sample$PREVAILING_WAGE), var(cen_2_sample$PREVAILING_WAGE))
## Thus, for a significance level of alpha = 0.05, we fail to reject the null hypothesis since the z-value lies within the range [−1.96, 1.96] and conclude that there is no significant difference between the mean wage of two samples.


## Two Sample t-test
## X1 = R.V. of wage of a person from first sample 
## X2 = R.V. of wage of a person from second sample 
## The mean age of sample 1 is therefore (Mu1)
mean(cen_1_sample$PREVAILING_WAGE)
## The mean wage of sample 2 is therefore (Mu2)
mean(cen_2_sample$PREVAILING_WAGE)
## Thus, our null and alternate hypothesis are:
## H0: Mu1 − Mu2 = 0 
## H0: Mu1 − Mu2 not equal 0
t.test(cen_1_sample$PREVAILING_WAGE, cen_2_sample$PREVAILING_WAGE)





## Since p−value ≥ 0.05, we fail to reject the null hypothesis and conclude that there is no significant difference between the mean wage of two samples, reaching the same conclusion as the two sample z-test.


## Logistic Regression

# Split the data into training and test set
set.seed(123)
training.samples <- data_science_df$CASE_STATUS %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- data_science_df[training.samples, ]
test.data <- data_science_df[-training.samples, ]


# Fit the model
model <- glm( as.factor(CASE_STATUS) ~ JOB_TITLE + FULL_TIME_POSITION + YEAR + PREVAILING_WAGE, data = train.data, family = binomial)
# Summarize the model
summary(model)

# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.10, "CERTIFIED", "DENIED")

## Model accuracy
mean(predicted.classes == test.data$CASE_STATUS)
#predicted <- plogis(predict(model, test.data))
#optCutOff <- optimalCutoff(test.data$CASE_STATUS, predicted)[1]
#confusionMatrix(test.data$CASE_STATUS, predicted, threshold = optCutOff)


