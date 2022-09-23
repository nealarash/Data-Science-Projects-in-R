library(knitr)
library(tidyverse)
library(dplyr)
library(pander)
library(boot)
library(tinytex)

data1 <- read.csv('brfss(2).csv')
# head(data1)
data1[is.na(data1)] = 0

ggplot(data1, aes(x=data1$BMI, y=data1$X_MENT14D)) +geom_histogram(stat='identity', col='Blue') +
  xlab("Body Mass Index") + ylab("Mental Health") +ggtitle("Mental Health(X_MENT14D) vs. Body Mass Index(BMI)")
  
set.seed(1)
clusters <- sample(unique(data1$BMI, data1$X_MENT14D), size=100, replace=F)
cluster_sample <- data1[data1$BMI %in% clusters, ]
table(cluster_sample$BMI)
ggplot(cluster_sample, aes(x=BMI, y=X_MENT14D)) +geom_histogram(stat='identity', col='Blue') +
  xlab("Body Mass Index") + ylab("Mental Health") + ggtitle("Cluster Sample Graph")
cor(cluster_sample[["BMI"]], cluster_sample[["X_MENT14D"]])
#define function to obtain systematic sample
set.seed(1)
obtain_sys = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  seq(r, r + k*(n-1), k)
}

#obtain systematic sample
sys_sample_df = data1[obtain_sys(nrow(data1), 100), ]

#view first six rows of data frame
# head(sys_sample_df)
ggplot(sys_sample_df, aes(x=sys_sample_df$BMI, y=sys_sample_df$X_MENT14D)) +geom_histogram(stat='identity', col='Blue')
+
  xlab("Body Mass Index") + ylab("Mental Health") +ggtitle("Systemic Sample Graph")
sample1<- sys_sample_df[-c(98:nrow(sys_sample_df)),]
cor(sample1[["BMI"]], sample1[["X_MENT14D"]])
mean(data1$BMI)
var(data1$BMI)
curve(1/1.188876*sqrt(2*pi)*exp((-1/2)*((x-2.837739)/1.188876))^2, 1, 4)
abline(a = .425167, b =0)
set.seed(1)
X = rnorm(4500, 0, 1) # we will reject some of these values
U = rnorm(4500, 0 ,1) # want this many because we want at least 1000 to random variables to fit our distribution

# targest distribution
pi_x <- function(x) { #accept-reject formula
  new_x = (1/1.188876*sqrt(2*pi)*exp((-1/2)*((x-2.837739)/1.188876))^2)
  return(new_x)
}
count = 1
accept = c() #empty vector that we will fill with the accepted random values from X

# Keep cycling until our sample size reaches 1000
while(count <= 4500 & length(accept) < 1000){
  test_u = U[count]
  test_x = pi_x(X[count])/(.27067*dunif(X[count],0,1))
  if (test_u <= test_x){
    accept = rbind(accept, X[count])
    count = count + 1
  }
  count = count + 1
}

hist(accept, col="Blue")

data <- read.csv("brfss.csv")
edu <- data %>% drop_na() %>%
  # filter(age != "Unsure/refused/missing" &Education != '9') %>%
   filter(Education != '9') %>%
  select(Education, X_MENT14D) %>%
  rename(Mental.Health.Status = X_MENT14D) %>%
  filter(Mental.Health.Status != '9')
  # mutate(Education = as.factor(Education))
head(edu) %>% pander(justify = "center")

ggplot(edu, aes(x = Mental.Health.Status, fill = as.factor(Education))) +
  geom_histogram(binwidth = 0.5) +
  guides(fill = guide_legend(title = "Education Level"))
  
strata <- edu %>%
  group_by(Education) %>%
  count()
strata <- strata %>%
  mutate(proportion = round(n/sum(strata['n']),3) ) %>%
  mutate(Number.of.Sample = round(proportion*nrow(edu)))
strata %>% pander(justify = "center")
proportion <- edu %>%
  group_by(Mental.Health.Status) %>%
  count()
proportion <- proportion %>%
  mutate(proportion = round(n/sum(proportion['n']),3) )
proportion %>% pander(justify = "center")

set.seed(5)
# 1. Sample 200 data from the population using the `sample_n()` function.
sample2.1 <- sample_n(edu, nrow(edu), replace = TRUE)
# 2. Sample data from each stratum  using the `sample_n()` function.
sample2.2 <- NULL
for (i in 1:nrow(strata)){
  sample2.2 <- rbind(sample2.2, sample_n(edu %>% filter(Education == strata[['Education']][i]),
strata[['Number.of.Sample']][i], replace = TRUE))
}
# 3. Sample data from each stratum  using discrete inverse cdf method.
inverse_cdf<-function(u){
  if(u<=0.6){
    return(1)
  }
  else if(0.6<u && u<=0.85){
return(2) }
  else{
    return(3)
} }
sample2.3 <- NULL
for (i in 1:nrow(strata)){
  u <- runif(strata[['Number.of.Sample']][i]) # Generate random number from U[0,1]
  Mental.Health.Status_sim <- sapply(u, inverse_cdf)
  Education <- replicate(strata[['Number.of.Sample']][i],strata[['Education']][i])
  tmp <- data.frame(Education, Mental.Health.Status_sim)
  sample2.3 <- rbind(sample2.3, tmp)
}
proportion2 <- sample2.3 %>%
  group_by(Mental.Health.Status_sim) %>%
  count()
proportion2 <- proportion2 %>%
  mutate(proportion = round(n/sum(proportion2['n']),3) )
proportion2 %>% pander()

# Visualize
par( mfrow= c(3,1) )
plot(density(edu[['Mental.Health.Status']]), lwd=4,  xlab='Mental Health Status', main = "bootstrap")
lines(density(sample2.1[['Mental.Health.Status']]), col = 'purple')
plot(density(edu[['Mental.Health.Status']]), lwd=4,  xlab='Mental Health Status', main = "stratified ramdom sampling")
lines(density(sample2.2[['Mental.Health.Status']]), col = 'orange')
plot(density(edu[['Mental.Health.Status']]), lwd=4,  xlab='Mental Health Status', main = "stratified sampling with
inverse CDF")
lines(density(sample2.3[['Mental.Health.Status_sim']]), col = 'red')

# legend("topright", 
legend=c('orignal data', 'bootstrap', 'stratified ramdom sampling', 'stratified sampling withinverse CDF'), fill =c("black","purple", "orange", "red"))
df <- data.frame(Variance=c(var(edu[['Mental.Health.Status']]),
                            var(sample2.1[['Mental.Health.Status']]),
                            var(sample2.2[['Mental.Health.Status']]),
                            var(sample2.3[['Mental.Health.Status_sim']])),
                 StandardError = c( sd(edu[['Mental.Health.Status']])/nrow(edu),
                                    sd(sample2.1[['Mental.Health.Status']])/nrow(sample2.1),
                                    sd(sample2.2[['Mental.Health.Status']])/nrow(sample2.2),
                                    sd(sample2.3[['Mental.Health.Status_sim']])/nrow(sample2.3) ))
rownames(df) <- c('original data', 'random sample', 'stratified ramdom sampling', 'stratified sampling with inverse
CDF')

df
cor(sample2.3[['Education']], sample2.3[['Mental.Health.Status_sim']])

hist(data$X_MENT14D, main = "Histogram of Mental Health", xlab = "Mental Health")
hist(data$STRFREQ_, main = "Histogram of Strength Frequency", xlab = "Strength Frequency")

new_data <- select(data, 'X_MENT14D','STRFREQ_')
new_data <- na.omit(new_data)
new_data <- subset(new_data, X_MENT14D < 9)
new <- new_data %>%
  group_by('X_MENT14D') %>%
  sample_n(50)
plot(new$X_MENT14D, new$STRFREQ_)
cor(new$X_MENT14D, new$STRFREQ_)
a <- ggplot(new, aes(x = X_MENT14D))
a + geom_density() +
  geom_vline(aes(xintercept = (X_MENT14D)),
             linetype = "dashed", size = 0.6)
b <- ggplot(new, aes(x = STRFREQ_))
b + geom_density() +
  geom_vline(aes(xintercept = (STRFREQ_)),
             linetype = "dashed", size = 0.6)
