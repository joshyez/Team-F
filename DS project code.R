##Project code:




##Create state_data  



  #create dataset for cases from May 1, 2020
state_data <- subset(state_data, date >= "2020-05-01")

  #create variable of daily change in cases (May to mid-July) per state 
library(dplyr)
state_data <- state_data %>% 
      group_by(state) %>%
      mutate(change_cases = cases - cases[date=="2020-05-01"])
  #Get the daily percent change in total cases since May 1st for each state
state_data <-  state_data %>%
        group_by(state) %>% 
        arrange(date, .by_group = TRUE) %>%
        mutate(pct_change = (cases/lag(cases) - 1) * 100)

##Create data_by_state



  #Get the average percentage change over time for each state 
data_by_state$avg_change <- aggregate(Cases.per.thou~State, data_by_state, function(x) mean(c(NA, diff(x)), na.rm = TRUE))


##Code for Siyu's Analysis





                                      
                                      
                                      
##Code for Ngodoo's Analysis
library(ggplot2)
  #boxplot
ggplot(y, aes(x=party, y=avg_change, fill = party, na.action())) + 
  geom_boxplot() + scale_fill_manual(values = c("blue", "red")) +labs(title="Plot of Avg change in Cases \n by Party",
        x ="Political Party", y = "Avg Change in Covid-19 Cases (%)") + theme( plot.title = element_text(hjust = 0.5,  size=14, face="bold")) 

#bar graph 
ggplot(data=y, aes(x=reorder(state, -avg_change), y=avg_change, fill=party)) + ggtitle("Avg Change in Covid-19 Cases \n by State and Political Affiliation") +
  xlab("State") + ylab("Avg Change in Cases (%)") +
  geom_bar(stat="identity") + scale_fill_manual(values = c("blue", "red")) + theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5,  size=14, face="bold"))

#test assumptions:

# Are the data normal? 
qqnorm(y$avg_change)  

#Shapiro-Wilk test for normality
with(y, shapiro.test(avg_change[party == "rep"]))# p = 0.8837
with(y, shapiro.test(avg_change[party == "dem"])) # p = 0.9927

#test homogeneity using f-test
var.test(avg_change ~ party, data = y) # p = 0.03929

#Two sample t-test results
results <- t.test(avg_change~party, data = y, alternative = "less")
results



##Code for Brooke's Analysis

#Scatterplot with regression lines
ggplot(data_by_state_clean, aes(x=Cases.per.thou, y=Deaths.per.thou, color=Party))+ geom_point()+labs(title="Scatterplot", subtitle = "Deaths vs Cases", x= "Cases (per 1,000)", y= "Deaths (per 1,000)") +geom_smooth(method = "lm" )+ scale_color_manual(values=c("red", "blue"))+ xlim(c(0,850))+ ylim(c(0,50))

#Correlation 
cor.test(rep_data1$Cases.per.thou, rep_data1$Deaths.per.thou, method = "pearson", conf.level = 0.95)
cor.test(dem_data1$Cases.per.thou, dem_data1$Deaths.per.thou, method = "pearson", conf.level = 0.95)

#Linear Model
rep_lm <- glm(formula= Cases.per.thou~Deaths.per.thou, data=rep_data1)
summary(rep_lm)
dem_lm <- glm(formula= Cases.per.thou~Deaths.per.thou , data=dem_data1)
summary(dem_lm)

# Confidence Interval 
confint(rep_lm)
confint(dem_lm)




#Code for Josh's Analysis






  
