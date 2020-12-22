library(tidyverse)
library(repr)
library(gridExtra)

# dataset: https://www.kaggle.com/nilimajauhari/glassdoor-analyze-gender-pay-gap
glassdoor <- read_csv("./Glassdoor_Gender_Pay_Gap.csv")
head(glassdoor)

# convert to factors
glassdoor <- glassdoor %>% mutate_if(is.character, as.factor) 
glassdoor$Seniority <- as.factor(glassdoor$Seniority)
glassdoor$PerfEval <- as.factor(glassdoor$PerfEval)

# add variable: TotalPay
glassdoor$TotalPay <- TotalPay <- glassdoor$BasePay + glassdoor$Bonus

# split into sub dataframes for male and female
male <- glassdoor %>% filter(Gender == "Male")
female <- glassdoor %>% filter(Gender == "Female")

options(repr.plot.width = 20, repr.plot.height = 10)
p <- ggplot(data = male, aes(TotalPay)) + geom_histogram(bins=30) + ggtitle("Distribution of total pay among men") +
            theme(text = element_text(size = 20)) + 
            geom_vline(aes(xintercept = mean(TotalPay)),col='red',size=1)
q <- ggplot(data = female, aes(TotalPay)) + geom_histogram(bins=30) + ggtitle("Distribution of total pay among women") +
            theme(text = element_text(size = 20)) + 
            geom_vline(aes(xintercept = mean(TotalPay)),col='red',size=1)
grid.arrange(p, q, ncol = 1)


# helper function for making pie charts
MakePieChart <- function(col, df, extra) {
    tbl <- as.data.frame(table(df[[col]]))     
    colnames(tbl) <- c(col, "Count")
    tbl <- tbl %>%
          arrange(desc(tbl)) %>%
          mutate(prop = round(Count / sum(tbl$Count), 3) * 100) %>%
          mutate(ypos = cumsum(prop) - 0.5*prop)

    pie_chart <- ggplot(tbl, aes(x = "", y = prop, fill = .data[[col]])) +
         geom_bar(stat = "identity", width = 1) +
         coord_polar("y", start = 0) +
         geom_text(aes(y = ypos, label = paste(prop, "%")), color = "white", size = 6) +
         theme_void() +     
         theme(text = element_text(size = 20)) +
         ggtitle(paste("Summary of ", col, extra))
    
    return (pie_chart)
}

# pir charts for gender and educaton
options(repr.plot.width = 15, repr.plot.height = 5)
MakePieChart("Gender", glassdoor, "")

grid.arrange(MakePieChart("Education", male, " level among men"), 
             MakePieChart("Education", female, " level among women"), nrow = 1) 

grid.arrange(MakePieChart("Seniority", male, " level among men"), 
             MakePieChart("Seniority", female, " level among women"), nrow = 1) 

# boxplots for difference in total pay between gender groups
ggplot(glassdoor, aes(x = Seniority, y = TotalPay, fill = Gender)) +
    geom_boxplot() +
    ggtitle("Difference in total pay between Seniority and Gender") +
    theme(text = element_text(size = 18)) 

ggplot(glassdoor, aes(x = PerfEval, y = TotalPay, fill = Gender)) +
    geom_boxplot() +
    ggtitle("Difference in total pay between Performance Evaluation Score (PerfEval) and Gender") +
    theme(text = element_text(size = 18)) 

# helper function for making bar charts
MakeBarChart <- function(df, gender) {
    options(repr.plot.width = 22, repr.plot.height = 8)
    ggplot(data = df) +
       geom_bar(mapping = aes(x = JobTitle, fill = JobTitle)) +
       theme(text = element_text(size = 18)) +
       ggtitle(paste("Summary of different job titles and respective counts among", gender))
}

# barchart summary of JobTitles
MakeBarChart(male, "males")
MakeBarChart(female, "females")

# Difference in total pay among the all job positions
ggplot(glassdoor, aes(x = JobTitle, y = TotalPay, fill = Gender)) +
    geom_boxplot() +
    ggtitle("Difference in total pay among all job positions") +
    theme(text = element_text(size = 18)) 

library(MASS)

# create a new variable: JobGroup
glassdoor$JobGroup <- glassdoor$JobTitle
glassdoor <- glassdoor %>% mutate(
                              JobGroup = case_when(
                                  JobGroup %in% c("Manager", "Software Engineer")  ~ "H",
                                  JobGroup == "Marketing Associate" ~ "L",
                                  TRUE ~ "M")                              
                              )
glassdoor$JobGroup <- as.factor(glassdoor$JobGroup)

full <- lm(TotalPay ~ JobGroup + Gender + Age + PerfEval + Education + Seniority, data = glassdoor)
summary(full)

# find the optimal model (the one with most predictive power) using AIC 
stepAIC(full, scope = list(lower = ~ 1, upper = full), direction = "backward")

# summary of the optimal model (essentially full model neglecting the gender term)
optimal <- lm(TotalPay ~ JobGroup + Age + PerfEval + Education + Seniority, data = glassdoor)
summary(optimal)

# proportionate reduction of error
(summary(full)$sigma - summary(optimal)$sigma)/summary(optimal)$sigma

sub <- glassdoor %>%
       filter(JobGroup == "M")
ggplot(sub, aes(x = Seniority, y = TotalPay, fill = Gender)) +
    geom_boxplot() +
    ggtitle("Difference in total pay among job positions in medium pay group") +
    theme(text = element_text(size = 20)) 
