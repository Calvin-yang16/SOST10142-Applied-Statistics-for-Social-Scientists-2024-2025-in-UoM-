## Load necessary packages ----
pacman::p_load(modelsummary, tidyverse, psych, haven)
## Import the data
data <- read_dta("ESS11-subset.dta")
table(data$cntry)
glimpse(data)
## 2.Measure 
## 2.1 View the data 
psych::describe(data_clean[, c("happy", "hinctnta", "health", "gndr", "alcfreq")])

## 2.3 Drop NA 
## MCAR missing completely at random - listwide deletion 
data_clean <- data %>%
  select(happy, hinctnta, eisced, health, gndr, alcfreq) %>%
  drop_na()
glimpse(data_clean)
## 2.4 Save a version BEFORE outlier removal
data_no_outlier_removal <- data_clean
## 2.5 Visually inspect continuous measure' potential outliers 
boxplot_vars <- c("happy", "health", "alcfreq")
boxplot(data_clean[, boxplot_vars])
## Only maintain mean、sd、min、max 
outl <- psych::describe(data_clean[, boxplot_vars], fast = TRUE)
outl <- data.frame(outl[, c(3:6)])  
bounds <- outl %>%
  mutate(
    lower = mean - 3 * sd,
    upper = mean + 3 * sd)
data_clean <- data_clean %>%
  mutate(
    happy_outl = ifelse(happy > bounds["happy", "upper"] | happy < bounds["happy", "lower"], 1, 0),
    health_outl = ifelse(health > bounds["health", "upper"] | health < bounds["health", "lower"], 1, 0),
    alcfreq_outl = ifelse(alcfreq > bounds["alcfreq", "upper"] | alcfreq < bounds["alcfreq", "lower"], 1, 0)
  )
data_clean <- data_clean %>%
  mutate(outlier = ifelse(happy_outl == 1 | health_outl == 1 | alcfreq_outl == 1, 1, 0))
## Check numbers of outliers
table(data_clean$outlier)
## Delete outliers and keep the "clean data" 
data_clean <- data_clean %>% filter(outlier == 0)
## 3 Visualise outcome variable and main predictor 
ggplot(data_no_outlier_removal, aes(x = factor(hinctnta), y = happy)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Happiness across household income deciles (With Outliers)",
    x = "Household Income Decile (1 = Lowest, 10 = Highest)",
    y = "Happiness (0–10)"
  )

ggplot(data_clean, aes(x = factor(hinctnta), y = happy)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Happiness across Household income Deciles (Without Outliers)",
    x = "Household Income Decile (1 = Lowest, 10 = Highest)",
    y = "Happiness (0–10)"
  )

## 4 Own visualisation ----
## Clean data and mutate edu_3cat    
data_clean <- data_clean %>%
  mutate(edu_3cat = case_when(
    eisced %in% 0:2 ~ "Low",
    eisced %in% 3:4 ~ "Middle",
    eisced %in% 5:6 ~ "High"
  )) %>%
  mutate(edu_3cat = factor(edu_3cat, levels = c("Low", "Middle", "High")))

ggplot(data_clean %>% filter(!is.na(edu_3cat)), 
       aes(x = edu_3cat, y = happy)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Happiness by Education Level (After Outlier Removal)",
    x = "Education Level",
    y = "Self-reported Happiness (0–10)"
  )
## 5 Regression
## 5.1 # Recode variables(housekeeping)
data_clean <- data_clean %>%
  mutate(
    # Reverse coding
    health_rev = 6 - health,
    alcfreq_rev = 8 - alcfreq,
    
    # Gender as factor
    gndr = factor(gndr, levels = c(1, 2), labels = c("Male", "Female"))
  )
## 5.2 # Regression
model1 <- lm(happy ~ hinctnta + health_rev + gndr + alcfreq_rev + edu_3cat, data = data_clean)
summary(model1)
modelsummary(model1,
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "regression.docx",
             title = "Table 1.Linear Regression Model Predicting Happiness from Household Income and Control Variables")

