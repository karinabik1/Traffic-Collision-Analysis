# ------------------------------------------------
# Loading data 
# ------------------------------------------------

library('dplyr')
library('readr')
library('forcats')
library('ISLR')
library('ggplot2')
library('rsample')
library('yardstick')
library('glmnet')
library('glmnetUtils')
library('plotROC')

crash <- read_csv("datasets/Crash_Reporting_-_Drivers_Data.csv")

# ------------------------------------------------
# Cleaning data 
# ------------------------------------------------

crash_clean <- crash %>% select("Report Number",
                                "Weather", 
                                "Light", 
                                "Speed Limit", 
                                "Collision Type", 
                                "Driver Substance Abuse",
                                "Driver Distracted By",
                                "Injury Severity")

crash_clean <- na.omit(crash_clean)
crash_clean <- unique(crash_clean)
crash_clean <- crash_clean %>% filter(!apply(crash_clean, 1, function(x) any(x == "N/A")))

table(crash_clean$`Weather`)
crash_clean <- crash_clean %>% mutate(`Weather` = 
                                        fct_recode(`Weather`,
                                                   "Blowing Snow" = "BLOWING SNOW",
                                                   "Clear" = "CLEAR",
                                                   "Cloudy" = "CLOUDY",
                                                   "Fog, Smog, Smoke" = "FOGGY",
                                                   "Rain" = "RAINING",
                                                   "Severe winds" = "SEVERE WINDS",
                                                   "Severe winds" = "Severe Crosswinds",
                                                   "Sleet Or Hail" = "SLEET",
                                                   "Snow" = "SNOW",
                                                   "Unknown" = "UNKNOWN"))
crash_clean <- crash_clean %>% mutate(`Weather` = fct_lump_n(`Weather`, n = 6))
crash_clean <- crash_clean %>% mutate(`Weather` = 
                                        fct_recode(`Weather`,
                                                   "Other" = "Unknown"))
table(crash_clean$`Weather`)
crash_clean <- crash_clean %>% mutate(`Weather` = as_factor(`Weather`))

table(crash_clean$Light)
crash_clean <- crash_clean %>% mutate(Light = 
                                        fct_recode(Light,
                                                   "Dark/Limited Lighting" = "Dark - Unknown Lighting",
                                                   "Dark/Limited Lighting" = "DARK -- UNKNOWN LIGHTING",
                                                   "Dark/Limited Lighting" = "Dark - Lighted",
                                                   "Dark/Limited Lighting" = "Dark - Not Lighted",
                                                   "Dark/Limited Lighting" = "DARK LIGHTS ON",
                                                   "Dark/Limited Lighting" = "DARK NO LIGHTS",
                                                   "Twilight" = "Dawn",
                                                   "Twilight" = "DAWN",
                                                   "Twilight" = "Dusk",
                                                   "Twilight" = "DUSK",
                                                   "DAYLIGHT" = "Daylight",
                                                   "OTHER" = "Other",
                                                   "UNKNOWN" = "Unknown"))
crash_clean <- crash_clean %>% mutate(Light = fct_lump_n(Light, n = 6))
table(crash_clean$Light)
crash_clean <- crash_clean %>% mutate(Light = 
                                        fct_recode(Light,
                                                   "UNKNOWN" = "OTHER"))
crash_clean <- crash_clean %>% mutate(Light = as_factor(Light))

table(crash_clean$`Collision Type`)
crash_clean <- crash_clean %>% mutate(`Collision Type` = 
                                        fct_recode(`Collision Type`,
                                                   "Unknown" = "UNKNOWN",
                                                   "SINGLE VEHICLE" = "Single Vehicle",
                                                   "ANGLE COLLISIONS" = "Angle",
                                                   "ANGLE COLLISIONS" = "ANGLE MEETS LEFT TURN",
                                                   "ANGLE COLLISIONS" = "ANGLE MEETS RIGHT TURN",
                                                   "HEAD ON" = "Front to Front",
                                                   "HEAD ON" = "HEAD ON LEFT TURN",
                                                   "HEAD ON" = "ANGLE MEETS LEFT HEAD ON",
                                                   "OPPOSITE DIRECTION" = "OPPOSITE DIR BOTH LEFT TURN",
                                                   "OPPOSITE DIRECTION" = "OPPOSITE DIRECTION SIDESWIPE",
                                                   "Other" = "OTHER",
                                                   "Front to Rear" = "SAME DIR REND LEFT TURN",
                                                   "Front to Rear" = "SAME DIR REND RIGHT TURN",
                                                   "SAME DIRECTION" = "SAME DIRECTION LEFT TURN",
                                                   "SAME DIRECTION" = "SAME DIRECTION RIGHT TURN",
                                                   "Sideswipe" = "Sideswipe, Same Direction",
                                                   "Sideswipe" = "Sideswipe, Opposite Direction",
                                                   "Sideswipe" = "OPPOSITE DIRECTION SIDESWIPE",
                                                   "Sideswipe" = "SAME DIRECTION SIDESWIPE",
                                                   "Front to Rear" = "SAME DIR REAR END",
                                                   "SAME DIRECTION" = "SAME DIR BOTH LEFT TURN",
                                                   "ANGLE COLLISIONS" = "STRAIGHT MOVEMENT ANGLE"))
crash_clean <- crash_clean %>% mutate(`Collision Type` = fct_lump_n(`Collision Type`, n = 6))
table(crash_clean$`Collision Type`)
crash_clean <- crash_clean %>% mutate(`Collision Type` = as_factor(`Collision Type`))

table(crash_clean$`Driver Substance Abuse`)
crash_clean <- crash_clean %>% mutate(`Driver Substance Abuse` = 
                                        fct_recode(`Driver Substance Abuse`,
                                                   "Alcohol-Related" = "ALCOHOL CONTRIBUTED",
                                                   "Alcohol-Related" = "ALCOHOL PRESENT",
                                                   "Alcohol-Related" = "Suspect of Alcohol Use, Not Suspect of Drug Use",
                                                   
                                                   "Drug-Related" = "ILLEGAL DRUG CONTRIBUTED",
                                                   "Drug-Related" = "ILLEGAL DRUG PRESENT",
                                                   "Drug-Related" = "MEDICATION CONTRIBUTED",
                                                   "Drug-Related" = "MEDICATION PRESENT",
                                                   "Drug-Related" = "Not Suspect of Alcohol Use, Suspect of Drug Use",
                                                   
                                                   
                                                   "Combined Substances" = "COMBINED SUBSTANCE PRESENT",
                                                   "Combined Substances" = "COMBINATION CONTRIBUTED",
                                                   "Combined Substances" = "Suspect of Alcohol Use, Suspect of Drug Use",
                                                   "Combined Substances" = "Suspect of Alcohol Use, Unknown",
                                                   "Combined Substances" = "Unknown, Suspect of Drug Use",
                                                   
                                                   "Other" = "UNKNOWN",
                                                   "Other" = "OTHER",
                                                   "Other" = "Not Suspect of Alcohol Use, Not Suspect of Drug Use",
                                                   "Other" = "Unknown, Unknown",
                                                   "Other" = "Unknown, Not Suspect of Drug Use",
                                                   "Other" = "Not Suspect of Alcohol Use, Unknown",
                                                   
                                                   "Clean" = "NONE DETECTED"))
crash_clean <- crash_clean %>% mutate(`Driver Substance Abuse` = as_factor(`Driver Substance Abuse`))
table(crash_clean$`Driver Substance Abuse`)


table(crash_clean$`Driver Distracted By`)
crash_clean <- crash_clean %>% mutate(`Driver Distracted By` = 
                                        fct_recode(`Driver Distracted By`,
                                                   "Interacting with objects within car" = "ADJUSTING AUDIO AND OR CLIMATE CONTROLS",
                                                   "Interacting with objects within car" = "BY MOVING OBJECT IN VEHICLE",
                                                   "Interacting with objects within car" = "USING OTHER DEVICE CONTROLS INTEGRAL TO VEHICLE",
                                                   "Interacting with objects within car" = "Vehicle Controls",
                                                   
                                                   "Interactions with passengers" = "BY OTHER OCCUPANTS",
                                                   "Interactions with passengers" = "Talking/listening",
                                                   
                                                   "Using Electronic Devices" = "DIALING CELLULAR PHONE",
                                                   "Using Electronic Devices" = "OTHER CELLULAR PHONE RELATED",
                                                   "Using Electronic Devices" = "TALKING OR LISTENING TO CELLULAR PHONE",
                                                   "Using Electronic Devices" = "TEXTING FROM A CELLULAR PHONE",
                                                   "Using Electronic Devices" = "Manually Operating (dialing, playing game, etc.)",
                                                   "Using Electronic Devices" = "USING DEVICE OBJECT BROUGHT INTO VEHICLE",
                                                   "Using Electronic Devices" = "Other Electronic Device",
                                                   
                                                   "External Distractions" = "DISTRACTED BY OUTSIDE PERSON OBJECT OR EVENT",
                                                   
                                                   "Eating or Drinking" = "EATING OR DRINKING",
                                                   
                                                   "Inattention to surroundings" = "INATTENTIVE OR LOST IN THOUGHT",
                                                   "Inattention to surroundings" = "LOOKED BUT DID NOT SEE",
                                                   
                                                   "No Driver Present" = "NO DRIVER PRESENT",
                                                   
                                                   "Not Distracted" = "NOT DISTRACTED",
                                                   
                                                   "Other" = "Other Action (looking away from task, etc.)",
                                                   "Other" = "OTHER DISTRACTION",
          
                                                   
                                                   "Smoking Related" = "SMOKING RELATED",
                                                   
                                                   "Unknown" = "UNKNOWN",
                                                   
                                                   "Vehicle Controls" = "USING OTHER DEVICE CONTROLS INTEGRAL TO VEHICLE"))
crash_clean <- crash_clean %>% mutate(`Driver Distracted By` = fct_lump_n(`Driver Distracted By`, n = 6))
crash_clean <- crash_clean %>% mutate(`Driver Distracted By` = as_factor(`Driver Distracted By`))
table(crash_clean$`Driver Distracted By`)


table(crash_clean$`Injury Severity`)
crash_clean <- crash_clean %>% mutate(`Injury Severity` = fct_recode(`Injury Severity`,
                                    "Suspected Injury" = "Fatal Injury",
                                    "Suspected Injury" = "FATAL INJURY",
                                    "Suspected Injury" = "No Apparent Injury",
                                    "Suspected Injury" = "NO APPARENT INJURY",
                                    "Suspected Injury" = "Possible Injury",
                                    "Suspected Injury" = "POSSIBLE INJURY",
                                    "Suspected Injury" = "Suspected Minor Injury",
                                    "Suspected Injury" = "SUSPECTED MINOR INJURY",
                                    "Suspected Injury" = "Suspected Serious Injury",
                                    "Suspected Injury" = "SUSPECTED SERIOUS INJURY",
                                    "No apparent injury" = "NO APPARENT INJURY"))
table(crash_clean$`Injury Severity`)

crash_clean <- crash_clean %>% rename(`Injury` = `Injury Severity`)
table(crash_clean$`Injury`)

crash_clean <- crash_clean %>% rename(report_num = `Report Number`)
                                        
# Downloading clean dataset

write.csv(crash_clean, "Final_Clean_Dataset.csv", row.names = FALSE)

# Stratified Sampling + Splitting data set

crash_clean <- 
  crash_clean %>% 
  mutate(`Injury` = ifelse(`Injury` == "Suspected Injury",1,0))

sampled_crash <- crash_clean %>%
  group_by(Injury) %>%
  sample_n(35000)

crash_split <- initial_split(sampled_crash, prop = 0.75)
crash_train <- training(crash_split)
table(crash_train$`Injury`)
crash_test <- testing(crash_split)
table(crash_test$`Injury`)

# Model Creation
 
log_model <- glm(`Injury` ~ ., family = binomial, data = crash_train %>% select(-report_num))

# Obtaining Coefficients
summary(log_model)
exp(log_model$coefficients)

# Scoring for training set and testing set 

scores <- predict(log_model, type = "response", data = crash_train)

scores2 <- predict(log_model, type = "response", newdata = crash_test)

# Results for training set

results_train <- tibble(
  `true_class` = as.numeric(crash_train$`Injury`),
  `prob_event` =  scores,
  `prob_not_event` = 1 - scores,
  `pred_class` = as.numeric(ifelse(scores > 0.4,
                                  "1","0"))
)

# Confusion Matrix for training set

cm <- results_train %>%
  mutate(
    true_class = as.factor(true_class),
    pred_class = as.factor(pred_class)
  ) %>%
  conf_mat(truth = true_class, estimate = pred_class)

print(cm)
autoplot(cm, "heatmap")

# Accuracy for Training set
accuracy_result_train <- results_train %>% mutate(true_class = as.factor(true_class),
                                                  pred_class = as.factor(pred_class)) %>% accuracy(truth = true_class, estimate = pred_class)
print(accuracy_result_train)

# Results for testing set

results_test <- tibble(
  `true_class` = as.numeric(crash_test$`Injury`),
  `prob_event` =  scores2,
  `prob_not_event` = 1 - scores2,
  `pred_class` = as.numeric(ifelse(scores2 > 0.4,
                                   "1","0"))
)

# Confusion matrix for testing set

cm2 <- results_test %>%
  mutate(
    true_class = as.factor(true_class),
    pred_class = as.factor(pred_class)
  ) %>%
  conf_mat(truth = true_class, estimate = pred_class)
print(cm2)
autoplot(cm2, "heatmap")

# Accuracy for testing set

accuracy_result_test <- results_test %>% mutate(true_class = as.factor(true_class),
                                                  pred_class = as.factor(pred_class)) %>% accuracy(truth = true_class, estimate = pred_class)
print(accuracy_result_test)

# ROC and AUC for training set

p <- ggplot(results_train, 
             aes(m = prob_event, d = true_class)) + 
  geom_roc(labelsize = 3.5, 
           cutoffs.at = 
             c(0.9,0.8,0.7,0.5 ,0.4,0.3,0.2,0.1)) +
  theme_minimal(base_size = 16)
print(p)
calc_auc(p)

# ROC and AUC for testing set

p2 <- ggplot(results_test, 
             aes(m = prob_event, d = true_class)) + 
  geom_roc(labelsize = 3.5, 
           cutoffs.at = 
             c(0.9,0.8,0.7,0.5, 0.4,0.3,0.2,0.1)) +
  theme_minimal(base_size = 16)
print(p2)
calc_auc(p2)

