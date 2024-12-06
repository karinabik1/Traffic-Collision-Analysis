# Author: Julianna Larios

# Plots may be slightly different from presentation
# This is due to me making some code changes, to make the code look nicer and more legible
# Due to time constraints I was not able to make the plots to how they looked in the presentation
# However, values are not very off -- I think it may be due to me reformatting how the categorical variables are grouped
# Thus, I don't think there is much of a change to the model

# ------------------------
# Load in neccessary libraries
# ------------------------

library(randomForest)    
library(dplyr)          
library(ggplot2)         
library(caret)           
library(writexl)         
library(pROC)            

# ------------------------
# Read in data
# ------------------------

# Read in the dataset from a CSV file
data <- read.csv("clean_Crash_Report_Drivers_Data.csv")

# Check the structure and summary of the dataset
str(data) 
summary(data)  

# Display column names
colnames(data)

# ------------------------
# Convert Categorical Variables into Factors
# ------------------------

# Set a list of categorical variables in the dataset
categorical_columns <- c("Collision.Type", "Weather", "Surface.Condition", "Light", 
                         "Traffic.Control", "Driver.Substance.Abuse", "Driver.At.Fault", 
                         "Injury.Severity", "Driver.Distracted.By", "Vehicle.Damage.Extent", 
                         "Vehicle.Body.Type", "Vehicle.Movement", "Parked.Vehicle", "Vehicle.Make")

# Convert categorical variables into factors
data[categorical_columns] <- lapply(data[categorical_columns], factor)

# Convert the target variable to binary
data$Driver.At.Fault <- ifelse(data$Driver.At.Fault == "Yes", 1, 0)

# ------------------------
# Clean and preprocess data + Combine categories for each categorical column
# ------------------------

# Create a new dataset + Remove rows with any missing values from the dataset
data2 <- na.omit(data)

# Check for missing values in the dataset after cleaning
sapply(data2, function(x) sum(is.na(x)))

# -------- Weather

# Put weather conditions into broader categories
data2$Weather <- recode(data2$Weather,
                        "Rain" = "Precipitation",      
                        "Snow" = "Precipitation",      
                        "SNOW" = "Precipitation",      
                        "Sleet" = "Precipitation",     
                        "SLEET" = "Precipitation",     
                        "Sleet Or Hail" = "Precipitation", 
                        "Thunderstorm" = "Precipitation", 
                        "FOGGY" = "Fog",                 
                        "Fog, Smog, Smoke" = "Fog",     
                        "Severe Crosswinds" = "Windy",
                        "Windy" = "Windy",             
                        "UNKNOWN" = "Other",           
                        "Other" = "Other",             
                        "WINTRY MIX" = "Other",        
                        "BLOWING SNOW" = "Blowing Snow",
                        "CLOUDY" = "Cloudy",          
                        "Freezing Rain Or Freezing Drizzle" = "Precipitation",
                        "N/A" = "Other",               
                        "OTHER" = "Other",             
                        "RAINING" = "Precipitation",  
                        "Severe Crosswinds" = "Windy", 
                        "SEVERE WINDS" = "Windy",      
                        "BLOWING SAND, SOIL, DIRT" = "Blowing Sand/Dirt",
                        "CLEAR" = "Clear",
                        .default = "Other") # Default to 'Other' for unhandled cases  

# Display the frequency of each weather condition after recoding
table(data2$Weather)

# -------- Surface Condition

# Put surface conditions into broader categories
data2$Surface.Condition <- recode(data2$Surface.Condition,
                                  "Ice/Frost" = "Ice",
                                  "ICE" = "Ice",
                                  "WATER(STANDING/MOVING)" = "Water",
                                  "Water (standing, moving)" = "Water",
                                  "OIL" = "Oil",
                                  "SAND" = "Sand",
                                  "SLUSH" = "Slush",
                                  "UNKNOWN" = "Other",
                                  "OTHER" = "Other",
                                  "N/A" = "Other",
                                  "Mud, Dirt, Gravel" = "Mud/Gravel",
                                  "MUD, DIRT, GRAVEL" = "Mud/Gravel",
                                  "SNOW" = "Snow",
                                  "WET" = "Wet",
                                  "DRY" = "Dry",
                                  .default = "Other") # Default to 'Other' for unhandled cases

# Display the frequency of each surface condition after recoding
table(data2$Surface.Condition)

# -------- Light

# Put lighting conditions into broader categories
data2$Light <- recode(data2$Light,
                      "DARK -- UNKNOWN LIGHTING" = "Dark - Unknown Lighting",
                      "DAYLIGHT" = "Daylight",
                      "DAWN" = "Daylight",
                      "DUSK" = "Dusk",
                      "DARK LIGHTS ON" = "Dark - Lights On",
                      "DARK NO LIGHTS" = "Dark - No Lights",
                      "Dark - Lighted" = "Dark - Lights On",
                      "Dark - Not Lighted" = "Dark - No Lights",
                      "N/A" = "Other",
                      "UNKNOWN" = "Other",
                      "OTHER" = "Other",
                      .default = "Other") # Default to 'Other' for unhandled cases

# Display the frequency of each lighting condition after recoding
table(data2$Light)

# -------- Vehicle Body Type

# Put vehicle body types into broader categories
data2$Vehicle.Body.Type <- recode(data2$Vehicle.Body.Type,
                                  "SPORT UTILITY VEHICLE" = "SUV",                       
                                  "Sport Utility Vehicle" = "SUV", 
                                  "PICKUP TRUCK" = "Pickup",                             
                                  "Pickup" = "Pickup", 
                                  "MOTORCYCLE" = "Motorcycle",                           
                                  "Motorcycle - 2 Wheeled" = "Motorcycle", 
                                  "Motorcycle - 3 Wheeled" = "Motorcycle", 
                                  "MOPED" = "Moped",                                   
                                  "Moped Or motorized bicycle" = "Moped", 
                                  "AMBULANCE/EMERGENCY" = "Ambulance",                
                                  "AMBULANCE/NON EMERGENCY" = "Ambulance", 
                                  "FIRE VEHICLE/EMERGENCY" = "Fire Truck",              
                                  "FIRE VEHICLE/NON EMERGENCY" = "Fire Truck", 
                                  "POLICE VEHICLE/EMERGENCY" = "Police",                 
                                  "POLICE VEHICLE/NON EMERGENCY" = "Police", 
                                  "CARGO VAN/LIGHT TRUCK 2 AXLES (OVER 10,000LBS (4,536 KG))" = "Cargo Van",
                                  "OTHER LIGHT TRUCKS (10,000LBS (4,536KG) OR LESS)" = "Light Truck", 
                                  "OTHER" = "Other",                                      
                                  "OTHER BUS" = "Bus",                                   
                                  "Bus - Cross Country" = "Bus", 
                                  "Bus - Mini" = "Bus", 
                                  "Bus - Other Type" = "Bus", 
                                  "Bus - School" = "Bus", 
                                  "Bus - Transit" = "Bus", 
                                  "TRANSIT BUS" = "Bus", 
                                  "SCHOOL BUS" = "Bus", 
                                  "VAN" = "Van",                                          
                                  "Van - Cargo" = "Van", 
                                  "Van - Passenger (<9 Seats)" = "Van", 
                                  "LIMOUSINE" = "Limousine",                             
                                  "LOW SPEED VEHICLE" = "Low Speed Vehicle",             
                                  "RECREATIONAL VEHICLE" = "Recreational Vehicle",      
                                  "Recreational Off-Highway Vehicles (ROV)" = "Recreational Vehicle", 
                                  "ALL TERRAIN VEHICLE (ATV)" = "ATV",                  
                                  "All-Terrain Vehicle/All-Terrain Cycle (ATV/ATC)" = "ATV", 
                                  "Farm Equipment (Tractor, combine harvester, etc.)" = "Farm Equipment", 
                                  "FARM VEHICLE" = "Farm Equipment", 
                                  "Construction Equipment (backhoe, bulldozer, etc.)" = "Construction Equipment", 
                                  "TRUCK TRACTOR" = "Truck",                           
                                  "Single-Unit Truck" = "Truck", 
                                  "MEDIUM/HEAVY TRUCKS 3 AXLES (OVER 10,000LBS (4,536KG))" = "Truck", 
                                  "UNKNOWN" = "Unknown",                                  
                                  "N/A" = "Unknown", 
                                  .default = "Other") # Default to 'Other' for unhandled cases

# Display the frequency of each vehicle body type after recoding
table(data2$Vehicle.Body.Type)

# -------- Traffic Control

# Put traffic control conditions into broader categories
data2$Traffic.Control <- recode(data2$Traffic.Control,
                                "Bicycle Crossing Sign" = "Bicycle Sign",                        
                                "Pedestrian Crossing Sign" = "Pedestrian Sign",                   
                                "Pedestrian Crossing" = "Pedestrian Sign",                          
                                "Person" = "Person (Flagger/Law Enforcement)",                     
                                "Person (including flagger, law enforcement, crossing guard, etc." = "Person (Flagger/Law Enforcement)", 
                                "Flashing Traffic Control Signal" = "Flashing Signal",            
                                "FLASHING TRAFFIC SIGNAL" = "Flashing Signal", 
                                "Flashing Railroad Crossing Signal (may include gates)" = "Flashing Signal", 
                                "Lane Use Control Signal" = "Lane Control Signal",                
                                "Ramp Meter Signal" = "Lane Control Signal", 
                                "TRAFFIC SIGNAL" = "Traffic Signal",                              
                                "Traffic Control Signal" = "Traffic Signal", 
                                "STOP SIGN" = "Stop Sign",                                         
                                "YIELD SIGN" = "Yield Sign",                                        
                                "WARNING SIGN" = "Warning Sign",                                    
                                "Reduce Speed Ahead Warning Sign" = "Warning Sign", 
                                "Curve Ahead Warning Sign" = "Warning Sign", 
                                "Intersection Ahead Warning Sign" = "Warning Sign", 
                                "School Zone Sign" = "School Zone",                                 
                                "School Zone" = "School Zone", 
                                "SCHOOL ZONE SIGN DEVICE" = "School Zone", 
                                "Railway Crossing Device" = "Railway Crossing",                    
                                "RAILWAY CROSSING DEVICE" = "Railway Crossing", 
                                "Other Signal" = "Other Signal",                                    
                                "Other Pavement Marking (excluding edgelines, centerlines, or lane lines)" = "Other Pavement Marking", 
                                "Other Warning Sign" = "Other Warning Sign",                        
                                "NO CONTROLS" = "No Controls",                                       
                                "N/A" = "No Controls",                                              
                                "UNKNOWN" = "Unknown",                                              
                                .default = "Other") # Default to 'Other' for unhandled cases                                                

# Display the frequency of each traffic control after recoding
table(data2$Traffic.Control)

# -------- Collision Type

# Put collision types into broader categories
data2$Collision.Type <- recode(data2$Collision.Type,
                               "Angle" = "Angle",                                         
                               "ANGLE MEETS LEFT HEAD ON" = "Head On",                     
                               "ANGLE MEETS LEFT TURN" = "Left Turn",                      
                               "ANGLE MEETS RIGHT TURN" = "Right Turn",                    
                               "Front to Front" = "Front to Front",                         
                               "Front to Rear" = "Rear End",                                
                               "HEAD ON" = "Head On",                                      
                               "HEAD ON LEFT TURN" = "Left Turn",                           
                               "OPPOSITE DIR BOTH LEFT TURN" = "Left Turn",                
                               "OPPOSITE DIRECTION SIDESWIPE" = "Opposite Direction Sideswipe", 
                               "Rear To Rear" = "Rear End",                                 
                               "Rear To Side" = "Rear End",                                
                               "SAME DIR BOTH LEFT TURN" = "Left Turn",               
                               "SAME DIR REAR END" = "Rear End",                           
                               "SAME DIR REND LEFT TURN" = "Left Turn",                   
                               "SAME DIR REND RIGHT TURN" = "Right Turn",                 
                               "SAME DIRECTION LEFT TURN" = "Left Turn",                   
                               "SAME DIRECTION RIGHT TURN" = "Right Turn",                
                               "SAME DIRECTION SIDESWIPE" = "Same Direction Sideswipe",    
                               "Sideswipe, Opposite Direction" = "Opposite Direction Sideswipe", 
                               "Sideswipe, Same Direction" = "Same Direction Sideswipe",   
                               "SINGLE VEHICLE" = "Single Vehicle",                        
                               "STRAIGHT MOVEMENT ANGLE" = "Straight Movement Angle",      
                               "UNKNOWN" = "Unknown",                                       
                               "N/A" = "Unknown",                                         
                               .default = "Other") # Default to 'Other' for unhandled cases                                         

# Display the frequency of each collision type after recoding
table(data2$Collision.Type)

# -------- Vehicle Damage Extent

# Put vehicle damage extent conditions into broader categories
data2$Vehicle.Damage.Extent <- recode(data2$Vehicle.Damage.Extent,
                                      "DESTROYED" = "Severe Damage",                      
                                      "DISABLING" = "Severe Damage",                       
                                      "FUNCTIONAL" = "Functional Damage",                 
                                      "SUPERFICIAL" = "Minor Damage",                      
                                      "NO DAMAGE" = "No Damage",                           
                                      "OTHER" = "Other",                                   
                                      "UNKNOWN" = "Unknown",                               
                                      "N/A" = "Unknown",                                   
                                      "Vehicle Not at Scene" = "Vehicle Not Present",      
                                      .default = "Other") # Default to 'Other' for unhandled cases      

# Display the frequency of each vehicle damage extent after recoding
table(data2$Vehicle.Damage.Extent)

# -------- Vehicle Movement

# Put vehicle movement conditions into broader categories
data2$Vehicle.Movement <- recode(data2$Vehicle.Movement,
                                 "ACCELERATING" = "Moving",                   
                                 "MOVING CONSTANT SPEED" = "Moving",      
                                 "STARTING FROM LANE" = "Starting",           
                                 "STARTING FROM PARKED" = "Starting",          
                                 "SLOWING OR STOPPING" = "Slowing/Stopping",   
                                 "PARKED" = "Parked",                         
                                 "PARKING" = "Parked",                        
                                 "STOPPED IN TRAFFIC LANE" = "Stopped",       
                                 "STOPPED IN TRAFFIC" = "Stopped",            
                                 "ENTERING TRAFFIC LANE" = "Entering Lane",    
                                 "LEAVING TRAFFIC LANE" = "Leaving Lane",      
                                 "TURNING LEFT" = "Turning",                  
                                 "TURNING RIGHT" = "Turning",                  
                                 "MAKING LEFT TURN" = "Turning",               
                                 "MAKING RIGHT TURN" = "Turning",             
                                 "MAKING U TURN" = "U-Turn",                   
                                 "Making U-Turn" = "U-Turn",                   
                                 "NEGOTIATING A CURVE" = "Negotiating Curve", 
                                 "BACKING" = "Reversing",                     
                                 "CHANGING LANES" = "Changing Lanes",          
                                 "OVERTAKING/PASSING" = "Overtaking/Passing", 
                                 "PASSING" = "Overtaking/Passing",           
                                 "RIGHT TURN ON RED" = "Turning",              
                                 "SKIDDING" = "Skidding",                     
                                 "DRIVERLESS MOVING VEH." = "Driverless",     
                                 "UNKNOWN" = "Unknown",                      
                                 "OTHER" = "Other",                          
                                 .default = "Other") # Default to 'Other' for unhandled categories

# Display the frequency of each vehicle movement after recoding
table(data2$Vehicle.Movement)

# -------- Driver Substance Abuse

# Put driver substance abuse conditions into broader categories
data2$Driver.Substance.Abuse <- recode(data2$Driver.Substance.Abuse,
                                       "ALCOHOL CONTRIBUTED" = "Alcohol Present",      
                                       "ALCOHOL PRESENT" = "Alcohol Present",            
                                       "COMBINATION CONTRIBUTED" = "Combination Present", 
                                       "COMBINED SUBSTANCE PRESENT" = "Combination Present", 
                                       "ILLEGAL DRUG CONTRIBUTED" = "Illegal Drug Present",
                                       "ILLEGAL DRUG PRESENT" = "Illegal Drug Present",   
                                       "MEDICATION CONTRIBUTED" = "Medication Present", 
                                       "MEDICATION PRESENT" = "Medication Present",    
                                       "NONE DETECTED" = "No Substance Detected",        
                                       "Not Suspect of Alcohol Use, Not Suspect of Drug Use" = "No Suspicion of Substance Use", 
                                       "Not Suspect of Alcohol Use, Suspect of Drug Use" = "Suspect Drug Use",   
                                       "Not Suspect of Alcohol Use, Unknown" = "Unknown",  
                                       "OTHER" = "Other",                                
                                       "Suspect of Alcohol Use, Not Suspect of Drug Use" = "Suspect Alcohol Use",  
                                       "Suspect of Alcohol Use, Suspect of Drug Use" = "Suspect Alcohol and Drug Use",  
                                       "Suspect of Alcohol Use, Unknown" = "Suspect Alcohol Use (Unknown Drug Use)",  
                                       "UNKNOWN" = "Unknown",                         
                                       "Unknown, Not Suspect of Drug Use" = "Unknown (No Suspect Drug Use)",  
                                       "Unknown, Suspect of Drug Use" = "Unknown (Suspect Drug Use)",  
                                       "Unknown, Unknown" = "Unknown",                  
                                       .default = "Other") # Default to 'Other' for unhandled categories

# Display the frequency of each driver substance abuse category after recoding
table(data2$Driver.Substance.Abuse)

# -------- Driver Distracted By

# Put driver distracted by conditions into broader categories
data2$Driver.Distracted.By <- recode(data2$Driver.Distracted.By,
                                     "BY MOVING OBJECT IN VEHICLE" = "Distraction by Moving Object in Vehicle",  
                                     "BY OTHER OCCUPANTS" = "Distraction by Other Occupants",               
                                     "DIALING CELLULAR PHONE" = "Cellular Phone (Dialing)",            
                                     "DISTRACTED BY OUTSIDE PERSON OBJECT OR EVENT" = "Distraction by Outside Event",   
                                     "EATING OR DRINKING" = "Eating or Drinking",                           
                                     "INATTENTIVE OR LOST IN THOUGHT" = "Inattentive / Lost in Thought",     
                                     "LOOKED BUT DID NOT SEE" = "Looked but Didn't See",                    
                                     "MANUALLY OPERATING (DIALING, PLAYING GAME, ETC.)" = "Manually Operating Device",  
                                     "NO DRIVER PRESENT" = "No Driver Present",                            
                                     "NOT DISTRACTED" = "Not Distracted",                                  
                                     "OTHER ACTION (LOOKING AWAY FROM TASK, ETC.)" = "Other Action (Looking Away)",  
                                     "OTHER CELLULAR PHONE RELATED" = "Cellular Phone (Other Activities)", 
                                     "OTHER DISTRACTION" = "Other Distraction",                        
                                     "OTHER ELECTRONIC DEVICE (NAVIGATIONAL PALM PILOT)" = "Other Electronic Device",  
                                     "SMOKING RELATED" = "Smoking",                                     
                                     "TALKING OR LISTENING TO CELLULAR PHONE" = "Cellular Phone (Talking/Listening)", 
                                     "TALKING/LISTENING" = "Talking/Listening",                            
                                     "TEXTING FROM A CELLULAR PHONE" = "Texting from Cellular Phone",       
                                     "UNKNOWN" = "Unknown",                                                
                                     "USING DEVICE OBJECT BROUGHT INTO VEHICLE" = "Using Device from Outside the Vehicle", 
                                     "USING OTHER DEVICE CONTROLS INTEGRAL TO VEHICLE" = "Using Vehicle's Integral Controls",  
                                     .default = "Other") # Default to 'Other' for unhandled categories

# Display the frequency of each driver distraction category after recoding
table(data2$Driver.Distracted.By)

# ---------------------------------------------
# Check the structure and dimension of updated dataset
# ---------------------------------------------

# Check the structure of the cleaned data
str(data2)

# Get the dimensions of the dataset (number of rows and columns)
dim(data2)

# ---------------------------------------------
# Split Data into Training and Testing Sets
# ---------------------------------------------

# Set a seed for reproducibility
set.seed(888)

# Create a train-test split (80% for training, 20% for testing)
trainIndex <- createDataPartition(data2$Driver.At.Fault, p = 0.8, list = FALSE)

# Create training and testing datasets based on the split index
trainData <- data2[trainIndex, ]
testData <- data2[-trainIndex, ]

# Ensure again that the target variable 'Driver.At.Fault' is a factor for classification
trainData$Driver.At.Fault <- factor(trainData$Driver.At.Fault)

# Ensure 'Driver.At.Fault' is a factor
class(trainData$Driver.At.Fault)

# Remove rows with missing values from the training dataset
trainData <- na.omit(trainData)

# ---------------------------------------------
# Check Categorical Variable Levels
# ---------------------------------------------

# Check the number of levels for each categorical variable in the training data
# This is to ensure no columns with over 53 categories
sapply(trainData[, categorical_columns], nlevels)

# ------------------------
# Remove Problematic or Unused Columns
# ------------------------

# Remove specific columns from the training data that may not be useful for the analysis
trainData <- trainData[, !(names(trainData) %in% c("Parked.Vehicle"))]    
trainData <- trainData[, !(names(trainData) %in% c("Vehicle.Year"))]      
trainData <- trainData[, !(names(trainData) %in% c("Vehicle.Make"))]      
trainData <- trainData[, !(names(trainData) %in% c("Injury.Severity"))]  
trainData <- trainData[, !(names(trainData) %in% c("Crash.Date.Time"))]   

# ------------------------
# Did not standardize Speed Limit, since it was the only numeric value + not needed for RF
# ------------------------

# ------------------------
# Filter Out 'Unknown' Driver At Fault Values
# ------------------------

# Remove rows where Driver.At.Fault is labeled as 'Unknown'
trainData <- trainData %>% filter(Driver.At.Fault != "Unknown")

# Check the dimensions of the cleaned training dataset (to ensure target didn't get erased)
dim(trainData)

# ---------------------------------------------
# Train Random Forest Model
# ---------------------------------------------

# Convert Driver.At.Fault to a factor (Doing this again since I ran into issues if not)
trainData$Driver.At.Fault <- factor(trainData$Driver.At.Fault)

# Fit the Random Forest model for classification (using 300 trees and 3 variables for each split)
rf_model <- randomForest(Driver.At.Fault ~ ., data = trainData, importance = TRUE, ntree = 300, mtry = 3)

# Check the results of the Random Forest model
print(rf_model)

# ---------------------------------------------
# Visualize Predictions vs Actual
# ---------------------------------------------

# Predict the target variable on the test data
predictions <- predict(rf_model, testData)

# Plot Predicted vs Actual counts 
ggplot(data.frame(Actual = testData$Driver.At.Fault, Predicted = predictions), 
       aes(x = Actual, fill = factor(Predicted))) +  # Ensure 'Predicted' is treated as a factor
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Predicted vs Actual for Driver At Fault", 
       x = "Actual", y = "Count") +
  scale_fill_manual(
    values = c("0" = "lightpink", "1" = "lightblue"),
    labels = c("0" = "Not at Fault", "1" = "Driver at Fault"),  # Custom legend labels
    name = "Predicted"  # Set the legend title
  ) +
  theme_minimal()

# ---------------------------------------------
# Feature Importance Plot
# ---------------------------------------------

# Get the importance of features in the Random Forest model
importance_rf <- randomForest::importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[, 1])

# Sort the importance values
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Features", y = "Importance") +
  theme_minimal()

# ---------------------------------------------
# ROC Curve and AUC
# ---------------------------------------------

# Get predicted probabilities for the positive class (1) on the training data
probabilities <- predict(rf_model, newdata = trainData, type = "prob")[, 2]

# Compute ROC curve using the pROC library
roc_curve <- roc(trainData$Driver.At.Fault, probabilities)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add AUC to the plot for visuals
text(0.6, 0.4, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1.5)

# ---------------------------------------------
# Model Evaluation (Training Data)
# ---------------------------------------------

# Make predictions on the training data
predictionsTrain <- predict(rf_model, trainData)

# Create a confusion matrix
cm1 <- confusionMatrix(factor(predictionsTran), factor(trainData$Driver.At.Fault))
dim(cm1)

# Extract performance metrics from the confusion matrix
accuracy <- cm1$overall['Accuracy']
precision <- cm1$byClass['Pos Pred Value']
recall <- cm1$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

# Create a dataframe for plotting model performance metrics
metrics_df_train <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

# Plot the metrics with values displayed on the bars for the training data
ggplot(metrics_df_train, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylim(0, 1) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 5) +  # Add text labels
  labs(title = "Train Set Model Performance Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------
# Step 12: Confusion Matrix and Model Evaluation (Test Data)
# ---------------------------------------------

# Make predictions on the test data
predictionsTest <- predict(rf_model, testData)

# Create a confusion matrix
cm2 <- confusionMatrix(factor(predictionsTest), factor(testData$Driver.At.Fault))
dim(cm2)

# Extract performance metrics from the confusion matrix
accuracy <- cm2$overall['Accuracy']
precision <- cm2$byClass['Pos Pred Value']
recall <- cm2$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

# Create a dataframe for plotting model performance metrics for the test data
metrics_df_test <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

# Plot the metrics with values displayed on the bars for the test data
ggplot(metrics_df_test, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylim(0, 1) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 5) +  # Add text labels
  labs(title = "Test Set Model Performance Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


