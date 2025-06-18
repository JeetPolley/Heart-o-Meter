# --------------------------
# HEART ATTACK RISK PREDICTION
# --------------------------

# Step 1: Install & Load Packages
library(tidyverse)
library(caret)
library(pROC)
library(SHAPforxgboost)
library(shiny)
heart_data=read.csv("C:\\Users\\HP\\Downloads\\heart.csv")
glimpse(heart_data)

# Step 3: Data Cleaning
heart_clean <- heart_data %>%
  mutate(
    across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))),
    target = as.factor(target)  # Convert target to factor
  )

# Step 4: Feature Engineering
heart_clean <- heart_clean %>%
  mutate(
    age_group = cut(age, breaks = c(0, 40, 60, 100),
    high_bp = ifelse(trestbps > 140, 1, 0)
  )

# Step 5: Train-Test Split
set.seed(572)
train_index <- createDataPartition(heart_clean$target, p = 0.8, list = FALSE)
train_data <- heart_clean[train_index, ]
test_data <- heart_clean[-train_index, ]

# Step 6: Model Training (Logistic Regression)
model <- train(
  target ~ age + sex + cp + trestbps + chol + thalach + exang + oldpeak,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)
)

# Step 7: Model Evaluation
predictions <- predict(model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$target)
print(conf_matrix)

# ROC Curve
prob_predictions <- predict(model, test_data, type = "prob")[,2]
roc_curve <- roc(test_data$target, prob_predictions)
plot(roc_curve, main = paste0("AUC = ", round(auc(roc_curve), 2)))

# Step 8: SHAP Analysis (Interpretability)
# Note: Requires XGBoost model for full SHAP. Alternative for logistic regression:
var_imp <- varImp(model)
ggplot(var_imp, top = 5) + labs(title = "Top 5 Predictive Features")

# Step 9: Shiny App Deployment


library(shiny)
library(caret)

# Load your pre-trained model (replace with your actual model)
# model <- readRDS("your_model.rds")

# For testing, we'll create a dummy model
set.seed(123)
dummy_data <- data.frame(
  age = rnorm(100, 60, 10),
  thalach = rnorm(100, 150, 20),
  cp = sample(0:3, 100, replace = TRUE),
  target = sample(0:1, 100, replace = TRUE)
)

model <- train(
  as.factor(target) ~ age + thalach + cp,
  data = dummy_data,
  method = "glm",
  family = "binomial"
)

# Define UI
ui <- fluidPage(
  titlePanel("CardioGuard AI - Heart Attack Risk Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age (Years)", 60, min = 20, max = 100),
      numericInput("thalach", "Max Heart Rate", 50, min = 30, max = 220),
      selectInput("cp", "Chest Pain Type",
                  choices = c("Typical Angina" = 0,
                             "Atypical Angina" = 1,
                             "Non-Anginal Pain" = 2,
                             "Asymptomatic" = 3)),
      actionButton("calculate", "Calculate Risk")
    ),
    mainPanel(
      h3("Your Heart Attack Risk:"),
      textOutput("risk_output"),
      plotOutput("risk_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    # Create new data frame from inputs
    new_data <- data.frame(
      age = input$age,
      thalach = input$thalach,
      cp = as.numeric(input$cp)
    )
    
    # Make prediction
    risk <- predict(model, newdata = new_data, type = "prob")[,2]
    
    # Output results
    output$risk_output <- renderText({
      paste0(round(risk * 100, 1), "% risk of heart disease")
    })
    
    output$risk_plot <- renderPlot({
      barplot(c(risk, 1-risk), 
              names.arg = c("High Risk", "Low Risk"),
              col = c("red", "green"),
              ylim = c(0, 1),
              main = "Risk Probability")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)






# 1. Install and load packages
install.packages(c("shapper", "ggplot2"))
library(shapper)
library(ggplot2)

# 2. Prepare data (ensure numeric, no missing values)
features <- heart_data[, c("age", "thalach", "chol", "cp", "oldpeak")]
features <- na.omit(features)

# 3. Convert factors to numeric if needed
features$cp <- as.numeric(as.factor(features$cp))

# 4. Select specific observation(s) to explain
new_observation <- features[1:5, ]  # Explain first 5 patients

# 5. Calculate SHAP values
shap_values <- individual_variable_effect(
  model,
  data = features,
  new_observation = new_observation,
  predict_function = function(m, x) predict(m, x, type = "response"),
  nsamples = 100  # More samples = more accurate but slower
)

# 6. Plot results
plot(shap_values) +
  labs(title = "SHAP Values for Heart Disease Risk Prediction",
       subtitle = "How each feature impacts individual patient risk",
       x = "Feature Value Impact") +
  theme_minimal(base_size = 12)





# 1. Load required packages
library(xgboost)
library(SHAPforxgboost)
library(data.table)

# 2. Prepare data (must be numeric matrix)
features <- as.matrix(heart_data[, c("age", "thalach", "chol", "cp", "oldpeak")])
features <- na.omit(features)

# 3. Calculate SHAP values PROPERLY
shap_result <- shap.prep(
  xgb_model = model,
  X_train = features,
  top_n = 5  # Limit to top 5 features
)

# 4. Plot summary (fixed version)
shap.plot.summary(shap_result) +
  labs(title = "Top Heart Disease Risk Factors",
       subtitle = "SHAP values show feature importance",
       x = "SHAP Value Magnitude") +
  theme_minimal(base_size = 12)