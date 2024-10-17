# Automobile Price Analysis

## Description
This project was completed in 2022 as part of my early exploration of Machine Learning with RStudio. The focus is on building a robust model to analyze variations in automobile prices based on observed characteristics. Using the **Automobile Data Set**, which includes data from sources such as the *1985 Model Import Car and Truck Specifications* and *Insurance Collision Reports*, i apply data preprocessing, variable transformation, and modeling techniques to predict car prices.

## Objectives
- Predict automobile prices based on features such as engine type, dimensions, and fuel system.
- Address missing data using **MICE Imputation**.
- Group high-level categorical variables for improved model performance.

## Key Steps
1. **Data Cleaning and Imputation**: 
   - Handled missing values using predictive mean matching and logistic regression.
   - Removed variables with high percentages of missing values.

2. **Feature Engineering**: 
   - Created a new variable `volume` based on vehicle dimensions.
   - Converted `engineSize` from CID to CC for better interpretability.
   - Grouped categorical variables (e.g., `make`, `symboling`) to reduce levels.

3. **Modeling**:
   - Applied a **Generalized Additive Model (GAM)** for smooth transformations of non-linear covariates.
   - Used **Box-Cox transformation** to normalize the target variable `price`.
   - Performed **model selection** using AIC and SBC criteria.

4. **Model Diagnostics**:
   - Addressed multicollinearity by removing highly correlated variables.
   - Verified the final model assumptions using the **Breusch-Pagan test** and **Bootstrap** for confidence intervals.

## Results
- The final model showed significant improvement after transformation and selection, correctly predicting 92.9% of observations using a logistic model.
- **Outliers** and **heteroscedasticity** were successfully managed, improving model fit.

