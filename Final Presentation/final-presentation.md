Ames Housing Kaggle Competition
========================================================
author: CycloneSTAT: Emily Goren, Andrew Sage, and Haozhe Zhang
date: May 2, 2017
autosize: true

Missing Features
========================================================
- Categorical:
    - None if it doesn't apply (e.g., pool quality when there is no pool)
    - Others: impute by most frequent category
- Numerical: 
    - Garage year built: use house year built, add indicator for no garage
    - Yard frontage: impute by median for its neighborhood
    - Others: impute by median

Feature Selection
========================================================
- Change MSSubClass to a factor
- Indicators for combination of Condition 1 & 2
- Average room size (above ground square feet / rooms above grade)
- Indicators for 
    - Second floor
    - Remodeled
    - Brand new (built and sold in the same year)
    - Sold during late spring or early summer

Methods
========================================================
Tune using 10-fold repeated (10X) CV:
- Elastic net (using quadratic terms, first order interactions)
- Partial least squares regression (PLS)
- Random forest
- XGBoost

"Honest" Cross-Validation
========================================================
- Compared imputation done on each CV fold ("honest") to that done on entire data set before tuning and fitting.
- Accomplished by defining a custom model in caret.
- Results for elastic net using quadratic terms, first order interactions:

|   Imputation Method       |  CV  RMSE             | Kaggle RMSE           | 
| ------------   | --------------------- | --------------------- |
| Each CV Fold   | 0.17409               | 0.17501               |
| Entire Data    | 0.13384               | 0.13309               |



Stacking
========================================================
- Two methods:
   - Caret ensemble based on linear regression (performs stacking within each CV fold)
   - Stacked models based on Lasso (our own code)
   
- Coefficients for optimal model using Lasso:

|   Method      | Stacking Coefficient | 
| ------------  | ---------------------|
| (intercept)   |   0.0312             |
| XGBoost       |   0.5016             | 
| PLS           |   0.2480             |
| Elastic Net   |   0.2329             |

  
  
RMSE (on Log Scale)
========================================================

|   Method                                | CV RMSE | Kaggle Score  |
| --------------------------------------- | ----------------------|-------------- |
| Elastic Net                             | 0.13384               | 0.13309       |
| XGBoost                                 | 0.11607               | 0.12362       |
| Random Forest                           | 0.14342               | 0.14174       |
| PLS                                     | 0.12482               | 0.12290       |
| Caret Ensemble (E.Net, PLS, XGBoost)    | 0.12336               | 0.12284       |
| Lasso Stacking (E.Net, PLS, XGBoost)    | ?                     | 0.11774       |


#267 on Kaggle Scoreboard using Lasso Stacking 
