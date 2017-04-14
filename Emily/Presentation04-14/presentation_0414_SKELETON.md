CycloneSTAT's Kaggle Update
========================================================
author: Emily Goren, Andrew Sage, and Haozhe Zhang
date: April 14th, 2017
autosize: true

Feature Selection
========================================================
- Included quadratic terms and first order interactions in some regression-based methods
- Missing features
    - Categorical: added `NA` as a factor level (`addNA` function is handy)
    - Numerical: replaced with zero (after centering), added additional feature indicating missingness

Feature Selection Mysteries
========================================================
- What to do about "double field" factors? E.g.,
   - Condition1, Condition2
   - Exterior1st, Exterior2nd
- Feature importance specific to model
    - Fancy methods like Boruta (uses random forest) contraindicated with regression, for example?
- Be cognizant of factors with levels present in test, but not training, data

RMSE Progress
========================================================

|   Method     | Cross-validation RMSE |
| ------------ | ------- |
| Elastic Net  | TBD: currently running on stat server |
| Random GLM   | TBD: currently running on stat server |
| Andrew's     | ~ |
| ...          | ~ |
| Haozhe's     | ~ |
| ...          | ~ |
| Stacked?     | ~ |
