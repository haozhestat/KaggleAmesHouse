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
    - Numerical: replaced with zero (after centering), added additional feature indicating missingness if $>$ 1 case

Feature Selection Mysteries
========================================================
- What to do about "double field" factors? E.g.,
   - Condition1, Condition2
   - Exterior1st, Exterior2nd
- Feature importance specific to model
    - Fancy methods like Boruta (uses random forest) contraindicated with regression, for example?
- Be cognizant of factors with levels present in test, but not training, data

Current Progress
========================================================

|   Method                                | Cross-validation RMSE |
| --------------------------------------- | -------- |
| Elastic Net (tuned to Ridge Regression) | 27267.2  |
| Random GLM (order 2)                    | 609239.4 |
| Random Forest                           | 27928.9  |
| Conditional Random Forest               | 30489.9  |
| PLS                                     | 32350.0  |

Kaggle Scorboard: 0.12746 (#987) using average of random forest and PLS predictions.
