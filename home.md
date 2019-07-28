### Housing Values in Suburbs of Boston

This is the **Boston Housing** app. This app is based on [the Boston Housing Dataset](http://lib.stat.cmu.edu/datasets/boston), which is derived from information collected by the US Census Service concerning housing in the area of Boston Mass in 1970. A clean version of the same dataset was downloaded from [GitHub](https://github.com/selva86/datasets/blob/master/BostonHousing.csv). 

[The Boston Housing Dataset](http://lib.stat.cmu.edu/datasets/boston) contains 506 cases and has been used extensively throughout the literature to benchmark algorithms. The data was originally published by [Harrison, D. and Rubinfeld, D.L. Hedonic prices and the demand for clean air, J. Environ. Economics & Management, vol.5, 81-102, 1978.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.926.5532&rep=rep1&type=pdf)


#### Data Fromat

The data are 506 observations on 14 variables, `medv` being the target variable:
* `crim`:	per capita crime rate by town
* `zn`:	proportion of residential land zoned for lots over 25,000 sq.ft
* `indus`:	proportion of non-retail business acres per town
* `chas`:	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
* `nox`:	nitric oxides concentration (parts per 10 million)
* `rm`:	average number of rooms per dwelling
* `age`:	proportion of owner-occupied units built prior to 1940
* `dis`:	weighted distances to five Boston employment centres
* `rad`:	index of accessibility to radial highways
* `tax`:	full-value property-tax rate per USD 10,000
* `ptratio`:	pupil-teacher ratio by town
* `b`:	1000(B - 0.63)^2 where B is the proportion of blacks by town
* `lstat`:	percentage of lower status of the population
* `medv`:	median value of owner-occupied homes in USD 1000's

#### App Abilities

This app has multiple abilities as shown on different pages. With this app, you can

* Explore common numeric and graphical summaries of different variables in the dataset 
* Apply Principal Components Analysis (PCA) for certain variables in the dataset
* Explore the performance of two supervised learning models with different parameter settings on the prediction power of `medv`
* Use the trained model to do prediction
* Scroll through all the data recrods
* Interact with generated plots
* Save interested plots and data



