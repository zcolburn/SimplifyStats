# SimplifyStats
In many analyses, pairwise group comparisons or groupwise descriptive statistics are produced for numerous variables. 'SimplifyStats' is an R package consisting of a set of functions that simplify this process.


[![Travis-CI Build Status](https://travis-ci.org/zcolburn/SimplifyStats.svg?branch=master)](https://travis-ci.org/zcolburn/SimplifyStats)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/zcolburn/SimplifyStats?branch=master&svg=true)](https://ci.appveyor.com/project/zcolburn/SimplifyStats)
[![codecov](https://codecov.io/gh/zcolburn/SimplifyStats/branch/master/graph/badge.svg)](https://codecov.io/gh/zcolburn/SimplifyStats)
[![DOI](https://zenodo.org/badge/138657516.svg)](https://zenodo.org/badge/latestdoi/138657516)

# Functions by category
## Groupwise descriptive statistics
The function **group_summarize** accepts a data frame as input and uses the names of user-specified columns of grouping variables to partition the data. For each unique combination of interactions between the grouping variables, univariate descriptive statistics are computed for another set of user-specified columns of numeric variables.


The specific statistics computed are:
* Sample size (N)
* Mean
* Standard deviation (StdDev)
* Standard error (StdErr)
* Minimum value (Min)
* First quartile value (Quartile1)
* Median
* Third quartile value (Quartile3)
* Maximum value (Max)
* Proportion of missing values (PropNA)
* Kurtosis
* Skewness
* Jarque-Bera test P value (Jarque-Bera_p.value)
* Shapiro-Wilk test P value (Shapiro-Wilk_p.value)


These values are returned in an object of class *group_summary*, which is a list of data frames where each element of the list is named according to the variable for which statistics were computed. Additional parameters, i.e. na.rm = TRUE, can be passed to *group_summarize*.

## Pairwise hypothesis testing
Like *group_summary*, the function **pairwise_stats** accepts as input a data frame and the names of user-specified columns of grouping variables. Unlike *group_summary*, *pairwise_stats* can accept only one numeric variable for analysis. Using a user-specified function, which must accept as both its first and second argument a vector of values corresponding to each group (i.e. t.test, wilcox.test, ks.test, or a custom function f(a,b)), every combination of group comparisons are made. In some cases the order in which these vectors are passed to the function matters, i.e. when settting *alternative = "greater"* in *t.test*. To account for this possiblity *two_way = TRUE* can be passed as a named parameter to *group_summarize*. This will test all possible pairs of unique grouping variable interactions in forward and reverse order. With this function, all two sample hypothesis tests can be quickly computed.
