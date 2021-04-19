# Boston-Crime-Analysis-with-R

Authors: LIN Tzu Chun (Ryan), Ahmad Fady GANIS, CHE Ning, GUO Yucheng, LI Ben, YAN Ruiyun

## Problem Statement

The numbers of records of crime and shootings in Boston increased dramatically from 2019 to 2020. And the crime number is projected to be still maintained at a high level in 2021. Yet the large number of crimes can be related to some reality factors like unemployment, as well as the events such as the COVID-19 or George Freud. It will be important to explore the root of crime and put forward relevant suggestions.
The objective of this project is to analyze the overall crime data in Boston using descriptive statistics techniques based on the dataset provided by Boston Police Department (BPD). In February 2020, COVID-19 broke out in Boston. In order to compare the impact of COVID-19 on the crime rate in Boston and get the trend of the impact. First, we carefully analyze the crime situation in Boston before 2020, and then we analyze the crime situation in Boston before and after February 2020. If there is a change, use the hypothesis test to analyze the variables that affect the change and use time series to build predictive model to predict the number of crimes in 2021.

## DATA PREPARATION
We used the dataset from [Crimes in Boston | Kaggle](https://www.kaggle.com/AnalyzeBoston/crimes-in-boston) and [Boston Open Data](https://www.mass.gov/crime-statistics), for year 2016 to 2020. Since the data for 2015 is incomplete, lack of February to April, we finally chose to use crime data from 2016 to 2020. There are 17 columns (Table 1) and more than 800,000 rows in the original dataset but at the same time there are many identical duplicate rows. Each row represents one crime. We took steps to finish the data cleaning.
Imported raw data into R.

### T-Test for Difference in Means
As part of the Inferential Statistics of the project, we use T Test to exam whether the means of the numbers of crimes by month before and after Covid-19 are different or not. Firstly, we need to divide data into 2 parts, the data before and after Covid-19, then we would test the means of the numbers of all kinds of crime.
Take numbers of Sex Offence for example as shown above, p-value = 2.098e-06, so we can conclude that there is significant difference in terms of means between data after Covid-19 and data before Covid- 19. And concluded result are shown as below table.
The mean of the total crime number decreased significantly (p-value is less than 5%), also some categories of crime, like Drugs, Vehicle accident, and Weapons, dropped as well. According to Boston news, the city was shut down, hence people seldom went out then the number of vehicle accident dropped down. Also, due to border closures and movement restrictions in many countries to stem the spread of Covid-19, the global supply of illegal drugs and firearms disrupted. On the other hand, because people spent more time staying home, the number of sex harassment and offence increased a lot. Also, the big increase of the number of shooting followed by the police killing of George Floyd in Minneapolis and reduced the police force in Boston.

### Correlations Between Variables
Influence on crimes by Covid-19 is consequence, and we tried to find out the root cause. Therefore, we created the correlation heatmap to observe correlations between different variables, including the dropout rate, the number of crimes, the education rate, the unemployment rate, and the number of shooting by year.
Then, we found out that the number of shooting and the unemployment rate have the strong correlation. Furthermore, we have done the correlation test for these 2 variables, and the result are shown as below. Since p-value is < 5% and correlation is 0.96, we reject the null hypothesis. We have enough evidence to conclude the number of shooting and the unemployment rate have the strong correlation.
Moreover, higher the unemployment rate is, more the number of shooting is. The right side of the chart shows that data have stronger positive linear correlation. Then, we only use data after Covid-19 to do the scatter plot and the regression.
The Boston unemployment rate jumped to 19.3% in June, the highest level since the Great Depression, as many businesses shut down or severely curtailed operations to try to limit the spread of the deadly coronavirus. Unluckily, 141 shooting cases also came together, the highest level as well.
People who were lay off with anger to the Floyd event increased the number of shooting. Luckily, With COVID-19 cases falling, vaccines rolling out, and prevention measures, the unemployment rate was decreasing and increased the number of shooting was dropping. And we expect that when the unemployment rate backs to the normal level, the number of shooting will also back.

## CONCLUSION

While conducting the analysis, we realized that model has some limitations and, in this section, we have tried to provide some ideas to improve it.

### Model Limitations
Because of difficulties in collecting data, convenience of calculations, we made some assumptions to develop our model. These assumptions have limited our model in the following respects:
- Predicting the numbers of crimes is so difficult, because data before and after Covid-19 have the huge difference. Hence, we can’t use historical data to predict. To improve the app, we will collect more kinds of data, like demographic data.
- We categorized crimes manually, so categories may not be so accurate.

### Lessons Learnt
We believe that Covid-19 has a huge influence on crimes in Boston, and we suggest that police should focus on reducing crimes of violence, shooting, and sex offense.

### Summary
We believe that when police understand these crimes better, they will be better able to respond to crimes. The results of these analyses and the interactive R Shiny App can help the Boston Police Department efficiently allocate its limited resources and, on the other hand, provide helpful managerial recommendations and even reduce or prevent crimes.
