Updated Project Proposal -

Jullian Schrup
Leading Indicator Analysis - Revenue Edition

## Abstract 
Connecting the dots between website traffic and revenue is no easy feat when the website has no direct attribution action (e.g. Buy Now or Checkout). It is my mission to use advanced data science and machine learning techniques to measure site activity for my client (name masked) and accurately predict revenue in order to make the connection between website traffic and revenue. By Backwards engineering and using a series of statistical tests, I can determine how customers are using a highly informative site to make their purchasing decisions in order to define the leading indicators of revenue. 

## Context 
_- What is the problem?_
My client’s website has only a basket transfer capability and not a direct checkout option. A high degree of attrition occurs during the checkout process as prices are not the same with each basket transfer (Costco will offer tires at a higher price than the client’s website). The nature of the product is that the customer has to do a high degree of labor to acquire the tires. This adds  complexity to the process that and is concerning. The vast majority of customers fall away…
Some traffic to the website are customers further down the marketing funnel (bottom of the funnel, or BOFU) and are looking to buy. How do I attribute Revenue with website traffic & marketing investment?
_- Why is it important to solve?_
By identifying the many routes a prospective customer may take to purchase tires, we may identify channels that are far more important than others. This will be able to have systemic effect on the strategy and tactics the marketing firms and their army of partners that this fortune 500 company commands. 
The client can direct funds and attribute revenue to customer landing pages and site channels by being able to attribute stops in the customer journey as highly correlated to revenue.  

**_ Note: This isn’t the customer journey analysis. This is Site Activity and Revenue Attribution analysis that can be operationalized into a customer journey analysis._

## Proposal 
_- What is the specific question are you trying to answer?_
How does one attribute Revenue with website traffic & marketing investment?

_- How will you answer it?_
The intention here is to survey my clients vast ecommerce empire and download traffic and interactions. The theory goes that the interactions and traffic that most align with the ebbs and flows of revenue will be a strong indicator of users who made a purchase and thus we can attribute the website as a key tool in the user purchase journey.

_- What do you expect/hope to find?_
I hope to find two things: 
1. New key performance indicators that will be highly indicative of user purchases. What traffic is most correlated with Revenue? How can we test for causation?
2. Online Marketing attribution of Revenue – how much does the online empire net the retail sales? What percent of the overall Revenue can the online marketers lay claim to?

## Introduction Conclusion 
_- Summarize the problem and your solution._
How do you attribute Revenue with website traffic & marketing investment less any direct attribution options (e.g. Buy Now or Check Out). This analysis will look at all day level data from the client’s website traffic & interactions to determine key channels that influence prospective customers to move further down the marketing funnel. 

_- If applicable, describe how your solution could be important beyond your specific context--i.e. can it be generalized?_
These New KPIs can be used in the following ways:
•	Track Ecommerce health
•	Calculate net marketing contribution
•	Segment customers and their journey
•	Construct scenarios where certain variables (or coefficients) are optimized 
•	Influence Tactical and Strategic decisions on where to spend money on which projects
•	To optimize the user journey 
- Discuss limitations and potential future directions to take the project.
•	Correlation is not causation. One of the biggest mathematical hurdles will identifying causation.
•	Validating the KPIs. How do we know they are tracking as intended?
•	Developing an accurate model that preforms well when shocks are introduced.
•	Model deployment may mean we’d need to predict the KPIs, which is messy.
  
## Analysis
_Description of the data_
I have access to Day Level Traffic Data. This includes all site traffic. This excludes Personal Identification Data or Segmentations. 
_Origins_
This data is coming from Adobe Analytics (AA) that is using an AA tag manager to fire at a hit and visit level. 

_Limitations_
-	The limitations are immense. Our primary issue is that some data is mis-tagged and is not clean. This mis-tagging is dirty data that can lead us down rabbit holes that should not be chased. However – the chance that mis-tagged data will be a false positive is extremely low. It’s more likely that mis-tagged data will be a false negative in that the data will say that the site traffic is not important when it actually is. 
-	The sight has undergone major changes in the last 12 months – including a full redesign in August & November on both site properties. 
-	Website data of this sort is brimming with multi-collinearity. That is extremely dangerous and means we will need to use tools such as a shapely regression to try and tease apart the r-squared.
-	Seasonality greatly affects this business and we will need to test for autocorrelation.
-	The data is not at a user data – so we aren’t tracking user flow – just volume of traffic.

_Features_
-	We can parse the features any way we want. The data can be aggregated to incorporate specific actions on certain sites – this is where feature engineering and qualitative analysis will be incorporated to select highly indicative features. 
-	The features of this data is that we can segment traffic based on previous site interactions – e.g. Do folks who visit the Warranty or Offer Page often proceed to a product view page? If so, does this ‘journey’ track with revenue? 
-	The data is extremely dynamic at the Day Traffic Level. We can apply City/Device type segments before importing the data.
-	I have 30-40 weeks of clean data (as we do the analysis, we can feed in more data as the weeks pass)

__**Exploratory Data Analysis**__

These beginning stages of exploration is intended to reduce dimensionality of our variables – we have 30+ with only 30-40 rows of data (weeks). One of the best initial tools for this is a PCA and qualitative tools to reduce the number of columns. Our goal is to leave this analysis with a handful of variables that can accurately predict revenue – how many variables is not known in this initial stage, but we are hoping for less than 10.

The initial analysis is a PCA and work with a Data Science (DS) expert who has over 7 years of experience with the data. I have spent around 3 hours navigating the site and interviewing my family members as they went navigated the site. I took careful notes and have been building Calculated Metrics based on their interactions. Utilizing the DS subject matter expert we created around 30 calculated metrics that should offer us a strong glimpse into revenue predictions. 

I have exported all the traffic data into a CSV and imported it to conduct a Principle Component Analysis ( in R. (See PCA). This PCA found that all our metrics can be clumped into three principle components that have extraordinary revenue predicting capabilities. We are using a Mean Average Precent Error (MAPE) and Root Mean Square Error (RMSE) to determine model accuracy and health. A quality MAPE score is around less than 10% and this preliminary PCA is netting a MAPE of 5.6% and an RMSE of around 6% of the mean (which is very good). This validates the PCA’s – when we open up the Principle Components we find some interesting findings.

Catalog Interaction and Catalog search is highly correlated with Revenue. This is very surprising as it is the current theory that a “Store Search” or “Basket Transfer” will be the strongest predictors of Revenue. It is assumed that these are the last stops right before you head to a store to purchase the product. 

What does this mean? With this principle components we can now begin testing a model that can predict revenue based on the insights found in the PCA. We have reduced many of the ‘entry-pages’ down – and we are only left with a handful of entry pages that seem to have any prediction value. 
