## Project Proposals

The following section outlines three potential project proposals, which are ordered according to our prioritization.

# 1. Sentiment about a company/a company's CEO predicts stock market prices

When Elon Musk posts random Tweets (not even related to his companies), the stock prices of his companies (e.g., Tesla) can abruptly increase or decrease. We would like to find out if this is really associated with the fact how people feel about his tweets.

- Dependent variable (DV): Stock market prices
- Data Retrieval DV: Loading stock prices directly into R with Tidyquant
- Independent variable (IV): Sentiment about a company and/or sentiment about a company's CEO
- Data Retrieval IV: Analyzing Twitter data from companies and their CEOs using sentiment analyses (e.g., VADER)

# 2. Sentiment about Joe Biden and income and educational level (comparing it with the results of the group from last year)

In previous years, a group of the Social Data Science course has examined the sentiment about Donald Trump with respect to income and educational level on a regional basis. We would like to run similar analyses about Joe Biden (very recent events!) and compare it with the data from the "Trump project". Do people really feel different about Joe Biden depending on their income and education?

- Dependent variable (DV): Sentiment about Joe Biden
- Data Retrieval DV: Analyzing Twitter data related to Joe Biden using sentiment analyses (e.g., VADER)
- Independent variable (IV): Income and educational level
- Data Retrieval IV: Analyzing census data

# 3. Linking Future Orientation Index to educational level of countries

In the course, we have discussed how FOI might be associated with the prosperity of countries. While  this makes sense, we believe another important factor is represented by the educational level of the countries. As education, especially college education, expands your horizon, we assume that countries with higher educational levels also show higher FOI.

- Dependent variable (DV): Future Orientation Index of countries
- Data Retrieval DV: Analyzing FOI using Google trends analysis
- Independent variable (IV): Educational level of countries
- Data Retrieval IV: Analyzing OECD/UN data