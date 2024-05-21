# From Clicks to Cases: Leveraging Wikipedia Pageviews to Predict Mpox Cases in the United States

<!-- inspo from https://github.com/davidpomerenke/protest-impact -->

## Abstract

As the world becomes increasingly interconnected and climate change elevates the risk of zoonotic spillover events, the public becomes ever more susceptible to global-scale outbreaks. Traditional disease surveillance methods are prone to under-reporting and time lags. By contrast, Wikipedia pageviews offer a real-time and cost-effective open source resource for tracking online health-related information-seeking behavior with the potential for enhancing global disease surveillance. This paper investigates the value of anonymized country-level Wikipedia pageviews data for predicting case incidence during the 2022-2024 mpox outbreak in the United States. The methods employed in this study involve a combination of quan- titative techniques aimed at increasing understanding of the relationship between online behaviors and disease dynamics. A lag analysis correlating mpox cases and pageviews for mpox-related Wikipedia articles at different time lags was conducted to assess the variation in directionality between pageviews and cases across mpox-related articles. This was followed by a multivariate linear regression analysis aimed at predicting mpox incidence based on pageview data. Finally, impulse response and Granger-causality tests were performed to further analyze the directionality of the relationship between online activity and mpox cases. The study’s findings underscore the potential of Wikipedia traﬀic as a predictive tool for public health trends, revealing a bidirectional relationship between pageviews and mpox cases that unfolds over time. The predictive models struggled with accuracy, highlighting the need for further model refinement to adequately account for the complexity of online attention and disease dynamics.

[➡️ Read the paper](https://raw.githubusercontent.com/smkerr/mpox-wiki-analysis/main/7-paper/thesis.pdf)

## Repository structure

    .
    ├── README.md
    ├── 1-proposal
    │   ├── data-report
    │   └── pre-analysis-plan
    ├── 2-literature
    ├── 3-data
    │   ├── mpox-cases
    │   ├── mpox-news
    │   ├── mpox-studies
    │   ├── wikipedia
    │   └── output
    ├── 4-code
    ├── 5-tables
    ├── 6-figures
    ├── 7-paper
    └── 8-poster

<!-- ## Sources -->

<!-- ## License --> 
