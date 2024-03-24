# Data Sources

* **Our World in Data (OWID)**: Mpox cases by country and report date were downloaded from OWID's [`monkeypox` repository](https://github.com/owid/monkeypox/blob/main/owid-monkeypox-data.csv) hosted on GitHub. While this data was originally published by the World Health Organization (WHO), as of late 2023, WHO no longer makes this data publicly available for download. As a result, OWID's archived download of the data is used.

* **Wikimedia Foundation**: This analysis relies on three types of Wikipedia pageview data.
  1. *Language project-level* **daily pageview data** are queried from the [Wikimedia Analytics Query Service (AQS) REST API](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews) using the [{waxer}](https://wikimedia.github.io/waxer/articles/waxer.html) package.
  2. *Country-level* **differentially-private daily pageview data** are downloaded from the [Wikimedia Foundation public data repository](https://analytics.wikimedia.org/published/datasets/country_project_page_historical/). Further details on how this dataset is constructed can be found [here](https://analytics.wikimedia.org/published/datasets/country_project_page_historical/00_README.html).
  3. *Country-level* **total monthly pageview data** are queried from the [Wikimedia AQS REST API](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews#Pageviews_split_by_country). Further details on how this dataset is constructed can be found [here](https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews/Pageviews_per_project).
