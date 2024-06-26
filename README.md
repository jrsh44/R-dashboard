# R-dashboard

## About

This project contains visualized data from game League of Legends in form of dashboard. Application was created for Data Visualization Techniques course at Faculty of Mathematics and Information Science at Warsaw University of Technology.

## How to run

[The application is available online](https://jrsh.shinyapps.io/r-dashboard/)

You can easily update the database by implementing functions in the `./db` catalog.

```r
install.packages(c("jpeg", "png", "shinythemes", "base64enc", "httr", "networkD3", "ggplot2", "tidyverse", "plotly", "shiny"))
```

and then running `app.R` in main catalog.

## Data set

Application is based on data available through [Riot API](https://developer.riotgames.com/apis)

You can easily update databese by implemented functions in `./db` catalog.

First you need to create `keys.txt` file based on `keys.example.txt` and implement necessary values from [Riot API](https://developer.riotgames.com/apis)

Then in order to update data simply run one of `./db/...Update.R` file

## Authors

- [Jan Cwalina](https://github.com/Janusz99bis)
- [Bartłomiej Borycki](https://github.com/Bartekb3)
- [Mateusz Jarosz](https://github.com/jrsh44)
