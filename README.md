# NBA Win-Loss Model

## Project Intro

The purpose of this project was to build a model to predict the pre-game probability that one NBA team will defeat another NBA team in their next game. A variety of factors were taken into account such as the offensive and defensive ability of both teams, which team was playing at home, and the amount of rest days for each team.

### Methods/Techniques Used

- Data Gathering/Cleaning
    - Pulling data from the hoopR API
    - Cleaning data with the tidyverse libraries
    - Feature engineering using well studied NBA advanced statistics
- Models Tested
    - Logistic Regression
    - Penalized Logistic Regression (Elastic Net)
    - XGBoost
    - Random Forest
    - PCA
- Dashboard Creation
    - Shiny for framework
    - gt for dynamic table creation


### R Libraries Used

- tidyverse family of libraries for data manipulation and cleaning
- caret for model building
- ggplot2 for plotting
- gt for building HTML tables used in reporting

## Project Description

**Goal:** Build a model which produces a well-calibrated pre-game probability for the chance that one NBA team has to beat another NBA team in their next game.
- Data Source: [hoopR](https://hoopr.sportsdataverse.org/index.html)
- Data Cleaning:
    - Parsed Play-by-Play data to create a unique dataset which allowed for pace-adjusted stats as well as the removal of garbage time
    - Had to explore some inconsistencies in the data due to reporting error or due to games being canceled because of COVID-19
- Feature Engineering:
    - Built pace-adjusted stats such as Offensive/Defensive Rating, Turnover Percentage, and a custom stat called Morey Rate - the percentage of possessions which results in at least one 3PTA, one shot in the paint, or one Free Throw Attempt
- Model Building:
    - Data from the 2003-2004 season until the 2020-2021 season were split into a 80%/20% training/testing split for model building
    - Data from the current 2021-2022 season were held back to see how the final model would perform on new data
    - Tested a few different models which had the potential to be the best performing model
    - Model hyperparameters were chosen with 10-fold Cross Validation
- Model Evaluation:
    - Since the goal was to build a model which is well-calibrated, the models were evaluated with a combination of Mean Log-Loss and Calibration Plots
    - All models were compared to each other as well as against a Naive model which assigns a 50% probability to each game
- Final Model Results:
    - A XGBoost model was the final model chosen
    - The XGBoost model had a **testing Mean Log-Loss of 0.522**, compared to a *testing Mean Log-Loss of 0.693 for the Naive model*
    - For the **held-out 2021-2022 season, the Mean Log-Loss of the XGBoost model was 0.566**, showing that the model generalized well
- Future Improvements:
    - Adjust for injuries and players who are sitting out
    - Adjust for how well/poor the team has been playing recently

**Lessons Learned:** With data like this, it is very easy to accidentally introduce data leakage. Being cognizant of this issue while producing the model input dataframes paid dividends while model building, it allowed our models to be optimized based on the data at any given point in time and not accidentally on future information.

**Why is this useful?**

While it is possible for the model outputs to be used for gambling or for content creation, the better use is to use the model to better understand the game of basketball. Since the model is tree-based, it is very easy to look at the importance of each variable.

The model shows that the three most important factors are the Win Percentage of each team and who is playing at home. This is important because it shows that especially in basketball the ability of a team to win past games dramatically impacts its ability to win in the future. This is not necessarily true in all sports - baseball for example - which are much more random than basketball.

## Featured Notebooks / Dashboards

- [Shiny App](https://justinrydell.shinyapps.io/nbaShiny)
    - Built with Shiny and deployed on shinyapps.io
- [Exploring Models](https://github.com/jrydell15/nba-wl-model/blob/main/notebooks/ModelBuilding.html)
    - Walks through the process of building many different models and choosing the best performing model

## Contributors

- Justin Rydell
    - Email: justin.rydell@tamu.edu
    - GitHub: jrydell15
- Jiajun Chen
    - Email: jiajunchen@tamu.edu
    - GitHub: MOOCJJC
- Edward Keefe:
    - Email: keefee@tamu.edu
    - GitHub: longsnoutter


