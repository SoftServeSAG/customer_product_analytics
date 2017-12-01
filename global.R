#Libraries
require(shiny)
require(shinydashboard)
require(plotly)
require(DT)
require(dygraphs)
require(xts)

require(data.table)
require(dplyr)
require(plotly)
require(ggplot2)
require(moments)
require(prophet)
require(Rssa)

#Load data
load("data/wd.RData")
load("data/user_profile.RData")
load("data/users_rates.RData")
load("data/price_avg.RData")
wdays=rep(wd$week_day_name,100)

#Data for recommendations
load("data/PROD_CAT_DIC.RData")
load("data/UserRecommPROD1.RData")
load("data/users_12.RData")

#Products analysis
load("data/data_full_dt.RData")
load("data/holidays.RData")

#Source functions
source("functions/doFunctions.R")
source("functions/prophetFunction.R")
source("functions/convert_date_index_functions.R")
