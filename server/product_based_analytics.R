#----------------------------------------
#Reactive expressions with data subsets
#----------------------------------------
#Transactional data for selected department/commodity/sub-commodity/product
transactional_product_data <- reactive({
  
  req(!is.null(input$pa_departments_list) & 
        !is.null(input$pa_commodity_list) & 
        !is.null(input$pa_sub_commodity_list) & 
        !is.null(input$pa_product_list))
  
  df <- data_full_dt[DEPARTMENT == input$pa_departments_list]
  
  if (!(input$pa_commodity_list %in% c("All", ""))) {
    df <- df[COMMODITY_DESC == input$pa_commodity_list]
  }
  
  if (!(input$pa_commodity_list %in% c("All", ""))) {
    df <- df[COMMODITY_DESC == input$pa_commodity_list]
  }
  
  if (!(input$pa_sub_commodity_list %in% c("All", ""))) {
    df <- df[SUB_COMMODITY_DESC == input$pa_sub_commodity_list]
  }
  
  if (!(input$pa_product_list %in% c("All", ""))) {
    df <- df[PRODUCT_ID == input$pa_product_list]
  }
  
  req(nrow(df) > 0)
  
  df
})


#----------------------------------------
#Rendering of input elements
#----------------------------------------
#List of available departments
output$pa_render_departments_list <- renderUI({
  
  departments <- as.character(unique(data_full_dt$DEPARTMENT))
  departments <- departments[departments != " "]
  
  selectInput(inputId = "pa_departments_list", 
              label = "Select product department", 
              choices = departments,
              selected = 1, multiple = FALSE)
  
})

#List of commodities
output$pa_render_commodity_list <- renderUI({
  
  req(!is.null(input$pa_departments_list))
  
  commodities <- data_full_dt[DEPARTMENT == input$pa_departments_list]
  commodities <- as.character(unique(commodities$COMMODITY_DESC))
  
  selectInput(inputId = "pa_commodity_list", 
              label = "Select product commodity", 
              choices = c("All", commodities),
              selected = 1, multiple = FALSE)
  
})

#List of sub_commodities
output$pa_render_sub_commodity_list <- renderUI({
  
  req(!is.null(input$pa_commodity_list))
  
  if (input$pa_commodity_list == "All") {
    available_choices = ""
  } else {
    sub_commodities <- data_full_dt[DEPARTMENT == input$pa_departments_list & COMMODITY_DESC == input$pa_commodity_list]
    available_choices <- c("All", as.character(unique(sub_commodities$SUB_COMMODITY_DESC)))
  }
  
  selectInput(inputId = "pa_sub_commodity_list", 
              label = "Select product sub-commodity", 
              choices = available_choices,
              selected = 1, multiple = FALSE)
  
})

#List of products
output$pa_render_products_list <- renderUI({
  
  req(!is.null(input$pa_sub_commodity_list))
  
  if (input$pa_sub_commodity_list %in% c("All", "")) {
    available_choices = ""
  } else {
    products <- data_full_dt[DEPARTMENT == input$pa_departments_list & COMMODITY_DESC == input$pa_commodity_list & SUB_COMMODITY_DESC == input$pa_sub_commodity_list]
    available_choices <- c("All", sort(as.character(unique(products$PRODUCT_ID))))
  }
  
  selectInput(inputId = "pa_product_list", 
              label = "Select product", 
              choices = available_choices,
              selected = 1, multiple = FALSE)
  
})

#----------------------------------------
#Product purchase history
#----------------------------------------
#CALCULATIONS
first_product_bought <- reactive(
  convert_index_to_date(
    min(transactional_product_data()$DAY)
  )
)

last_product_bought <- reactive(
  convert_index_to_date(
    max(transactional_product_data()$DAY)
  )
)

#Calculating average number of purchases per week
product_weeks_purchases <- reactive({
  
  weeks_purchases <- transactional_product_data()
  
  weeks_purchases <- weeks_purchases[, list(n_baskets = length(unique(BASKET_ID))), by = WEEK_NO][order(WEEK_NO)]
  avg_per_week <- round(sum(weeks_purchases$n_baskets) / (102 - min(weeks_purchases$WEEK_NO)), 2)
  
  #Set WEEK_NO as factor for visualization
  weeks_purchases$WEEK_NO <- factor(weeks_purchases$WEEK_NO, levels = seq(1, 102))
  
  weeks_purchases <- copy(week_first_week_day_mapping())[weeks_purchases, n_baskets := i.n_baskets, on = "WEEK_NO"]
  weeks_purchases <- weeks_purchases[is.na(n_baskets), n_baskets := 0]
  
  return(list(avg_per_week = avg_per_week, weeks_purchases = weeks_purchases))
  
})

#----------------------------------------
#VISUALZATION
output$box_first_product_bought <- renderValueBox({
  
  valueBox(
    first_product_bought(), "First purchase", icon = icon("calendar-check-o")
  )
  
})

output$box_last_product_bought <- renderValueBox({
  
  valueBox(
    last_product_bought(), "Last purchase", icon = icon("calendar-check-o"),
    color = "orange"
  )
  
})

output$box_average_number_purchases_per_week <- renderValueBox({
  
  valueBox(
    product_weeks_purchases()$avg_per_week, "Average number of purchases per week for selected product", icon = icon("shopping-basket"),
    color = "purple"
  )
  
})

output$plot_weeks_purchases <- renderPlotly({
  
  plot_ly(product_weeks_purchases()$weeks_purchases, x = ~first_day, y = ~n_baskets, type = 'bar',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5))) %>%
    layout(yaxis = list(title = ""), xaxis = list(title = "Week"))
  
})

#----------------------------------------
#Product profile
#----------------------------------------
#CALCULATIONS
age_profile_full <- reactive({
  
  data_full_dt[, list(AGE_DESC_COUNT_FULL = length(unique(household_key))), by = AGE_DESC]
  
})

age_profile_data <- reactive({
  
  age_profile_proportion <- transactional_product_data()
  age_profile_proportion <- age_profile_proportion[, list(AGE_DESC_COUNT = length(unique(household_key))), by = AGE_DESC]
  age_profile_proportion <- age_profile_proportion[copy(age_profile_full()), AGE_DESC_COUNT_FULL := i.AGE_DESC_COUNT_FULL, on = "AGE_DESC"]
  age_profile_proportion <- age_profile_proportion[, age_proportion:= round(AGE_DESC_COUNT * 100 / AGE_DESC_COUNT_FULL, 2)]
  
  age_profile_proportion[complete.cases(age_profile_proportion)]
})


income_profile_full <- reactive({
  
  data_full_dt[, list(INCOME_DESC_COUNT_FULL = length(unique(household_key))), by = INCOME_DESC]
  
})

income_profile_data <- reactive({
  
  income_profile_proportion <- transactional_product_data()
  income_profile_proportion <- income_profile_proportion[, list(INCOME_DESC_COUNT = length(unique(household_key))), by = INCOME_DESC]
  income_profile_proportion <- income_profile_proportion[copy(income_profile_full()), INCOME_DESC_COUNT_FULL := i.INCOME_DESC_COUNT_FULL, on = "INCOME_DESC"]
  income_profile_proportion <- income_profile_proportion[, income_proportion:= round(INCOME_DESC_COUNT * 100 / INCOME_DESC_COUNT_FULL, 2)]
  income_profile_proportion <- income_profile_proportion[, INCOME_DESC := factor(INCOME_DESC, 
                                                                                 levels = c("Under 15K", "15-24K", "25-34K", "35-49K", "50-74K", "75-99K", "100-124K", "125-149K", "150-174K", "175-199K", "200-249K", "250K+"))
                                                         ]
  income_profile_proportion
})

#----------------------------------------
#VISUALIZATION
output$age_profile <- renderPlotly({
  
  plot_ly(age_profile_data(), 
          x = ~AGE_DESC, 
          y = ~age_proportion, type = 'bar',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5))) %>% 
    layout(yaxis = list(title = "% of customers from group"), xaxis = list(title = "Age"))
  
})


output$income_profile <- renderPlotly({
  
    
  plot_ly(income_profile_data(), 
          x = ~INCOME_DESC, 
          y = ~income_proportion, type = 'bar',
          marker = list(color = 'rgba(219, 64, 82, 0.7)',
                        line = list(color = 'rgba(219, 64, 82, 1.0)',
                                    width = 2))
  )  %>% layout(margin = list(b = 80), yaxis = list(title = "% of customers from group"), xaxis = list(title = "Income"))
  
})

#----------------------------------------
#Demand prediction
#----------------------------------------
#CALCULATIONS
future_demand <- eventReactive(input$pa_analyse_product, {

  withProgress(message = "", value = 0, style="old", {
    
    res <- predict_prophet(DF = data.frame(transactional_product_data()), maxd = 711, len = as.integer(input$pa_date_range), minR = 150)
    
    dt <- res$load[[1]]
    dt <- dt[dt$f == "Predicted", ]
    dt <- dt[, c("date", "count")]
    dt$count_lower <- res$forecast_all[,'count_lower'] %>% round(0) %>% lost_item()
    dt$count_upper <- res$forecast_all[,'count_upper'] %>% round(0)
  })
  
  return(list(predictions = dt, chart = res$load[[2]]))
  
})

#----------------------------------------
#VISUALIZATION
output$demand_predictions_data <- renderDataTable({
  
  datatable(future_demand()$predictions,
            rownames = FALSE,
            colnames = c("Date", "Count", "Lower bound", "Upper bound"),
            escape = FALSE,
            selection = "none",
            options = list(autoWidth = FALSE,
                           align = 'center',
                           sDom = '<"top">rt<"bottom">ip',
                           scrollX = TRUE,
                           info = FALSE,
                           paging = TRUE,
                           ordering=FALSE,
                           pageLength = 10))
  
})

output$demand_predictions_chart <- renderDygraph({
  
  future_demand()$chart
  
})

#----------------------------------------
#Customers to recommend
#----------------------------------------
#CALCULATIONS
#Customers to recommend data
users_to_recommend_data_subset <- reactive({
  
  req(!is.null(input$pa_departments_list) & 
        !is.null(input$pa_commodity_list) & 
        !is.null(input$pa_sub_commodity_list) & 
        !is.null(input$pa_product_list))
  
  df <- UPROD1[DEPARTMENT == input$pa_departments_list]
  
  if (!(input$pa_commodity_list %in% c("All", ""))) {
    df <- df[COMMODITY_DESC == input$pa_commodity_list]
  }
  
  if (!(input$pa_commodity_list %in% c("All", ""))) {
    df <- df[COMMODITY_DESC == input$pa_commodity_list]
  }
  
  if (!(input$pa_sub_commodity_list %in% c("All", ""))) {
    df <- df[SUB_COMMODITY_DESC == input$pa_sub_commodity_list]
  }
  
  if (!(input$pa_product_list %in% c("All", ""))) {
    df <- df[PRODUCT_ID == input$pa_product_list]
  }
  
  df[order(-RATE)]
})

users_to_recommend <- eventReactive(input$pa_analyse_product, {
  
  withProgress(message = "", value = 0, style="old", {
    
    preferences <- users_to_recommend_data_subset()[FLAG == 0]
    preferences <- preferences[, c("DEPARTMENT", "COMMODITY_DESC", "SUB_COMMODITY_DESC", "PRODUCT_ID", "USER", "RATE"), with = FALSE]
    preferences$RATE <- round(preferences$RATE, 2)
    
    recommendations <- users_to_recommend_data_subset()[FLAG == 1 & RATE >= 0.6]
    recommendations <- recommendations[, c("DEPARTMENT", "COMMODITY_DESC", "SUB_COMMODITY_DESC", "PRODUCT_ID", "USER", "RATE"), with = FALSE]
    recommendations$RATE <- round(recommendations$RATE, 2)
    
  })
  
  return(list(preferences = preferences, recommendations = recommendations))
  
})

#----------------------------------------
#VISUALIZATION
output$product_preferences <- renderDataTable({
  
  datatable(users_to_recommend()$preferences,
            rownames = TRUE,
            escape = FALSE,
            selection = "none",
            options = list(autoWidth = FALSE,
                           align = 'center',
                           sDom = '<"top">rt<"bottom">ip',
                           scrollX = TRUE,
                           info = FALSE,
                           paging = TRUE,
                           ordering=FALSE,
                           pageLength = 10))
  
})

output$product_recommendations <- renderDataTable({
  
  datatable(users_to_recommend()$recommendations,
            rownames = TRUE,
            escape = FALSE,
            selection = "none",
            options = list(autoWidth = FALSE,
                           align = 'center',
                           sDom = '<"top">rt<"bottom">ip',
                           scrollX = TRUE,
                           info = FALSE,
                           paging = TRUE,
                           ordering=FALSE,
                           pageLength = 10))
  
})