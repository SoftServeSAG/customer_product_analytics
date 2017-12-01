#----------------------------------------
#Reactive expressions with data subsets
#----------------------------------------
#Subset of data for selected customer
customer_data <- reactive({
  data_full_dt[household_key == input$ca_user_id]
})

#Week-day mapping
week_first_week_day_mapping <- reactive({
  
  df <- data_full_dt[, list(first_day = min(DAY)), by = WEEK_NO][order(WEEK_NO)]
  df <- df[, first_day := convert_index_to_date(first_day)]
  
  df
})

#Recommendations slice
users_recommendations <- reactive({
  
  UPROD1[USER == input$ca_user_id]
  
})

#Transactional data for selected customer and department
transactional_customer_data <- reactive({
  
  req(!is.null(input$ca_departments_list))
  
  df <- customer_data()
  
  if (input$ca_departments_list != "All") {
    df <- df[DEPARTMENT == input$ca_departments_list]
  }
  
  df
})

#----------------------------------------
#Rendering of input elements
#----------------------------------------
#Detect list of departments for customer
output$ca_render_departments_list <- renderUI({
  
  #DEPARTMENTS with at least 5 visits in different days
  departments <- unique(customer_data()[, c("DAY", "DEPARTMENT"), with = FALSE])
  departments <- table(departments$DEPARTMENT)
  departments <- names(departments)[departments >= 5]
  
  selectInput(inputId = "ca_departments_list", 
              label = "Select product department", 
              choices = c("All", departments),
              selected = 1, multiple = FALSE)
  
})

#----------------------------------------
#Customer visit history
#----------------------------------------
#CALCULATIONS
first_user_visit <- reactive(
  convert_index_to_date(
    min(transactional_customer_data()$DAY)
  )
)

last_user_visit <- reactive(
  convert_index_to_date(
    max(transactional_customer_data()$DAY)
  )
)

#Calculating average number of visits per week
user_weeks_visits <- reactive({
  
  weeks_visits <- transactional_customer_data()
  
  weeks_visits <- weeks_visits[, list(n_baskets = length(unique(BASKET_ID))), by = WEEK_NO][order(WEEK_NO)]
  avg_per_week <- round(sum(weeks_visits$n_baskets) / (102 - min(weeks_visits$WEEK_NO)), 2)
  
  #Set WEEK_NO as factor for visualization
  weeks_visits$WEEK_NO <- factor(weeks_visits$WEEK_NO, levels = seq(1, 102))
  
  weeks_visits <- copy(week_first_week_day_mapping())[weeks_visits, n_baskets := i.n_baskets, on = "WEEK_NO"]
  weeks_visits <- weeks_visits[is.na(n_baskets), n_baskets := 0]
  
  return(list(avg_per_week = avg_per_week, weeks_visits = weeks_visits))
})

#----------------------------------------
#VISUALIZATIONS
output$ca_box_first_user_visit <- renderValueBox({
  
  valueBox(
    first_user_visit(), "First purchase", icon = icon("calendar-check-o")
  )
  
})

output$ca_box_last_user_visit <- renderValueBox({
  
  valueBox(
    last_user_visit(), "Last purchase", icon = icon("calendar-check-o"),
    color = "orange"
  )
  
})

output$ca_box_average_number_visits_per_week <- renderValueBox({
  
  valueBox(
    user_weeks_visits()$avg_per_week, "Average number of visits per week for selected department", icon = icon("shopping-basket"),
    color = "purple"
  )
  
})

output$ca_plot_weeks_visits <- renderPlotly(
  
  plot_ly(user_weeks_visits()$weeks_visits, x = ~first_day, y = ~n_baskets, type = 'bar',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5))) %>%
    layout(yaxis = list(title = ""), xaxis = list(title = "Week"))
  
)

#----------------------------------------
#Customer profile
#----------------------------------------
#CALCULATIONS
product_profile <- reactive({

  req(!is.null(input$ca_departments_list))
  
  if (input$ca_departments_list == "All") {
    var <- "DEPARTMENT"
  } else {
    var <- "COMMODITY_DESC"
  }
  
  baskets_aggregation <- transactional_customer_data()[, c("BASKET_ID", var), with = FALSE]
  baskets_aggregation <- baskets_aggregation[, list(n_baskets = length(unique(BASKET_ID))), by = var]
  baskets_aggregation <- baskets_aggregation[order(-n_baskets)]
  
  names(baskets_aggregation)[1] <- "Variable"
  
  return(head(baskets_aggregation, 5))
})

price_profile <- reactive({
  
  req(!is.null(input$ca_departments_list))
  
  USER=data.frame(customer_data())
  
  if (input$ca_departments_list == "All") {
    res=price_dep_policy(USER, prices_ls = price_dep)
  } else {
    res=price_com_policy(USER, dep=input$ca_departments_list, prices_ls = price_commodity)
  }
  
  names(res) <- c("Product", "Price_Rate")
  
  res$Price_Range <- ifelse(res$Price_Rate >= 1, "Preferes more expensive products", "Preferes cheaper products")
  res$line <- 1
  
  return(res)
})

#----------------------------------------
#VISUALIZATION
output$products_profile <- renderPlotly({
  
  plot_ly(data = product_profile(),
          labels = ~Variable,
          values = ~n_baskets, 
          type = "pie", 
          textposition = 'inside',
          textinfo = 'label',
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(line = list(color = '#FFFFFF', width = 1), 
                        colors = c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')),
          showlegend = FALSE) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
})

output$prices_profile <- renderPlotly({
  
  plot_ly(data = price_profile(), 
          x = ~Product, 
          y = ~Price_Rate, 
          color = ~Price_Range,
          type = "bar",
          marker = list(line = list(width = 2))) %>% layout(margin = list(b = 100), yaxis = list(title = "Price Rate"), xaxis = list(title = "")#,
                                                            #shapes=list(type='line', y0 = 1, x0 = -1, y1 = 1, x1 = (nrow(price_profile()) - 1), line = list(color = "grey"))
          )
  
})

#----------------------------------------
#Probabilities of next 3 visits + prediction of number of items bought
#----------------------------------------
#CALCULATIONS

#----------------------------------------
#Predicting next visits for customer
user_analysis <- eventReactive(input$ca_analyse_customer, {
  
  withProgress(message = "", value = 0, style="old", {
    
    if (input$ca_departments_list == "All") {
      dep <- NULL
    } else {
      dep <- input$ca_departments_list
    }
    
    df <- users_analysis_dep(data.frame(customer_data()),
                             lags=c(1:3),
                             period=c(convert_date_to_index(input$ca_dates_to_predict[1]), convert_date_to_index(input$ca_dates_to_predict[2]) + 1),
                             timeslot=7, 
                             wdays = wdays,
                             all_manuf_rates,
                             all_brand_rates,
                             all_prices_rates,
                             dep, 
                             fixed_date = 713)
    
    #Adding 'date' columns for visualization 
    df$users[[1]]$date <- convert_index_to_date(as.integer(as.character(df$users[[1]]$days)))
    df$users[[1]]$Week <- format(df$users[[1]]$date, "%Y - %W")
    df$users[[1]]$Week[df$users[[1]]$Week == "2016 - 00"] <- "2015 - 52"
    
  })
  
  df
})

#Predicting demand of customer
# future_customer_demand <- eventReactive(input$ca_analyse_customer, {
#   
#   withProgress(message = "", value = 0, style="old", {
#     res=predict_prophet(DF = data.frame(transactional_customer_data()), maxd = 713, len = 30, minR = 650)
#     
#     dt <- res$load[[1]]
#     dt <- dt[dt$f == "Predicted", ]
#     dt <- dt[, c("date", "count")]
#     dt$count_lower <- res$forecast_all[,'count_lower'] %>% round(0) %>% lost_item()
#     dt$count_upper <- res$forecast_all[,'count_upper'] %>% round(0)
#   })
#   
#   return(list(predictions = dt, chart = res$load[[2]]))
#   
# })

#----------------------------------------
#VISUALIZATION
#Plot predicted values
output$next_visit_plot <- renderPlot({
  
  ggplot(user_analysis()$users[[1]],
         aes(x=wdays,
             y=probwd * 100, group=visit, fill=visit))+
    geom_bar(stat="identity", position = "dodge", color="black") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    facet_grid(.~Week) +
    labs(x = "Weekday", y = "Probability, %")
  
})

#Show table with probabilities
output$next_visits_table <- renderDataTable({
  
  df <- user_analysis()$users[[1]]
  df$probwd <- round(df$probwd * 100, 2)
  df <- reshape(data = df[, c("date", "wdays", "probwd", "visit")], idvar = c("wdays", "date"), timevar = "visit", direction = "wide")
  
  DT::datatable(df, 
                rownames = FALSE, 
                colnames = c("Date", "Weekday", "Visit 1, %", "Visit 2, %", "Visit 3, %"),
                escape = FALSE,
                selection = "none",
                options = list(autoWidth = FALSE,
                               align = 'center',
                               sDom = '<"top">rt<"bottom">ip',
                               scrollX = TRUE,
                               info = FALSE,
                               paging = TRUE,
                               ordering=FALSE,
                               pageLength = 10)
  )
  
})


# #Plot predicted demand
# output$customer_demand_predictions_chart <- renderPlotly({
#   
#   future_customer_demand()$chart
#   
# })
# 
# #Demand prediction data
# output$customer_demand_predictions_data <- renderDataTable({
#   
#   datatable(future_customer_demand()$predictions,
#             rownames = FALSE,
#             colnames = c("Date", "Count", "Lower bound", "Upper bound"),
#             escape = FALSE,
#             selection = "none",
#             options = list(autoWidth = FALSE,
#                            align = 'center',
#                            sDom = '<"top">rt<"bottom">ip',
#                            scrollX = TRUE,
#                            info = FALSE,
#                            paging = TRUE,
#                            ordering=FALSE,
#                            pageLength = 10))
#   
# })

#----------------------------------------
#Recommendations for customer
#----------------------------------------
#CALCULATIONS
pref_rec_tables <- eventReactive(input$ca_analyse_customer, {
  
  withProgress(message = "", value = 0, style="old", {
    
    user_id <- as.character(input$ca_user_id)
    
    DF=users_recommendations()
    DF$DESC="REC"
    DF$DESC[DF$FLAG==0]="PREF"
    DF=DF[order(DF$RATE, decreasing = T),]
    
    
    brand=users_12$brand_rate_sel[[user_id]]
    
    
    DB=as.data.frame(brand)
    names(DB)=c("BRAND", "BRate")
    DF=merge(DF,DB,by="BRAND")
    
    manufacturer=users_12$manuf_rate_sel[[user_id]]
    
    manufacturer<-
      lapply(manufacturer, function(x){
        if (length(x)==1){
          data.frame(MANUFACTURER=names(x), MRate=x)
        }else{
          y=as.data.frame(x)
          names(y)=c("MANUFACTURER", "MRate")
          y
        }
      })
    
    DF_l=split(DF, DF$DEPARTMENT)
    
    for (dname in (names(DF_l)))
    {
      manufacturer[[dname]]$MANUFACTURER=as.numeric(as.character(manufacturer[[dname]]$MANUFACTURER))
      DF_l[[dname]]=merge(DF_l[[dname]], manufacturer[[dname]], by=c("MANUFACTURER"), all.x = T)
    }
    
    DF=rbindlist(DF_l, use.names = TRUE, fill = TRUE)   
    DF$MRate[is.na(DF$MRate)]=min(DF$MRate, na.rm = T)*0.7
    
    
    DF$RATE[DF$DESC=="REC"]=DF$RATE[DF$DESC=="REC"]*DF$BRate[DF$DESC=="REC"]*DF$MRate[DF$DESC=="REC"]
    
    
    DF$RATE[DF$DESC=="REC"]=DF$RATE[DF$DESC=="REC"]/max(DF$RATE[DF$DESC=="REC"])
    DF$RATE[DF$DESC=="REC"]=(DF$RATE[DF$DESC=="REC"]^0.7)
    
    DF=DF[order(DF$RATE, decreasing = T),]
    
    #Filtering by department
    if (input$ca_departments_list != "All") {
      
      DF <- DF[DF$DEPARTMENT == input$ca_departments_list, ]
      
    }
    
    #Increase preferences rates
    DF$RATE[DF$DESC=="PREF"]=(DF$RATE[DF$DESC=="PREF"]^0.5)
    DF$RATE=round(DF$RATE,2)
    
    preferences <- DF[DF$DESC=="PREF",
                      c("DEPARTMENT", "COMMODITY_DESC", "SUB_COMMODITY_DESC", "PRODUCT_ID", "RATE")]
    
    recommendations <- DF[DF$DESC=="REC" & DF$RATE>0.6,
                          c("DEPARTMENT", "COMMODITY_DESC", "SUB_COMMODITY_DESC", "PRODUCT_ID", "RATE")]
    
  })
  
  return(list(preferences = preferences, recommendations = recommendations))
})

#----------------------------------------
#VISUALIZATION
output$customer_preferences <- renderDataTable({
  
  datatable(pref_rec_tables()$preferences,
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

output$customer_recommendations <- renderDataTable({
  
  datatable(pref_rec_tables()$recommendations,
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