
###The file contains functions for outputting the forecast of the quantity of products that will be bought via Prophet
###https://research.fb.com/prophet-forecasting-at-scale/

# library(prophet)
# library(dygraphs)
# library(xts)

fit_prophet <- function(DF,  maxd = 711, len = 30, minR = 650){
###Predict forecast of the quantity of products via Prophet 
#INPUT: maxd - maximum number of days for which there is data
#       len - number of days for forecast
#       minR - day from which we use the data for the forecast
###
#OUTPUT: list(data=res, forecast=forecast, forecast_all=forecast_all)
#       res - data for historical days
#       forecast - predict for LEN days, columns: date, count, f
#       forecast_all - predict for LEN days, columns: date, count, count_lower, count_upper, f
  
   
    start_mat <- DF %>% mutate(ds = convert_index_to_date(DAY)) %>% 
        aggregate(QUANTITY~ds, ., sum)
    
    
    mat_zero <- data.frame(ds=seq(as.Date("2014/03/19"), maxd -1 + as.Date("2014/03/19"), by="days"),
                           y=0)
    #result - data for historical days, if the products did not buy, then count = 0
    result <- merge(mat_zero, start_mat, by='ds', all.x = T) %>% 
        mutate(y = ifelse(is.na(QUANTITY), y, y + QUANTITY)) %>% 
        select(ds, y)
    
    #train_start - data for predict
    train_start <- result[minR:maxd, ]
    
    #
    if (sum(train_start$y, na.rm = TRUE) > 0){
        print(holiday_days)
        m <- prophet(train_start, yearly.seasonality=TRUE, weekly.seasonality = TRUE, holidays = holiday_days, seasonality.prior.scale = 30)
        
        future <- make_future_dataframe(m, periods = len)
        forecast_all <- predict(m, future) %>% mutate(y_pred = lost_item(yhat) %>% round(0)) %>% 
            select(ds, y_pred, yhat_lower, yhat_upper) %>% 
            tail(len)
        
        forecast_all$f="Predicted"
        names(forecast_all)=c("date", "count", "count_lower", "count_upper", "f")
        
        res=result
        res$f="Observed"
        names(res)=c("date", "count", "f")
        
        
        forecast <- forecast_all %>% select(date, count, f)

        return(list(data=res, forecast=forecast, forecast_all=forecast_all))
        
    } else{
        forecast_all <- data.frame(date=seq(as.Date("2016-02-27"), as.Date("2016-02-27") + len - 1, by="days"),
                               count=0, count_lower=0, count_upper=0)
        forecast_all$f="Predicted"
        forecast <- forecast_all %>% select(date, count, f)
        
        res=result
        res$f="Observed"
        names(res)=c("date", "count", "f")

        return(list(data=res, forecast=forecast, forecast_all=forecast_all))
    }
    
    
}



predict_vis_prophet<-function(init_data, predict_data, forecast_all, maxd = maxd, len = len, minR = minR)
{
###Function output:
#   df - dataframe with historical and predict data
#   pp - plot for time series of historical and predict data 
  
    df=rbind(init_data, predict_data, make.row.names = F)
    
    #Create two time series for only historical data (predict = NA) - first
    #and for predict data (historical = NA) with confidance interval (count_low, count_up)  - second
    
    first <- df
    first$count_up <- NA
    first$count_low <- NA
    second <- first
    first[(maxd + 1):(maxd + len), ]$count <- NA
    
    
    second[1:maxd, ]$count <- NA
    second[(maxd + 1):(maxd + len), ]$count_up <- forecast_all$count_upper
    second[(maxd + 1):(maxd + len), ]$count_low <- forecast_all$count_lower %>% lost_item()
    
    #Cbind two time series in one dataframe for plot
    dt2 <- cbind(Observed = as.xts(first$count , order.by = first$date),
                 Predicted = as.xts(second$count, order.by = first$date),
                 low = as.xts(second$count_low, order.by = first$date),
                 up = as.xts(second$count_up, order.by = first$date))
    
    
    pp <- dygraph(dt2) %>%
      dySeries(c("low", "Predicted", "up"), label = "Predicted") %>%
      dySeries("Observed", label = "Observed") %>%
      dyRangeSelector(height = 20) %>% 
      dyRangeSelector(dateWindow = c(as.character(max(second$date) - len - 90), as.character(max(second$date)))) %>% 
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
    
    return(list(df,pp))
    
}


predict_prophet<-function(DF,  maxd = 711, len = 30, minR = 650)
{
###Function input dataframe with one category or user and output list:
#  load - list of dataframe with historical and predict data and plot for time series of historical and predict data 
#  forecast_all - predict data for LEN days, columns: date, count, count_lower, count_upper, f
  
    pload<-fit_prophet(DF, maxd = maxd, len = len, minR = minR)
    pp1=predict_vis_prophet(pload$data, pload$forecast, pload$forecast_all, maxd = maxd, len = len, minR = minR) 

    return(list(load=pp1, forecast_all=pload$forecast_all))
}

lost_item <- function(x){
###RELU function
    ifelse(x > 0, x, 0)
}


###For TEST

#test <- readRDS("test.rds")
# 
#res <- predict_prophet(test, maxd = 711, len = 300, minR = 650)
#res$load[[1]][700:800,]
#res$forecast_all
#res$load[[2]]

