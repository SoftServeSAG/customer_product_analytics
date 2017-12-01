ecdf_linear<-function (x)
 {
  
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    return(NA)
  vals <- sort(unique(x))
  if (length(vals)>1){
    rval <- approxfun(vals, cumsum(tabulate(match(x,vals)))/n,  
                      method = "linear", yleft = 0, yright = 1, f = 0,ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
  }else{
    NA
  }

 
 }




# Week day dependencies
wDayProb <- function (data)
{
  res=as.data.frame(prop.table(table(data$week_day_name)))
  names(res)=c("WDay", "Freq")
  return(res)
}

# Last visit

LastVisit <- function (data)
{
  unique(data.frame(week_day=data$week_day[data$DAY==max(data$DAY)],
                    week_day_name=data$week_day_name[data$DAY==max(data$DAY)], day=max(data$DAY)))
}



# Time Lag between last and last + N visit
diffN <- function (data, tslot=c("DAY", "WEEK_NO")[1], lag=1)
{
  res=as.data.frame(table(data[[tslot]]))
  names(res)=c(tslot, "Freq")
  diff(as.numeric(as.character(as.numeric(as.character(res[[tslot]])))), lag)
}

diffN.Stat <- function (x)
{
  m=mean(x)
  m1=m-sd(x)/sqrt(length(x))*1.65
  m2=m+sd(x)/sqrt(length(x))*1.65
  data.frame(low=(m1), avg = (m), 
             high=(m2), 
             sd=sd(x),
             ex = kurtosis(x),
             sk=skewness(x))
}

diffN.Gamma <- function (x)
{
  options(warn=-1)
  if (length(x)>=1)
  {
    r1=sd(x)^2
    r2=mean(x)
    k2=r2/r1
    k1=r2*k2
    ks=ks.test(x, "pgamma", k1,rate=k2)
    res=data.frame(pvalue= ks$p.value, shape=k1, scale=k2)
    
  }
  options(warn=0)
  return(res)
  
}

diffN.LateExp <- function (x)
{
  options(warn=-1)
  if (length(x)>=1)
  {
    m=mean(x)
    m1=m-sd(x)/sqrt(length(x))*1.65
    m2=m+sd(x)/sqrt(length(x))*1.65
    s=sd(x)
    x=x-m
    x=x[x>=0]
    l=1/mean(x)
    ks=ks.test(x, "pexp", l)
    res=data.frame(low=(m1), avg = (m), 
               high=(m2), 
               sd=s,
               pvalue = ks$p.value,
               n=length(x),
               exp=l)
    
  }
  options(warn=0)
  return(res)
}


diffN.ECDF <- function (x)
{
  ecdf_linear(x)
}

diffN.PFirst <- function (x)
{
 a=1
  return(list(mean(x)+3*sd(x), sd(x)))
  
}

# User Processing

user_proc <- function(user_dat, tslot=c("DAY", "WEEK_NO")[1], lag=1)
{
  r=diffN(user_dat, tslot, lag)
  if (lag>1)
  {
    ef_lag1=list()
    for (k in ((lag-1):1))
    {
      rr=diffN(user_dat, tslot, k)
      ef_lag1[[k]]=diffN.ECDF(rr) 
    }
   
  }else{
    ef_lag1=NA
  }
  
  if (length(r)>1)
  {
    
    
    diffN.Stat(r)
    diffN.ECDF(r)
    diffN.Gamma(r)
    diffN.LateExp(r)
    
    d_tslot = list(last = LastVisit(user_dat),
                   wday = wDayProb(user_dat),
                   tslot=tslot,
                   lag=lag,
                   diffN=r, 
                   stats=diffN.Stat(r),
                   gamma_fit=diffN.Gamma(r),
                   late_fit = diffN.Gamma(r),
                   ecdf = diffN.ECDF(r),
                   ecdfprev = ef_lag1,
                   pfirst = diffN.PFirst(diffN(user_dat, tslot, 1)))
    
    return(d_tslot)
  }else
    NA
   
  
}


# Probability for period

period_prob <- function(x, low=7, high=14, wdays, del=0)
{
  a=1
  pfirst=(1/exp(max(del-x$pfirst[[1]],0)))^(1/(x$pfirst[[2]]))
  if (!(is.na(x$gamma_fit$pvalue))){
    if (x$gamma_fit$pvalue>=0.05)
    {
      prob=pfirst*(pgamma(max(high+1, min(x$diffN)), 
                  x$gamma_fit$shape, x$gamma_fit$scale) - pgamma(max(low, min(x$diffN)), 
                                                                 x$gamma_fit$shape, x$gamma_fit$scale))
      a2=pgamma(c(range(x$diffN)[1]:range(x$diffN)[2]+(high+1-low)), x$gamma_fit$shape, x$gamma_fit$scale)
      a1=pgamma(c(range(x$diffN)[1]:range(x$diffN)[2]), x$gamma_fit$shape, x$gamma_fit$scale)
      scK=max(a2-a1)
    }else{
      prob=pfirst*(x$ecdf(high+1)-x$ecdf(low))
      a2=x$ecdf(c(range(x$diffN)[1]:range(x$diffN)[2]+(high+1-low)))
      a1=x$ecdf(c(range(x$diffN)[1]:range(x$diffN)[2]))
      scK=max(a2-a1)

    }
    
   
    p_prev=1
    if(!is.na(x$ecdfprev)){
      p_prev=(tanh(12/(length(x$ecdfprev)+1)/tanh(12))^3)
      for (k in (1: length(x$ecdfprev)))
        p_prev=p_prev*x$ecdfprev[[k]](low)
    }
      
  }else{
    prob=NA
  }
  
  if (x$tslot=="WEEK_NO")
    mlp=7
  else
    mlp=1
  
  if(!is.na(prob)){
    i1=which(wdays==x$last$week_day_name)[1]
    i2=i1+low*mlp
    i3=i1+high*mlp
    
    d=data.frame(WDay=wdays[i2:i3], c=c(1:(i3-i2+1)))
    d=merge(x$wday,d, by="WDay")
    d=d[order(d$c),]
    d$prob=prob
    d$probwd=d$Freq*prob
    d$confwd=d$Freq/max(d$Freq)*prob
    d$conf=d$Freq/max(d$Freq)*prob/scK
    probs=data.frame(wdays=d$WDay, days=c((x$last$day+low*mlp):(x$last$day+high*mlp)), 
                     prob=d$prob, probwd=d$probwd, confwd=d$confwd, probs=d$conf, sb_total_pr = p_prev*d$prob)
  }else
    probs=NA
 
 return(list(user=x, range= c(low, high), prob=prob, probs=probs))
   
}


user_prob.lastlag <- function(user_dat, 
                              tslot=c("DAY", "WEEK_NO")[1], lag=1, 
                              range=c(7, 14), wdays)
{
  r=user_proc(user_dat, tslot, lag)

  low=range[1]
  high=range[2]
  
  res=period_prob(r, low, high, wdays)
  
  return(res)
}



user_prob.fixPeriod <- function(user_dat, 
                                lag=1, 
                                period=c(720, 727), wdays, fixed_date = NULL)
{
  
  
  tslot="DAY"
  del=0
  r=user_proc(user_dat, tslot, lag)
  
  if (is.na(r))
    return()
  
  if (!is.null(fixed_date))
  {
    
    del=fixed_date-r$last$day
    r$last$week_day_name=as.character(wdays[which(wdays==r$last$week_day_name)[1]+del])
    r$last$day=fixed_date
  }
  
  low=max(0,period[1]-r$last$day)
  high=max(1, period[2]-r$last$day)
  
  res=period_prob(r, low, high, wdays, del=del)
  
  return(res)
}



user_prob.fixPeriod.lags <- function(user_dat, 
                                    lags=c(1), 
                                    period=c(721, 742), 
                                    firstval=NULL,
                                    split=c("DAY", "WEEK")[1],
                                    k=7,
                                    wdays, fixed_date=NULL)
{
  if (is.null(firstval))
  {
    period[1]=max(period[1], max(user_dat$DAY)+1)
    period[2]=max(period[2], max(user_dat$DAY)+1)
  }else{
    period[1]=max(period[1], firstval)
    period[2]=max(period[2], firstval)
  }
  
  if(diff(period)<=0)
    return()
  
  if (split=="WEEK")
  {
    sp=k*7
  }else{
    sp=k
  }
  
  sep=c(1:diff(period)-1) %/% sp
  
  period=c(period[1]:period[2])
  
  probs=NULL
  for (k in sort(unique(sep)))
  {
    for (lag in lags){
      res=user_prob.fixPeriod(user_dat = user_dat, 
                              lag = lag, 
                              period=c(min(period[which(sep==k)]), max(period[which(sep==k)])), wdays = wdays, fixed_date)
      pr=res$probs
      if (!is.null(pr))
      {
        pr$lag=lag
        pr$iter=k
        if (!is.na(pr))
        {
          probs=rbind(probs, pr)
        }
      }
      
      
    }
    
  }
  
  probs$visit=as.factor(probs$lag)
  probs$days=as.factor(probs$days)

  
  return(probs)
}


brand_rate <- function(user, br_total)
{
  user_prod=split(user, as.character(user$DEPARTMENT))
  brand_user=lapply(user_prod, function(x){t=prop.table(table(x$BRAND))
  t/br_total})
  brand_user_total = prop.table(table(user$BRAND))/br_total
  
  return (brand_user_total)
}



price_rate <- function(user, prices_ls)
{
  
  user_prod=split(user, as.character(user$DEPARTMENT))
  prices_rates=lapply(user_prod, function(x){pr=x$SALES_VALUE[x$QUANTITY>0]/x$QUANTITY[x$QUANTITY>0]
                                             pr=quantile(pr, 0.75, na.rm = T)
                                             pr1=prices_ls[[as.character(x$DEPARTMENT[1])]]
                                             pr/pr1
                                            })
  
  return(prices_rates)
}

price_cmd_rate <- function(user, dep, prices_ls1)
{
  prices_c=prices_ls1[[dep]]
  user=user[user$DEPARTMENT==dep,]
  user_prod=split(user, as.character(user$COMMODITY_DESC))
  prices_rates=lapply(user_prod, function(x){pr=x$SALES_VALUE[x$QUANTITY>0]/x$QUANTITY[x$QUANTITY>0]
                                             pr=quantile(pr, 0.75, na.rm = T)
                                             pr1=prices_c[[as.character(x$COMMODITY_DESC[1])]]
                                             pr/pr1
                                          })
  
  return(prices_rates)
}

price_dep_policy <- function(user, prices_ls)
{
  res=price_rate(user, prices_ls = prices_ls)
  res=do.call("rbind", res)
  res=as.data.frame((res))
  res$DEPARTMENT=rownames(res)
  names(res)=c("rate", "DEPARTMENT")
  res=res[,c("DEPARTMENT", "rate")]
}


price_com_policy <- function(user, dep, prices_ls1)
{
  res=price_cmd_rate(user, dep=dep, prices_ls1 = prices_ls1)
  res=do.call("rbind", res)
  res=as.data.frame((res))
  res$COMMODITY_DESC=rownames(res)
  names(res)=c("rate", "COMMODITY_DESC")
  res=res[,c("COMMODITY_DESC", "rate")]
}



manufactorer_rate <- function(user, manufactorer_ls)
{
  
  user_prod=split(user, as.character(user$DEPARTMENT))
  dep_man_user=lapply(user_prod, function(x){data.frame(DEPARTMENT=x$DEPARTMENT, MANUFACTURER=as.character(x$MANUFACTURER))})
  
  dep_man_user=lapply(dep_man_user, function(x){list(tbl=prop.table(table(as.character(x$MANUFACTURER))), dep=as.character(x$DEPARTMENT)[1])})
  
  dep_man_user=lapply(dep_man_user, function(x){x$tbl=x$tbl[order(names(x$tbl))]
                                                  tbl1=manufactorer_ls[[as.character(x$dep)]]
                                                  tbl1=tbl1[order(names(tbl1))]
                                                  nms=intersect(names(tbl1), names(x$tbl))
                                                  (x$tbl[names(x$tbl) %in% nms]/tbl1[names(tbl1) %in% nms])^0.2})
                                                  
  return(dep_man_user)
  
}



period=c(707, 797)


lags=c(1:5)
#user=df_trans_demogr_product.list$`1`
user=data_full_dt[household_key==1]



users_analysis_t <- function( dataset,
                              lags=c(1:3),
                              period=c(707, 725),
                              timeslot=7, 
                              wdays = wdays,
                              all_manuf_rates,
                              all_brand_rates,
                              all_prices_rates,
                              fixed_date=NULL)
{
  counter=0
  
  users_id = sort(unique(dataset$household_key))
  users_nm=names(all_manuf_rates)
  users_idch=as.character(users_id)
  
  users_ind=which(users_nm %in% users_idch)
  
  lst=split(dataset, users_id)
  
  users = lapply(lst, function(x){ 
    
    counter<<-counter+1
    print(counter)

    probs = user_prob.fixPeriod.lags ( x,lags=lags,period=period,
                                       split="DAY",k=timeslot, wdays = wdays, fixed_date=fixed_date)
    
    if (!is.null(nrow(probs))){
      res=aggregate(sb_total_pr~iter, data=probs, sum)
      res$sb_total_pr=res$sb_total_pr/timeslot
      res$sb_total_pr = apply(matrix(res$sb_total_pr, ncol=1),1,function(x){min(x,1)})
      
      names(res)=c("iter", "total_prop")
      probs=merge(probs,res, by="iter")
    }else
      NA
    
    
    
    })
  
  
  
  
  manuf_rate_sel=all_manuf_rates[users_ind]
  brand_rate_sel=all_brand_rates[users_ind]
  price_rate_sel=all_prices_rates[users_ind]
  
  
  return(list(users = users, manuf_rate_sel=manuf_rate_sel,
              brand_rate_sel=brand_rate_sel, price_rate_sel=price_rate_sel))
  
  
  
  
}

users_analysis_dep <- function(dataset,
                             lags=c(1:3),
                             period=c(707, 725),
                             timeslot=7, 
                             wdays = wdays,
                             all_manuf_rates,
                             all_brand_rates,
                             all_prices_rates,
                             dep_name = NULL,
                             fixed_date=NULL)
{
  counter=0
  
  users_id = sort(unique(dataset$household_key))
  users_nm=names(all_manuf_rates)
  users_idch=as.character(users_id)
  
  users_ind=which(users_nm %in% users_idch)
  lst=split(dataset, users_id)
  
  users = lapply(lst, function(x){ 
    
    counter<<-counter+1
    print(counter)
    
    if (!is.null(dep_name))
    {
      x=x[x$DEPARTMENT == dep_name,]
    }
    
    if(nrow(x)==0)
      return(NA)
    
    probs = user_prob.fixPeriod.lags ( x,lags=lags,period=period,
                                       split="DAY",k=timeslot, wdays = wdays, fixed_date=fixed_date)
    
    if (!is.null(nrow(probs))){
      res=aggregate(sb_total_pr~iter, data=probs, sum)
      res$sb_total_pr=res$sb_total_pr/timeslot
      res$sb_total_pr = apply(matrix(res$sb_total_pr, ncol=1),1,function(x){min(x,1)})
      
      names(res)=c("iter", "total_prop")
      probs=merge(probs,res, by="iter")
    }else
      NA
    
    
    
  })
  
  
  
  
  manuf_rate_sel=all_manuf_rates[users_ind]
  brand_rate_sel=all_brand_rates[users_ind]
  price_rate_sel=all_prices_rates[users_ind]
  
  
  return(list(users = users, manuf_rate_sel=manuf_rate_sel,
              brand_rate_sel=brand_rate_sel, price_rate_sel=price_rate_sel))
  
  
  
  
}


users_analysis <- function(dataset,
                              lags=c(1:3),
                              period=c(707, 725),
                              timeslot=7, 
                              wdays = wdays,
                              all_manuf_rates,
                              all_brand_rates,
                              all_prices_rates)
{
  
  users_id = sort(unique(dataset$household_key))
  users_nm=names(all_manuf_rates)
  users_idch=as.character(users_id)
  
  users_ind=which(users_nm %in% users_idch)
  lst=split(dataset, users_id)
  
  
  users = lapply(lst, function(x){ 
    user_prob.fixPeriod.lags (x,lags,period,
                              split="DAY",
                              k=timeslot, wdays = wdays)})
  
  
  
  
  manuf_rate_sel=all_manuf_rates[users_ind]
  brand_rate_sel=all_brand_rates[users_ind]
  price_rate_sel=all_prices_rates[users_ind]
  
  
  return(list(users = users, manuf_rate_sel=manuf_rate_sel,
              brand_rate_sel=brand_rate_sel, price_rate_sel=price_rate_sel))
  
  
  
  
}


user_plot <- function (users, ind, dep_name=NULL, prob_n=c("prob","probwd","confwd","probs","sb_total_pr","total_prob")[4], maxlag=5, maxiter=5)
{
  
  
  if (prob_n!="total_prop")
  {
    dpf=users$users[[ind]]
    dpf=dpf[dpf$iter<=maxiter & dpf$lag <=maxlag,]
    p1<-
      ggplot(dpf, aes(x=days, y=dpf[[prob_n]], group=visit, fill=visit))+
      geom_bar(stat="identity", position = "dodge", color="black")+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      labs(y = prob_n)+
      ggtitle("Next Visit")+
      facet_grid(iter~.)
    
  }else
  {
    dg=users$users[[ind]][users$users[[ind]]$visit==min(as.numeric(users$users[[ind]]$visit)),]
    p1<-
      ggplot(dg, aes(x=days, y=dg[[prob_n]], fill=as.factor(iter)))+
      geom_bar(stat="identity", position = "dodge", color="black", show.legend = F)+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      ggtitle("Next Visit")+
      labs(y = prob_n)+
      facet_grid(iter~.)
  }
  
  
  df=data.frame(rate=as.numeric(users$brand_rate_sel[[ind]]), brand=names(users$brand_rate_sel[[1]]))
  p2<-
    ggplot(df, aes(x=brand, y=rate, fill=brand))+
    geom_bar(stat="identity", position = "dodge", color="black")+
    ggtitle("Brand Rate")
  
  
  df=unlist(users$price_rate_sel[[ind]])
  df=data.frame(price_rate=as.numeric(df), department=as.character(names(df)))
  df$department=unlist(strsplit(as.character(df$department),'.75%'))
  
  p3<-
    ggplot(df, aes(x=department, y=price_rate, fill=department))+
    geom_bar(stat="identity", position = "dodge", color="black")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    ggtitle("Price Policy")
  
  if (is.null(dep_name))
  {
    dep_name=names(users$manuf_rate_sel[[ind]])[which.max(unlist(lapply(users$manuf_rate_sel[[ind]], length)))]
  }
  
  df=users$manuf_rate_sel[[ind]][[dep_name]]
  df=data.frame(rate=as.numeric(df), manufacturer=as.character(names(df)))
  
  p4<-
    ggplot(df, aes(x=manufacturer, y=rate, fill=manufacturer))+
    geom_bar(stat="identity", position = "dodge", color="black", show.legend=FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste("Manufacturer Rate:",dep_name))
  
  return(list(p1,p2,p3,p4))

}


profile_plot <- function (users, ind, dep_name=NULL)
{
  
  
  df=data.frame(rate=as.numeric(users$brand_rate_sel[[ind]]), brand=names(users$brand_rate_sel[[1]]))
  p2<-
    ggplot(df, aes(x=brand, y=rate, fill=brand))+
    geom_bar(stat="identity", position = "dodge", color="black")+
    ggtitle("Brand Rate")
  
  
  df=unlist(users$price_rate_sel[[ind]])
  df=data.frame(price_rate=as.numeric(df), department=as.character(names(df)))
  df$department=unlist(strsplit(as.character(df$department),'.75%'))
  
  p3<-
    ggplot(df, aes(x=department, y=price_rate, fill=department))+
    geom_bar(stat="identity", position = "dodge", color="black")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    ggtitle("Price Policy")
  
  if (is.null(dep_name))
  {
    dep_name=names(users$manuf_rate_sel[[ind]])[which.max(unlist(lapply(users$manuf_rate_sel[[ind]], length)))]
  }
  
  df=users$manuf_rate_sel[[ind]][[dep_name]]
  df=data.frame(rate=as.numeric(df), manufacturer=as.character(names(df)))
  
  p4<-
    ggplot(df, aes(x=manufacturer, y=rate, fill=manufacturer))+
    geom_bar(stat="identity", position = "dodge", color="black", show.legend=FALSE)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste("Manufacturer Rate:",dep_name))
  
  return(list(p2,p3,p4))
  
}


visit_all_plot <- function (users, ind, maxlag=5, maxiter=5)
{
  
  prob_n=c("prob","probwd","confwd","probs","sb_total_pr","total_prob")
  pp=list()
  
  df=users$users[[ind]]
  df=df[df$iter<=maxiter & df$lag <=maxlag,]
  
  pl=ggplot(df, aes(x=days, y=prob, group=visit, fill=visit))+
        geom_bar(stat="identity", position = "dodge", color="black")+
        theme(axis.text.x = element_text(angle = 40, hjust = 1))+
        labs(y = "prob")+
        ggtitle("Next Visit")
  
  pp[[1]]<-pl
  
 
  pl=ggplot(df, aes(x=days, y=probwd, group=visit, fill=visit))+
        geom_bar(stat="identity", position = "dodge", color="black")+
        theme(axis.text.x = element_text(angle = 40, hjust = 1))+
        labs(y = "probwd")+
        ggtitle("Next Visit")
  
  pp[[2]]<-pl
  
  pl=ggplot(df, aes(x=days, y=confwd, group=visit, fill=visit))+
        geom_bar(stat="identity", position = "dodge", color="black")+
        theme(axis.text.x = element_text(angle = 40, hjust = 1))+
        labs(y = "confwd")+
        ggtitle("Next Visit")
  
  pp[[3]]<-pl
  
  pl=ggplot(df, aes(x=days, y=sb_total_pr, group=visit, fill=visit))+
        geom_bar(stat="identity", position = "dodge", color="black")+
        theme(axis.text.x = element_text(angle = 40, hjust = 1))+
        labs(y = "sb_total_pr")+
        ggtitle("Next Visit")
  
  pp[[5]]<-pl
  
  
  pl=ggplot(df, aes(x=days, y=probs, group=visit, fill=visit))+
        geom_bar(stat="identity", position = "dodge", color="black")+
        theme(axis.text.x = element_text(angle = 40, hjust = 1))+
        labs(y = "probs")+
        ggtitle("Next Visit")
    #    facet_grid(iter~.))
  
  pp[[4]]<-pl
  
  
  dg=users$users[[ind]][users$users[[ind]]$visit==min(as.numeric(users$users[[ind]]$visit)),]
  pp[[6]]<-
        ggplot(dg, aes(x=days, y=dg[['total_prop']], fill=as.factor(iter)))+
        geom_bar(stat="identity", position = "dodge", color="black", show.legend = F)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        ggtitle("Visit Confidence")+
        labs(y = "total_prop")
  
  pp[[7]]<-
    ggplot(dg, aes(x=days, y=dg[['total_prop']], group=1))+
    geom_line(stat="identity", position = "dodge", color="steelblue", show.legend = F, size=1.5)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Visit Confidence")+
    labs(y = "total_prop")
  
  return(pp)
  
}




user_visits_plot <- function (users, ind, maxlag=5, maxiter=5)
{
  
  
  pp=list()
  
  df=users$users[[ind]]
  df=df[df$lag <=maxlag & df$iter <=maxiter,]
  
  
  pl=ggplot(df, aes(x=days, y=confwd, group=visit, fill=visit))+
    geom_bar(stat="identity", position = "dodge", color="black")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    ggtitle("Next Visit")+
    facet_grid(iter~.)
  
  
  pp[[1]]<-pl
  
  dg=users$users[[ind]][users$users[[ind]]$visit==min(as.numeric(users$users[[ind]]$visit)),]
  pp[[2]]<-
    ggplot(dg, aes(x=days, y=dg[['total_prop']], fill=as.factor(iter)))+
    geom_bar(stat="identity", position = "dodge", color="black", show.legend = F)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Visit Confidence")+
    labs(y = "total_prop")
  
  pp[[3]]<-
    ggplot(dg, aes(x=days, y=dg[['total_prop']], group=1))+
    geom_line(stat="identity", position = "dodge", color="steelblue", show.legend = F, size=1.5)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Visit Confidence")+
    labs(y = "total_prop")
  
  return(pp)
  
}


users_total_visits <- function (users)
{
  a=1
  pp=lapply(users, function(x){if(!is.na(x)){
      unique(data.frame(days=x$days, total_prop=x$total_prop))}})
  pp=do.call("rbind",pp)
  pp$user=unlist(lapply(strsplit(rownames(pp), '[.]'), function(x){x[[1]][1]}))
  pp$days=as.numeric(as.character(pp$days))
  
  return(pp)
  
}



convert_index_to_date <- function(index_) {
  as.Date("2014/01/01", format="%Y/%m/%d") + index_ - 1
}

convert_index_to_features <- function(index_) {
  date=as.Date("2014/01/01", format="%Y/%m/%d") + index_ - 1
  month=as.numeric(format(date,"%m"))
  week_day=as.numeric(wday(date))
  year_day=as.numeric(yday(date))
  nweek=as.numeric(format(date,"%W"))
  month_day=as.numeric(format(date,"%d"))
  return(data.frame(DAY=index_, date=date, month=month, week_day=week_day, year_day=year_day, nweek=nweek, month_day=month_day))
}



fit_glm <- function(DF=df_CAT2.lst$`DELI::DELI MEATS`, maxd=711 )
{
  dweq=data.frame(DAY=c(min(DF$DAY):maxd), fl=1)
  DF=merge(DF, dweq, by="DAY", all.y = T)
  
  
  r= convert_index_to_features(DF$DAY)
  DF$date=r$date
  DF$month=r$month
  DF$week_day=r$week_day
  DF$year_day=r$year_day
  DF$nweek=r$nweek
  DF$month_day=r$month_day
  DF$cnt=1
  DF$cnt[is.na(DF$WEEK_NO)]=0
  DF$fl[is.na(DF$WEEK_NO)]=0
  
  DR_B=aggregate(cnt ~ month+week_day+month_day+nweek, data = DF, sum)
  
  
  DR_B$fl=0
  DR_B$fl[DR_B$cnt>0]=((log(DR_B$cnt[DR_B$cnt>0])/max(log(DR_B$cnt[DR_B$cnt>0])))^0.8)
  #DR_B$fl[DR_B$cnt>0]=1
  
  
  p1<-ggplot(DR_B, aes(x=week_day, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  p2<-ggplot(DR_B, aes(x=month_day, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  
  p3<-ggplot(DR_B, aes(x=month, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  DR_B$n=c(1:nrow(DR_B))
  DR_B$f="observed"
  
  fit_cnt=glm(cnt ~ week_day+month_day+nweek+month , data=DR_B, family = gaussian(link = "identity"))
  #fit_cnt=glm(cnt ~ week_day+month_day+nweek, data=DR_B, family = gaussian(link = "identity", ))
  
  return(list(data=DR_B, charts=list(wday=p1, mday=p2, mns = p3), model=fit_cnt))
  
}


fit_prob_glm <- function(DR_B )
{

  p1<-ggplot(DR_B, aes(x=week_day, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
  p2<-ggplot(DR_B, aes(x=month_day, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
  
  p3<-ggplot(DR_B, aes(x=month, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
 
  fit_cnt=glm(fl ~ month+week_day+month_day+nweek, data=DR_B, family = gaussian(link = "identity"))
  
  return(list(data=DR_B, charts=list(wday=p1, mday=p2, mns = p3), model=fit_cnt))
  
}




predict_load<-function(fit_model, range=c(711:741))
{
  
  r = convert_index_to_features(range)
  g=predict(fit_model, r, type = "link", se.fit = TRUE)
  
  data.frame(n=range, cnt=g$fit, f="predicted")
  
}

predict_vis<-function(init_data, predict_data, minR=650)
{
  
  df=data.frame(n=init_data$DAY, cnt=init_data$cnt, f=init_data$f)
  df=rbind(df, predict_data)
  df$date=convert_index_to_date(df$n)
  df$count=round(df$cnt)
  df$count=apply(as.matrix(df$count),1, function(x){max(x,0)})
  pp<-ggplotly(ggplot(df[df$n>minR,], aes(x=date, y=count, color=f))+
                 geom_line(show.legend = F)+
                 scale_color_manual(values = c("steelblue", "indianred")))
  return(list(df,pp))
  
}



predict_vis_prob<-function(init_data, predict_data, minR=650)
{
  
  a=1
  df=data.frame(n=init_data$n, cnt=init_data$fl, f=init_data$f)
  df=rbind(df, predict_data)
  df$date=convert_index_to_date(df$n)
  df$prob=df$cnt
  df$prob[df$prob>1]=1
  df$prob[df$prob<0]=0
  pp<-ggplotly(ggplot(df[df$n>minR,], aes(x=date, y=prob, color=f))+
                 geom_line(show.legend = F)+
                 scale_color_manual(values = c("steelblue", "indianred")))
  return(list(df,pp))
  
}




fit_ssa <- function(DF=df_CAT2.lst$`DELI::DELI MEATS`, maxd=711, range=c(712:742) )
{
  dweq=data.frame(DAY=c(min(DF$DAY):maxd), fl=1)
  DF=merge(DF, dweq, by="DAY", all.y = T)
  
  
  r= convert_index_to_features(DF$DAY)
  DF$date=r$date
  DF$month=r$month
  DF$week_day=r$week_day
  DF$year_day=r$year_day
  DF$nweek=r$nweek
  DF$month_day=r$month_day
  DF$cnt=1
  DF$cnt[is.na(DF$WEEK_NO)]=0
  DF$fl[is.na(DF$WEEK_NO)]=0

  DF$QUANTITY[is.na(DF$WEEK_NO)]=0
  
  DR_B=aggregate(QUANTITY ~ month+week_day+month_day+nweek+DAY, data = DF, sum)
  names(DR_B)[names(DR_B)=='QUANTITY']="cnt"
  
  
  DR_B$fl=0
  DR_B$fl[DR_B$cnt>0]=((log(DR_B$cnt[DR_B$cnt>0])/max(log(DR_B$cnt[DR_B$cnt>0])))^0.8)
  #DR_B$fl[DR_B$cnt>0]=1
  
  
  p1<-ggplot(DR_B, aes(x=week_day, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  p2<-ggplot(DR_B, aes(x=month_day, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  
  p3<-ggplot(DR_B, aes(x=month, y=cnt))+
    geom_point()+
    geom_smooth(method="loess")
  
  DR_B$n=c(1:nrow(DR_B))
  DR_B$f="observed"
  
  
  S = ssa(DR_B$cnt)
  V=vforecast(S, groups = list(all = 1:50), len = max(range)-min(range)+1, only.new = TRUE)
  df=data.frame(n=range, cnt=V, f="predicted")

  
  return(list(data=DR_B, charts=list(wday=p1, mday=p2, mns = p3), forecast=df))
  
}


fit_prob_ssa <- function(DR_B, range=c(712:742))
{
  
  p1<-ggplot(DR_B, aes(x=week_day, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
  p2<-ggplot(DR_B, aes(x=month_day, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
  
  p3<-ggplot(DR_B, aes(x=month, y=fl))+
    geom_point()+
    geom_smooth(method="loess")
  
  
  S = ssa(DR_B$fl)
  V=vforecast(S, groups = list(all = 1:50), len = max(range)-min(range)+1, only.new = TRUE)
  df=data.frame(n=range, cnt=V, f="predicted")
  
  
  return(list(data=DR_B, charts=list(wday=p1, mday=p2, mns = p3), forecast=df))
  
}


predict_ssa <- function(DF=df_CAT2.lst[['DELI::CHICKEN/POULTRY']], maxd=711, len=30, minR=650 )
{
  range=c(maxd:(maxd+len))
  pload<-fit_ssa(DF = DF, maxd=maxd, range = range )
  pp1=predict_vis(pload$data, pload$forecast, minR=minR) #Load
  pprob=fit_prob_ssa (pload$data, range=range)
  pp2=predict_vis_prob(pprob$data, pprob$forecast, minR=minR) #Load
  pp1[[1]]=pp1[[1]][,c("date", "count", "f")]
  pp2[[1]]=pp2[[1]][,c("date", "prob", "f")]
  return(list(load=pp1, prob=pp2, profile=pload$charts))
  
}


predict_glm <- function(DF=df_CAT2.lst[['DELI::CHICKEN/POULTRY']], maxd=711, len=30, minR=650 )
{
  range=c(maxd:(maxd+len))
  pload<-fit_glm(DF=DF, maxd=711 )
  pred<-predict_load(pload$model,  range=range)
  pp1=predict_vis(pload$data, pred, minR=minR) #Load
  
  pprob<-fit_prob_glm(pload$data)
  pred_prob<-predict_load(pprob$model,  range=range)
  pp2=predict_vis_prob(pprob$data, pred_prob, minR=minR) #Probability
  
  pp1[[1]]=pp1[[1]][,c("date", "count", "f")]
  pp2[[1]]=pp2[[1]][,c("date", "prob", "f")]
  return(list(load=pp1, prob=pp2, profile=pload$charts))
  
}


fit_prophet <- function(DF,  maxd = 711, len = 30, minR = 650){
  
  window = maxd - minR
  start_mat <<- DF %>% mutate(ds = convert_index_to_date(DAY)) %>% 
    aggregate(QUANTITY~ds, ., sum)
  
  
  mat_zero <- data.frame(ds=seq(as.Date("2014/01/01"), as.Date("2015-12-12"), by="days"),
                         y=0)
  result <<- merge(mat_zero, start_mat, by='ds', all.x = T) %>% 
    mutate(y = ifelse(is.na(QUANTITY), y, y + QUANTITY)) %>% 
    select(ds, y)
  
  #train_start <<-result %>% 
  #  filter(ds > as.Date("2014/01/01", format="%Y/%m/%d") + minR)
  
  train_start <<- result[minR:maxd, ]
  
  if (sum(train_start$y) > 0){
    
    m <- prophet(train_start, yearly.seasonality=F)
    
    future <- make_future_dataframe(m, periods = len)
    forecast <- predict(m, future) %>% mutate(y_pred = lost_item(yhat) %>% round(0)) %>% 
      select(ds, y_pred) %>% 
      tail(len)
    
    
    res=result
    res$f="observed"
    names(res)=c("date", "count", "f")
    
    forecast$f="forecasted"
    names(forecast)=c("date", "count", "f")
    
    #res=rbind(res,forecast, make.row.names = F)
    
    
    return(list(data=res, forecast=forecast))
    
  } else{
    forecast <- data.frame(date=seq(as.Date("2015-12-12"), as.Date("2015-12-12") + 59, by="days"),
                           count=0)
    
    res=result
    res$f="observed"
    names(res)=c("date", "count", "f")
    
    forecast$f="forecasted"
    #names(forecast)=c("date", "count", "f")
    #res=rbind(res,forecast, make.row.names = F)
    
    
    return(list(data=res, forecast=forecast))
  }
  
  
}


predict_vis_prophet<-function(init_data, predict_data, minR=650)
{
  
  #df=data.frame(n=init_data$date, cnt=init_data$count, f=init_data$f)
  df=rbind(init_data, predict_data, make.row.names = F)
  #df$date=convert_index_to_date(df$n)
  #df$count=df$cnt
  pp<-ggplotly(ggplot(df[df$date > as.Date("2014/01/01", format="%Y/%m/%d") + minR,], aes(x=date, y=count, color=f))+
                 geom_line(show.legend = F)+
                 scale_color_manual(values = c("steelblue", "indianred")))
  return(list(df,pp))
  
}


predict_prophet<-function(DF,  maxd = 711, len = 30, minR = 650)
{
  
  range=c(maxd:(maxd+len))
  pload<<-fit_prophet(DF, maxd = maxd, len = len, minR = minR)
  pp1=predict_vis_prophet(pload$data, pload$forecast, minR=minR) #Load
  #pprob=fit_prob_ssa (pload$data, range=range)
  
  #pp1[[1]]=pp1[[1]][,c("date", "count", "f")]
  return(list(load=pp1))
  
  #res1=fit_prophet(DF, maxd = maxd, len = len, minR = minR)
  #res2=fit_prob_prophet(DF, maxd = maxd, len = len, minR = minR)
  #return(list(load=res1, prob=res2))
  #return(list(load=res1))
}

lost_item <- function(x){
  ifelse(x > 0, x, 0)
}



