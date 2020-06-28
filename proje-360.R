
#install the required packages first
require(jsonlite)
require(httr)
require(data.table)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("xts")
require(xts)
#install.packages("forecast")
require(forecast)
get_token <- function(username, password, url_site){
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  return(token)
}
get_data <- function(start_date='2020-03-20', token, url_site){
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}
send_submission <- function(predictions, token, url_site, submit_now=T){
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4}
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)
  }
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
    
  }
  print(content(result))
}
check_format <- function(predictions){
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
}
# this part is main code
subm_url = 'http://167.172.183.67'
u_name = "Group20"
p_word = "R0CF5zGSXc2WUEV7"
#submission
submit_now = FALSE
username = u_name
password = p_word
token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)
predictions=unique(data[,list(product_content_id)])
unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]
data[, visit_turnover_rate := data$sold_count/data$visit_count]
data[, basket_turnover_rate := data$sold_count/data$basket_count]
data[is.na(data)] <- 0
#data[data$sold_count==0]$sold_count=-1
product1 = data[data$product_content_id==85004]
product2 = data[data$product_content_id==4066298]
product3 = data[data$product_content_id==6676673]
product4 = data[data$product_content_id==7061886]
product5 = data[data$product_content_id==31515569]
product6 = data[data$product_content_id==32939029]
product7 = data[data$product_content_id==5926527]
product8 = data[data$product_content_id==3904356]
#
#
#
product1_ts = ts(product1$sold_count,start = min(product1$event_date))
ts.plot(product1_ts)
acf(product1_ts)
#choose lag=16
product1_ts = ts(product1$sold_count,start = min(product1$event_date),frequency = 16)
product1_ts = ts(product1$sold_count,start = min(product1$event_date),frequency = 16)
plot(product1_ts)
plot.ts(product1_ts)
product1_ts[190:220] = NA
product1_ts[190:220] = abs(arima.sim(model = list(ma = 3), mean = 9.090623, sd = sqrt(73.34555),n
                                     = 31))
plot.ts(product1_ts)
mean(product1_ts[0:190])
var(product1_ts[0:190])
product1_ts_train <- product1_ts[0:(length(product1_ts)-10)]
product1_ts_train <- ts(product1_ts_train, frequency = 16)
decomposed_p1_add<-decompose(product1_ts_train,type="additive")

plot(decomposed_p1_add)
decomposed_p1_mult<-decompose(product1_ts_train,type="multiplicative")
plot(decomposed_p1_mult)
#choose multipliactive
last_trend=decomposed_p1_mult$trend[max(which(!is.na(decomposed_p1_mult$trend)))]
last_trend
first_trend=decomposed_p1_mult$trend[min(which(!is.na(decomposed_p1_mult$trend)))]
first_trend
frequency_p1<-frequency(decomposed_p1_mult$trend)
decomposed_p1_mult$trend[1:(frequency_p1/2)]=first_trend
decomposed_p1_mult$trend[(length(decomposed_p1_mult$trend)-
                            (frequency_p1/2)+1):length(decomposed_p1_mult$trend)]=last_trend
head(decomposed_p1_mult$trend,20)
tail(decomposed_p1_mult$trend,20)
detrend_p1<- product1_ts_train / decomposed_p1_mult$trend
deseas_p1<-detrend_p1 / decomposed_p1_mult$seasonal
arima_model_p1<-auto.arima(deseas_p1,seasonal = F)
summary(arima_model_p1)
model_fitted_p1 <- deseas_p1 - residuals(arima_model_p1)
model_fitted_p1_seasonality_trend<-model_fitted_p1*
  (decomposed_p1_mult$seasonal*decomposed_p1_mult$trend)
ts.plot(deseas_p1, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_p1, type = "l", col = 2, lty = 2)
ts.plot(product1_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_p1_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p1_seasonality_trend - product1_ts_train)^2)) / length(product1_ts_train)
p1_forecast_arima<-predict(arima_model_p1,n.ahead=10)$pred
p1_forecast_arima
start_seasonality_p1 <- start(p1_forecast_arima)[2]
end_seasonality_p1 <- end(p1_forecast_arima)[2]
if(end_seasonality_p1 > start_seasonality_p1){
  p1_forecast_arima<-
    p1_forecast_arima*last_trend*decomposed_p1_mult$figure[start_seasonality_p1:end_seasonality_p1]
}
if(start_seasonality_p1 > end_seasonality_p1){
  counter <- c(start_seasonality_p1:16, 1:end_seasonality_p1)
  p1_forecast_arima<- p1_forecast_arima*last_trend*decomposed_p1_mult$figure[counter]
}
p1_forecast_arima
#arima_reg model
p1_visit_ts <- ts(product1$visit_turnover_rate, start= min(product1$event_date))
which.max(p1_visit_ts)
p1_visit_ts[which.max(p1_visit_ts)] = mean(p1_visit_ts[12:22])
plot(p1_visit_ts)
which.max(p1_visit_ts)
p1_visit_ts[which.max(p1_visit_ts)] = mean(p1_visit_ts[137:145])
plot(p1_visit_ts)
p1_price_ts <- ts(product1$price, start= min(product1$event_date))
ts.plot(p1_price_ts)
which(p1_price_ts == min(p1_price_ts))
plot(p1_price_ts)
p1_price_ts[which(p1_price_ts == min(p1_price_ts))] <- NA
p1_price_reg_matrix <- matrix(p1_price_ts[1:(length(p1_price_ts)-10)])
p1_visit_reg_matrix <- matrix(p1_visit_ts[1:(length(p1_visit_ts)-10)])
p1_reg_matrix <- cbind(p1_price_reg_matrix, p1_visit_reg_matrix)
colnames(p1_reg_matrix) <- c("price", "visit_turnover_rate")
p1_reg_model <- auto.arima(deseas_p1, xreg = p1_reg_matrix, seasonal = F)
summary(p1_reg_model)
model_fitted_reg_p1 <- deseas_p1 - residuals(p1_reg_model)
model_fitted_p1_reg_seasonality_trend<-model_fitted_reg_p1*
  (decomposed_p1_mult$seasonal*decomposed_p1_mult$trend)
ts.plot(deseas_p1, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_reg_p1, type = "l", col = 2, lty = 2)
ts.plot(product1_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_p1_reg_seasonality_trend, type = "l", col = 2, lty = 2)

sqrt(mean((model_fitted_p1_reg_seasonality_trend - product1_ts_train)^2)) / length(product1_ts_train)
p1_reg_matrix_test_1 <- rep(p1_reg_matrix[nrow(p1_reg_matrix),1],10)
p1_reg_matrix_test_2 <- rep(p1_reg_matrix[nrow(p1_reg_matrix),2],10)
p1_reg_matrix_test <- cbind(p1_reg_matrix_test_1, p1_reg_matrix_test_2)
colnames(p1_reg_matrix_test) <- c("price", "visit_turnover_rate")
p1_forecast_reg<-forecast(p1_reg_model,h=10,xreg=p1_reg_matrix_test)$mean
p1_forecast_reg
start_seasonality_reg_p1 <- start(p1_forecast_reg)[2]
end_seasonality_reg_p1 <- end(p1_forecast_reg)[2]
if(end_seasonality_reg_p1 > start_seasonality_reg_p1){
  p1_forecast_reg<-
    p1_forecast_reg*last_trend*decomposed_p1_mult$figure[start_seasonality_reg_p1:end_seasonality_reg_p1]
}
if(start_seasonality_reg_p1 > end_seasonality_reg_p1){
  counter_reg <- c(start_seasonality_reg_p1:16, 1:end_seasonality_reg_p1)
  p1_forecast_reg<- p1_forecast_reg*last_trend*decomposed_p1_mult$figure[counter_reg]
}
p1_forecast_reg
#etsmodel
p1_ets_model <- ets(deseas_p1)
autoplot(p1_ets_model)
summary(p1_ets_model)
residuals(p1_ets_model)
model_fitted_ets_p1 <- p1_ets_model$fitted
model_fitted_p1_ets_seasonality_trend<-model_fitted_ets_p1*
  (decomposed_p1_mult$seasonal*decomposed_p1_mult$trend)
accuracy(p1_ets_model)
p1_forecast_ets<-predict(p1_ets_model,h=10)$mean
p1_forecast_ets
start_seasonality_ets_p1 <- start(p1_forecast_ets)[2]
end_seasonality_ets_p1 <- end(p1_forecast_ets)[2]
if(end_seasonality_ets_p1 > start_seasonality_ets_p1){
  p1_forecast_ets<-
    p1_forecast_ets*last_trend*decomposed_p1_mult$figure[start_seasonality_ets_p1:end_seasonality_ets_p1]
}
if(start_seasonality_ets_p1 > end_seasonality_ets_p1){
  counter_ets <- c(start_seasonality_ets_p1:16, 1:end_seasonality_ets_p1)
  p1_forecast_ets<- p1_forecast_ets*last_trend*decomposed_p1_mult$figure[counter_ets]
}
p1_forecast_ets
#naivemodel
p1_naive_model <- naive(deseas_p1,h=10)
autoplot(p1_naive_model)
summary(p1_naive_model)
residuals(p1_naive_model)
model_fitted_naive_p1 <- p1_naive_model$fitted
model_fitted_p1_naive_seasonality_trend<-model_fitted_naive_p1*
  (decomposed_p1_mult$seasonal*decomposed_p1_mult$trend)
accuracy(p1_naive_model)
p1_forecast_naive<-predict(p1_naive_model,h=10)$mean
p1_forecast_naive
start_seasonality_naive_p1 <- start(p1_forecast_naive)[2]
end_seasonality_naive_p1 <- end(p1_forecast_naive)[2]
if(end_seasonality_naive_p1 > start_seasonality_naive_p1){
  p1_forecast_naive<-
    
    p1_forecast_naive*last_trend*decomposed_p1_mult$figure[start_seasonality_naive_p1:end_seasonality_naive_p1]
}
if(start_seasonality_naive_p1 > end_seasonality_naive_p1){
  counter_naive <- c(start_seasonality_naive_p1:16, 1:end_seasonality_naive_p1)
  p1_forecast_naive<- p1_forecast_naive*last_trend*decomposed_p1_mult$figure[counter_naive]
}
p1_forecast_naive
#------------------------------product 1matrix----------------------------
product1_valid_matrix <- data.frame(list(p1_arima = rep(0,10),p1_arima_reg = rep(0,10), p1_ets = rep(0,10),
                                         p1_naive = rep(0,10), p1_test =
                                           rep(0,10), p1_arima_error = rep(0,10), p1_reg_error = rep(0,10),p1_ets_error =
                                           rep(0,10),p1_naive_error = rep(0,10) ))
product1_valid_matrix$p1_arima <- p1_forecast_arima
product1_valid_matrix$p1_test <- product1_ts[(length(product1_ts)-9):(length(product1_ts))]
product1_valid_matrix$p1_arima_error <- product1_valid_matrix$p1_test-product1_valid_matrix$p1_arima
product1_valid_matrix$p1_arima_reg <-p1_forecast_reg
product1_valid_matrix$p1_reg_error<-product1_valid_matrix$p1_test-product1_valid_matrix$p1_arima_reg
product1_valid_matrix$p1_ets <-p1_forecast_ets
product1_valid_matrix$p1_ets_error<-product1_valid_matrix$p1_test-product1_valid_matrix$p1_ets
product1_valid_matrix$p1_naive <-p1_forecast_naive
product1_valid_matrix$p1_naive_error<-product1_valid_matrix$p1_test-product1_valid_matrix$p1_naive
mape_test_arima<-100*mean(abs(product1_valid_matrix$p1_arima_error)/product1_valid_matrix$p1_test)
mape_test_reg<- 100*mean(abs(product1_valid_matrix$p1_reg_error)/product1_valid_matrix$p1_test)
mape_test_ets<- 100*mean(abs(product1_valid_matrix$p1_ets_error)/product1_valid_matrix$p1_test)
mape_test_naive<- 100*mean(abs(product1_valid_matrix$p1_naive_error)/product1_valid_matrix$p1_test)
mape_test_arima
mape_test_reg
mape_test_ets
mape_test_naive
#!!!!!choose arima with reg!!!!!!
decomposed_p1_mult<-decompose(product1_ts,type="multiplicative")
last_trend=decomposed_p1_mult$trend[max(which(!is.na(decomposed_p1_mult$trend)))]
last_trend
first_trend=decomposed_p1_mult$trend[min(which(!is.na(decomposed_p1_mult$trend)))]
first_trend
frequency_p1<-frequency(decomposed_p1_mult$trend)
decomposed_p1_mult$trend[1:(frequency_p1/2)]=first_trend
decomposed_p1_mult$trend[(length(decomposed_p1_mult$trend)-
                            (frequency_p1/2)+1):length(decomposed_p1_mult$trend)]=last_trend
detrend_p1<- product1_ts / decomposed_p1_mult$trend
deseas_p1<-detrend_p1 / decomposed_p1_mult$seasonal
p1_price_reg_matrix <- matrix(p1_price_ts[1:length(p1_price_ts)])
p1_visit_reg_matrix <- matrix(p1_visit_ts[1:length(p1_visit_ts)])
p1_reg_matrix <- cbind(p1_price_reg_matrix, p1_visit_reg_matrix)
colnames(p1_reg_matrix) <- c("price", "visit_turnover_rate")
p1_reg_model <- auto.arima(deseas_p1, xreg = p1_reg_matrix, seasonal = F)
model_fitted_reg_p1 <- deseas_p1 - residuals(p1_reg_model)
model_fitted_p1_reg_seasonality_trend<-model_fitted_reg_p1*
  (decomposed_p1_mult$seasonal*decomposed_p1_mult$trend)
ts.plot(deseas_p1, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_reg_p1, type = "l", col = 2, lty = 2)
ts.plot(product1_ts, xlab = "Time Index", ylab = "Random Part",main="Sold Product1")
points(model_fitted_p1_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p1_reg_matrix_test_1 <- rep(p1_reg_matrix[nrow(p1_reg_matrix),1],2)
p1_reg_matrix_test_2 <- rep(p1_reg_matrix[nrow(p1_reg_matrix),2],2)
p1_reg_matrix_test <- cbind(p1_reg_matrix_test_1, p1_reg_matrix_test_2)
colnames(p1_reg_matrix_test) <- c("price", "visit_turnover_rate")
p1_forecast_reg<-forecast(p1_reg_model,h=2,xreg=p1_reg_matrix_test)$mean
p1_forecast_reg
start_seasonality_reg_p1 <- start(p1_forecast_reg)[2]

end_seasonality_reg_p1 <- end(p1_forecast_reg)[2]
if(end_seasonality_reg_p1 > start_seasonality_reg_p1){
  p1_forecast_reg<-
    p1_forecast_reg*last_trend*decomposed_p1_mult$figure[start_seasonality_reg_p1:end_seasonality_reg_p1]
}
if(start_seasonality_reg_p1 > end_seasonality_reg_p1){
  counter_reg <- c(start_seasonality_reg_p1:frequency_p1, 1:end_seasonality_reg_p1)
  p1_forecast_reg<- p1_forecast_reg*last_trend*decomposed_p1_mult$figure[counter_reg]
}
p1_forecast_reg
names(p1_forecast_reg) <- c("1 day ahead forecast", "2 day ahead forecast")
p1_forecast_reg
#
#------------------------------product 2----------------------------
#
product2 <- product2[134:length(product2$sold_count)]
#first 133 deleted because there is no data.
product2_ts = ts(product2$sold_count,start = min(product2$event_date))
ts.plot(product2_ts)
acf(product2_ts)
#choose lag=18
product2_ts = ts(product2$sold_count,start = min(product2$event_date),frequency = 18)
plot(product2_ts)
plot.ts(product2_ts)
#product2_ts[190:220] = NA
product2_ts[61:63] = (product2_ts[61:63]^1/4)
product2_ts[77:81] = (product2_ts[77:81]^1/4)
plot.ts(product2_ts)
product2_ts_train <- product2_ts[0:((length(product2_ts)-10))]
product2_ts_train <- ts(product2_ts_train, frequency = 18)
decomposed_p2_add<-decompose(product2_ts_train,type="additive")
plot(decomposed_p2_add)
decomposed_p2_mult<-decompose(product2_ts_train,type="multiplicative")
plot(decomposed_p2_mult)
#choose multipliactive
last_trend=decomposed_p2_mult$trend[max(which(!is.na(decomposed_p2_mult$trend)))]
last_trend
first_trend=decomposed_p2_mult$trend[min(which(!is.na(decomposed_p2_mult$trend)))]
first_trend
frequency_p2<-frequency(decomposed_p2_mult$trend)
decomposed_p2_mult$trend[1:(frequency_p2/2)]=first_trend
decomposed_p2_mult$trend[(length(decomposed_p2_mult$trend)-
                            (frequency_p2/2)+1):length(decomposed_p2_mult$trend)]=last_trend
head(decomposed_p2_mult$trend,20)
tail(decomposed_p2_mult$trend,20)
detrend_p2<- product2_ts_train / decomposed_p2_mult$trend
deseas_p2<-detrend_p2 / decomposed_p2_mult$seasonal
arima_model_p2<-auto.arima(deseas_p2,seasonal = F)
summary(arima_model_p2)
model_fitted_p2 <- deseas_p2 - residuals(arima_model_p2)
model_fitted_p2_seasonality_trend<-model_fitted_p2*
  (decomposed_p2_mult$seasonal*decomposed_p2_mult$trend)
ts.plot(deseas_p2, xlab = "Time Index", ylab = "Random Part",main="Sold product2")
points(model_fitted_p2, type = "l", col = 2, lty = 2)
ts.plot(product2_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product2")
points(model_fitted_p2_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p2_seasonality_trend - product2_ts_train)^2)) / length(product2_ts_train)
p2_forecast_arima<-predict(arima_model_p2,n.ahead=10)$pred
p2_forecast_arima
start_seasonality_p2 <- start(p2_forecast_arima)[2]
end_seasonality_p2 <- end(p2_forecast_arima)[2]
if(end_seasonality_p2 > start_seasonality_p2){
  
  p2_forecast_arima<-
    p2_forecast_arima*last_trend*decomposed_p2_mult$figure[start_seasonality_p2:end_seasonality_p2]
}
if(start_seasonality_p2 > end_seasonality_p2){
  counter <- c(start_seasonality_p2:frequency_p2, 1:end_seasonality_p2)
  p2_forecast_arima<- p2_forecast_arima*last_trend*decomposed_p2_mult$figure[counter]
}
p2_forecast_arima
#arima_reg model
p2_visit_ts <- ts(product2$visit_turnover_rate, start= min(product2$event_date))
which.max(p2_visit_ts)
plot(p2_visit_ts)
p2_visit_ts[which.max(p2_visit_ts)] = mean(p2_visit_ts[5:15])
p2_visit_ts[which.max(p2_visit_ts)] = mean(p2_visit_ts[5:15])
plot(p2_visit_ts)
Box.test(p2_visit_ts)
p2_price_ts <- ts(product2$price, start= min(product2$event_date))
ts.plot(p2_price_ts)
p2_price_ts[which(p2_price_ts == min(p2_price_ts))] <- NA
plot(p2_price_ts)
p2_price_reg_matrix <- matrix(p2_price_ts[1:(length(p2_price_ts)-10)])
p2_visit_reg_matrix <- matrix(p2_visit_ts[1:(length(p2_visit_ts)-10)])
p2_reg_matrix <- cbind(p2_price_reg_matrix, p2_visit_reg_matrix)
colnames(p2_reg_matrix) <- c("price", "visit_turnover_rate")
p2_reg_model <- auto.arima(deseas_p2, xreg = p2_reg_matrix, seasonal = F)
summary(p2_reg_model)
model_fitted_reg_p2 <- deseas_p2 - residuals(p2_reg_model)
model_fitted_p2_reg_seasonality_trend<-model_fitted_reg_p2*
  (decomposed_p2_mult$seasonal*decomposed_p2_mult$trend)
ts.plot(deseas_p2, xlab = "Time Index", ylab = "Random Part",main="Sold Product2")
points(model_fitted_reg_p2, type = "l", col = 2, lty = 2)
ts.plot(product2_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product2")
points(model_fitted_p2_reg_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p2_reg_seasonality_trend - product2_ts_train)^2)) / length(product2_ts_train)
p2_reg_matrix_test_1 <- rep(p2_reg_matrix[nrow(p2_reg_matrix),1],10)
p2_reg_matrix_test_2 <- rep(p2_reg_matrix[nrow(p2_reg_matrix),2],10)
p2_reg_matrix_test <- cbind(p2_reg_matrix_test_1, p2_reg_matrix_test_2)
colnames(p2_reg_matrix_test) <- c("price", "visit_turnover_rate")
p2_forecast_reg<-forecast(p2_reg_model,h=10,xreg=p2_reg_matrix_test)$mean
p2_forecast_reg
start_seasonality_reg_p2 <- start(p2_forecast_reg)[2]
end_seasonality_reg_p2 <- end(p2_forecast_reg)[2]
if(end_seasonality_reg_p2 > start_seasonality_reg_p2){
  p2_forecast_reg<-
    p2_forecast_reg*last_trend*decomposed_p2_mult$figure[start_seasonality_reg_p2:end_seasonality_reg_p2]
}
if(start_seasonality_reg_p2 > end_seasonality_reg_p2){
  counter_reg <- c(start_seasonality_reg_p2:frequency_p2, 1:end_seasonality_reg_p2)
  p2_forecast_reg<- p2_forecast_reg*last_trend*decomposed_p2_mult$figure[counter_reg]
}
p2_forecast_reg
#etsmodel
p2_ets_model <- ets(deseas_p2)
autoplot(p2_ets_model)
summary(p2_ets_model)
residuals(p2_ets_model)
model_fitted_ets_p2 <- p2_ets_model$fitted
model_fitted_p2_ets_seasonality_trend<-model_fitted_ets_p2*
  (decomposed_p2_mult$seasonal*decomposed_p2_mult$trend)
accuracy(p2_ets_model)
p2_forecast_ets<-predict(p2_ets_model,h=10)$mean

p2_forecast_ets
start_seasonality_ets_p2 <- start(p2_forecast_ets)[2]
end_seasonality_ets_p2 <- end(p2_forecast_ets)[2]
if(end_seasonality_ets_p2 > start_seasonality_ets_p2){
  p2_forecast_ets<-
    p2_forecast_ets*last_trend*decomposed_p2_mult$figure[start_seasonality_ets_p2:end_seasonality_ets_p2]
}
if(start_seasonality_ets_p2 > end_seasonality_ets_p2){
  counter_ets <- c(start_seasonality_ets_p2:frequency_p2, 1:end_seasonality_ets_p2)
  p2_forecast_ets<- p2_forecast_ets*last_trend*decomposed_p2_mult$figure[counter_ets]
}
p2_forecast_ets
#naivemodel
p2_naive_model <- naive(deseas_p2,h=10)
autoplot(p2_naive_model)
summary(p2_naive_model)
residuals(p2_naive_model)
model_fitted_naive_p2 <- p2_naive_model$fitted
model_fitted_p2_naive_seasonality_trend<-model_fitted_naive_p2*
  (decomposed_p2_mult$seasonal*decomposed_p2_mult$trend)
accuracy(p2_naive_model)
p2_forecast_naive<-predict(p2_naive_model,h=10)$mean
p2_forecast_naive
start_seasonality_naive_p2 <- start(p2_forecast_naive)[2]
end_seasonality_naive_p2 <- end(p2_forecast_naive)[2]
if(end_seasonality_naive_p2 > start_seasonality_naive_p2){
  p2_forecast_naive<-
    p2_forecast_naive*last_trend*decomposed_p2_mult$figure[start_seasonality_naive_p2:end_seasonality_naive_p2]
}
if(start_seasonality_naive_p2 > end_seasonality_naive_p2){
  counter_naive <- c(start_seasonality_naive_p2:frequency_p2, 1:end_seasonality_naive_p2)
  p2_forecast_naive<- p2_forecast_naive*last_trend*decomposed_p2_mult$figure[counter_naive]
}
p2_forecast_naive
#------------------------------product2 matrix----------------------------
product2_valid_matrix <- data.frame(list(p2_arima = rep(0,10),p2_arima_reg = rep(0,10), p2_ets = rep(0,10),
                                         p2_naive = rep(0,10), p2_test =
                                           rep(0,10), p2_arima_error = rep(0,10), p2_reg_error = rep(0,10),p2_ets_error =
                                           rep(0,10),p2_naive_error = rep(0,10) ))
product2_valid_matrix$p2_arima <- p2_forecast_arima
product2_valid_matrix$p2_test <- product2_ts[(length(product2_ts)-9):(length(product2_ts))]
product2_valid_matrix$p2_arima_error <- product2_valid_matrix$p2_test-product2_valid_matrix$p2_arima
product2_valid_matrix$p2_arima_reg <-p2_forecast_reg
product2_valid_matrix$p2_reg_error<-product2_valid_matrix$p2_test-product2_valid_matrix$p2_arima_reg
product2_valid_matrix$p2_ets <-p2_forecast_ets
product2_valid_matrix$p2_ets_error<-product2_valid_matrix$p2_test-product2_valid_matrix$p2_ets
product2_valid_matrix$p2_naive <-p2_forecast_naive
product2_valid_matrix$p2_naive_error<-product2_valid_matrix$p2_test-product2_valid_matrix$p2_naive
mape_test_arima<-100*mean(abs(product2_valid_matrix$p2_arima_error)/product2_valid_matrix$p2_test)
mape_test_reg<- 100*mean(abs(product2_valid_matrix$p2_reg_error)/product2_valid_matrix$p2_test)
mape_test_ets<- 100*mean(abs(product2_valid_matrix$p2_ets_error)/product2_valid_matrix$p2_test)
mape_test_naive<- 100*mean(abs(product2_valid_matrix$p2_naive_error)/product2_valid_matrix$p2_test)
mape_test_arima
mape_test_reg
mape_test_ets

mape_test_naive
#!!!!!choose arima!!!!!!
decomposed_p2_mult<-decompose(product2_ts,type="multiplicative")
last_trend=decomposed_p2_mult$trend[max(which(!is.na(decomposed_p2_mult$trend)))]
first_trend=decomposed_p2_mult$trend[min(which(!is.na(decomposed_p2_mult$trend)))]
frequency_p2<-frequency(decomposed_p2_mult$trend)
decomposed_p2_mult$trend[1:(frequency_p2/2)]=first_trend
decomposed_p2_mult$trend[(length(decomposed_p2_mult$trend)-
                            (frequency_p2/2)+1):length(decomposed_p2_mult$trend)]=last_trend
detrend_p2<- product2_ts / decomposed_p2_mult$trend
deseas_p2<-detrend_p2 / decomposed_p2_mult$seasonal
arima_model_p2<-auto.arima(deseas_p2,seasonal = F)
summary(arima_model_p2)
model_fitted_p2 <- deseas_p2 - residuals(arima_model_p2)
model_fitted_p2_seasonality_trend<-model_fitted_p2*
  (decomposed_p2_mult$seasonal*decomposed_p2_mult$trend)
ts.plot(deseas_p2, xlab = "Time Index", ylab = "Random Part",main="Sold product2")
points(model_fitted_p2, type = "l", col = 2, lty = 2)
ts.plot(product2_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product2")
points(model_fitted_p2_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p2_seasonality_trend - product2_ts)^2)) / length(product2_ts)
p2_forecast_arima<-predict(arima_model_p2,n.ahead=2)$pred
p2_forecast_arima
start_seasonality_p2 <- start(p2_forecast_arima)[2]
end_seasonality_p2 <- end(p2_forecast_arima)[2]
p2_forecast_arima<-
  p2_forecast_arima*last_trend*decomposed_p2_mult$figure[start_seasonality_p2:end_seasonality_p2]
p2_forecast_arima
#
#product 3
#inconsistent data, low sold count values in the beginning.
product3 <- product3[134:length(product3$sold_count)]
product3[94] <- mean(product3[84:104]$sold_count)
#mean value for stockout
product3_ts = ts(product3$sold_count,start = min(product3$event_date), freq= 1)
product3_ts = ts(product3$sold_count,start = min(product3$event_date))
ts.plot(product3_ts)
acf(product3_ts)
#choose lag=20
product3_ts = ts(product3$sold_count,start = min(product3$event_date),frequency = 20)
plot(product3_ts)
plot.ts(product3_ts)
product3_ts[61:63] = (product3_ts[61:63]^1/4)
product3_ts[80:81] = (product3_ts[80:81]^1/4)
#normalizing outliers
product3_ts_train <- product3_ts[0:((length(product3_ts)-10))]
product3_ts_train <- ts(product3_ts_train, frequency = 20)
plot.ts(product3_ts)
decomposed_p3_add<-decompose(product3_ts_train,type="additive")
plot(decomposed_p3_add)
decomposed_p3_mult<-decompose(product3_ts_train,type="multiplicative")
plot(decomposed_p3_mult)
#choose additive
last_trend=decomposed_p3_add$trend[max(which(!is.na(decomposed_p3_add$trend)))]
last_trend
first_trend=decomposed_p3_add$trend[min(which(!is.na(decomposed_p3_add$trend)))]
first_trend
frequency_p3<-frequency(decomposed_p3_add$trend)
decomposed_p3_add$trend[1:(frequency_p3/2)]=first_trend
decomposed_p3_add$trend[(length(decomposed_p3_add$trend)-
                           (frequency_p3/2)+1):length(decomposed_p3_add$trend)]=last_trend
head(decomposed_p3_add$trend,20)
tail(decomposed_p3_add$trend,20)
detrend_p3<- product3_ts_train - decomposed_p3_add$trend
deseas_p3<-detrend_p3 - decomposed_p3_add$seasonal
arima_model_p3<-auto.arima(deseas_p3,seasonal = F)
summary(arima_model_p3)

model_fitted_p3 <- deseas_p3 - residuals(arima_model_p3)
model_fitted_p3_seasonality_trend<-model_fitted_p3+
  (decomposed_p3_add$seasonal+decomposed_p3_add$trend)
ts.plot(deseas_p3, xlab = "Time Index", ylab = "Random Part",main="Sold product3")
points(model_fitted_p3, type = "l", col = 2, lty = 2)
ts.plot(product3_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product3")
points(model_fitted_p3_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p3_seasonality_trend - product3_ts)^2)) / length(product3_ts)
p3_forecast_arima<-predict(arima_model_p3,n.ahead=10)$pred
# Finding seasonality factors for additive
start_seasonality_p3 <- (length(product3_ts)+1) %% frequency_p3
end_seasonality_p3 <- (length(product3_ts)+11) %% frequency_p3
p3_forecast_arima <- p3_forecast_arima + last_trend
+decomposed_p3_add$figure[start_seasonality_p3:end_seasonality_p3]
p3_forecast_arima
#arima_reg model
p3_visit_ts <- ts(product3$visit_turnover_rate, start= min(product3$event_date))
which.max(p3_visit_ts)
p3_visit_ts[which.max(p3_visit_ts)] = mean(p3_visit_ts[95:105])
plot(p3_visit_ts)
which.max(p3_visit_ts)
p3_visit_ts[which.max(p3_visit_ts)] = mean(p3_visit_ts[5:15])
plot(p3_visit_ts)
Box.test(p3_visit_ts)
p3_price_ts <- ts(product3$price, start= min(product3$event_date))
ts.plot(p3_price_ts)
p3_price_ts[which(p3_price_ts == max(p3_price_ts))] <- NA
plot(p3_price_ts)
p3_price_reg_matrix <- matrix(p3_price_ts[1:(length(p3_price_ts)-10)])
p3_visit_reg_matrix <- matrix(p3_visit_ts[1:(length(p3_visit_ts)-10)])
p3_reg_matrix <- cbind(p3_price_reg_matrix, p3_visit_reg_matrix)
colnames(p3_reg_matrix) <- c("price", "visit_turnover_rate")
p3_reg_model <- auto.arima(deseas_p3, xreg = p3_reg_matrix, seasonal = F)
summary(p3_reg_model)
model_fitted_reg_p3 <- deseas_p3 - residuals(p3_reg_model)
model_fitted_p3_reg_seasonality_trend<-model_fitted_reg_p3+
  (decomposed_p3_add$seasonal+decomposed_p3_add$trend)
ts.plot(deseas_p3, xlab = "Time Index", ylab = "Random Part",main="Sold Product3")
points(model_fitted_reg_p3, type = "l", col = 2, lty = 2)
ts.plot(product3_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product3")
points(model_fitted_p3_reg_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p3_reg_seasonality_trend - product3_ts_train)^2)) / length(product3_ts_train)
p3_reg_matrix_test_1 <- rep(p3_reg_matrix[nrow(p3_reg_matrix),1],10)
p3_reg_matrix_test_2 <- rep(p3_reg_matrix[nrow(p3_reg_matrix),2],10)
p3_reg_matrix_test <- cbind(p3_reg_matrix_test_1, p3_reg_matrix_test_2)
colnames(p3_reg_matrix_test) <- c("price", "visit_turnover_rate")
p3_forecast_reg<-forecast(p3_reg_model,h=10,xreg=p3_reg_matrix_test)$mean
p3_forecast_reg

# additive specs.

start_seasonality_reg_p3 <- (length(product3_ts_train)+1) %% frequency_p3
end_seasonality_reg_p3 <- (length(product3_ts_train)+2) %% frequency_p3
if(end_seasonality_reg_p3 > start_seasonality_reg_p3){
  p3_forecast_reg<-
    p3_forecast_reg+last_trend+decomposed_p3_add$figure[start_seasonality_reg_p3:end_seasonality_reg_p3]
}
if(start_seasonality_reg_p3 > end_seasonality_reg_p3){
  counter_reg <- c(start_seasonality_reg_p3:frequency_p3, 1:end_seasonality_reg_p3)
  p3_forecast_reg<- p3_forecast_reg+last_trend+decomposed_p3_add$figure[counter_reg]
}
p3_forecast_reg

#etsmodel
p3_ets_model <- ets(deseas_p3)
autoplot(p3_ets_model)
summary(p3_ets_model)
residuals(p3_ets_model)
model_fitted_ets_p3 <- p3_ets_model$fitted
model_fitted_p3_ets_seasonality_trend<-model_fitted_ets_p3+
  (decomposed_p3_add$seasonal+decomposed_p3_add$trend)
accuracy(p3_ets_model)
p3_forecast_ets<-predict(p3_ets_model,h=10)$mean
p3_forecast_ets
start_seasonality_ets_p3 <- (length(product3_ts_train)+1) %% frequency_p3
end_seasonality_ets_p3 <- (length(product3_ts_train)+2) %% frequency_p3
if(end_seasonality_ets_p3 > start_seasonality_ets_p3){
  p3_forecast_ets<-
    p3_forecast_ets+last_trend+decomposed_p3_add$figure[start_seasonality_ets_p3:end_seasonality_ets_p3]
}
if(start_seasonality_ets_p3 > end_seasonality_ets_p3){
  counter_ets <- c(start_seasonality_ets_p3:frequency_p3, 1:end_seasonality_ets_p3)
  p3_forecast_ets<- p3_forecast_ets+last_trend+decomposed_p3_add$figure[counter_ets]
}
p3_forecast_ets
#naivemodel
p3_naive_model <- naive(deseas_p3,h=10)
autoplot(p3_naive_model)
summary(p3_naive_model)
residuals(p3_naive_model)
model_fitted_naive_p3 <- p3_naive_model$fitted
model_fitted_p3_naive_seasonality_trend<-model_fitted_naive_p3+
  (decomposed_p3_add$seasonal+decomposed_p3_add$trend)
accuracy(p3_naive_model)
p3_forecast_naive<-predict(p3_naive_model,h=10)$mean
p3_forecast_naive
start_seasonality_naive_p3 <- (length(product3_ts_train)+1) %% frequency_p3
end_seasonality_naive_p3 <- (length(product3_ts_train)+2) %% frequency_p3
if(end_seasonality_naive_p3 > start_seasonality_naive_p3){
  p3_forecast_naive<-
    p3_forecast_naive+last_trend+decomposed_p3_add$figure[start_seasonality_naive_p3:end_seasonality_naive_p3]
}
if(start_seasonality_naive_p3 > end_seasonality_naive_p3){
  counter_naive <- c(start_seasonality_naive_p3:frequency_p3, 1:end_seasonality_naive_p3)
  p3_forecast_naive<- p3_forecast_naive+last_trend+decomposed_p3_add$figure[counter_naive]
}
p3_forecast_naive
#------------------------------product3 matrix----------------------------
product3_valid_matrix <- data.frame(list(p3_arima = rep(0,10),p3_arima_reg = rep(0,10), p3_ets = rep(0,10),
                                         p3_naive = rep(0,10), p3_test =
                                           rep(0,10), p3_arima_error = rep(0,10), p3_reg_error = rep(0,10),p3_ets_error =
                                           rep(0,10),p3_naive_error = rep(0,10) ))
product3_valid_matrix$p3_arima <- p3_forecast_arima
product3_valid_matrix$p3_test <- product3_ts[(length(product3_ts)-9):(length(product3_ts))]
product3_valid_matrix$p3_arima_error <- product3_valid_matrix$p3_test-product3_valid_matrix$p3_arima
product3_valid_matrix$p3_arima_reg <-p3_forecast_reg
product3_valid_matrix$p3_reg_error<-product3_valid_matrix$p3_test-product3_valid_matrix$p3_arima_reg

product3_valid_matrix$p3_ets <-p3_forecast_ets
product3_valid_matrix$p3_ets_error<-product3_valid_matrix$p3_test-product3_valid_matrix$p3_ets
product3_valid_matrix$p3_naive <-p3_forecast_naive
product3_valid_matrix$p3_naive_error<-product3_valid_matrix$p3_test-product3_valid_matrix$p3_naive
mape_test_arima<-100*mean(abs(product3_valid_matrix$p3_arima_error)/product3_valid_matrix$p3_test)
mape_test_reg<- 100*mean(abs(product3_valid_matrix$p3_reg_error)/product3_valid_matrix$p3_test)
mape_test_ets<- 100*mean(abs(product3_valid_matrix$p3_ets_error)/product3_valid_matrix$p3_test)
mape_test_naive<- 100*mean(abs(product3_valid_matrix$p3_naive_error)/product3_valid_matrix$p3_test)
mape_test_arima
mape_test_reg
mape_test_ets
mape_test_naive
#!!!!!choose arima with reg!!!!!!
decomposed_p3_add<-decompose(product3_ts,type="additive")
last_trend=decomposed_p3_add$trend[max(which(!is.na(decomposed_p3_add$trend)))]
first_trend=decomposed_p3_add$trend[min(which(!is.na(decomposed_p3_add$trend)))]
frequency_p3<-frequency(decomposed_p3_add$trend)
decomposed_p3_add$trend[1:(frequency_p3/2)]=first_trend
decomposed_p3_add$trend[(length(decomposed_p3_add$trend)-
                           (frequency_p3/2)+1):length(decomposed_p3_add$trend)]=last_trend
detrend_p3<- product3_ts - decomposed_p3_add$trend
deseas_p3<-detrend_p3 - decomposed_p3_add$seasonal
p3_price_reg_matrix <- matrix(p3_price_ts[1:length(p3_price_ts)])
p3_visit_reg_matrix <- matrix(p3_visit_ts[1:length(p3_visit_ts)])
p3_reg_matrix <- cbind(p3_price_reg_matrix, p3_visit_reg_matrix)
colnames(p3_reg_matrix) <- c("price", "visit_turnover_rate")
p3_reg_model <- auto.arima(deseas_p3, xreg = p3_reg_matrix, seasonal = F)
model_fitted_reg_p3 <- deseas_p3 - residuals(p3_reg_model)
model_fitted_p3_reg_seasonality_trend<-model_fitted_reg_p3+
  (decomposed_p3_add$seasonal+decomposed_p3_add$trend)
ts.plot(deseas_p3, xlab = "Time Index", ylab = "Random Part",main="Sold product3")
points(model_fitted_reg_p3, type = "l", col = 2, lty = 2)
ts.plot(product3_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product3")
points(model_fitted_p3_reg_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p3_reg_seasonality_trend - product3_ts)^2)) / length(product3_ts)
p3_reg_matrix_test_1 <- rep(p3_reg_matrix[nrow(p3_reg_matrix),1],2)
p3_reg_matrix_test_2 <- rep(p3_reg_matrix[nrow(p3_reg_matrix),2],2)
p3_reg_matrix_test <- cbind(p3_reg_matrix_test_1, p3_reg_matrix_test_2)
colnames(p3_reg_matrix_test) <- c("price", "visit_turnover_rate")
p3_forecast_reg<-forecast(p3_reg_model,h=2,xreg=p3_reg_matrix_test)$mean
p3_forecast_reg
start_seasonality_reg_p3 <- (length(product3_ts)+1) %% frequency_p3
end_seasonality_reg_p3 <- (length(product3_ts)+2) %% frequency_p3
if(end_seasonality_reg_p3 > start_seasonality_reg_p3){
  p3_forecast_reg<-
    p3_forecast_reg+last_trend+decomposed_p3_add$figure[start_seasonality_reg_p3:end_seasonality_reg_p3]
}
if(start_seasonality_reg_p3 > end_seasonality_reg_p3){
  counter_reg <- c(start_seasonality_reg_p3:frequency_p3, 1:end_seasonality_reg_p3)
  p3_forecast_reg<- p3_forecast_reg+last_trend+decomposed_p3_add$figure[counter_reg]
}
p3_forecast_reg
names(p3_forecast_reg) <- c("1 day ahead forecast", "2 day ahead forecast")
p3_forecast_reg
#
#-----------------------------------product 4----------------------------------------
#
product4 <- product4[89:length(product4$sold_count)]
product4_ts = ts(product4$sold_count,start = min(product4$event_date), freq= 1)
product4_ts = ts(product4$sold_count,start = min(product4$event_date))
ts.plot(product4_ts)
acf(product4_ts)
#choose lag=10
product4_ts = ts(product4$sold_count,start = min(product4$event_date),frequency = 10)

plot(product4_ts)
plot.ts(product4_ts)
#product4_ts[106] = NA
product4_ts[106] = mean(product4_ts)
plot.ts(product4_ts)
product4_ts_train <- product4_ts[0:((length(product4_ts)-10))]
product4_ts_train <- ts(product4_ts_train, frequency = 10)
decomposed_p4_add<-decompose(product4_ts_train,type="additive")
plot(decomposed_p4_add)
decomposed_p4_mult<-decompose(product4_ts_train,type="multiplicative")
plot(decomposed_p4_mult)
#choose multipliactive
last_trend=decomposed_p4_mult$trend[max(which(!is.na(decomposed_p4_mult$trend)))]
last_trend
first_trend=decomposed_p4_mult$trend[min(which(!is.na(decomposed_p4_mult$trend)))]
first_trend
frequency_p4<-frequency(decomposed_p4_mult$trend)
decomposed_p4_mult$trend[1:(frequency_p4/2)]=first_trend
decomposed_p4_mult$trend[(length(decomposed_p4_mult$trend)-
                            (frequency_p4/2)+1):length(decomposed_p4_mult$trend)]=last_trend
head(decomposed_p4_mult$trend,20)
tail(decomposed_p4_mult$trend,20)
detrend_p4<- product4_ts_train / decomposed_p4_mult$trend
deseas_p4<-detrend_p4 / decomposed_p4_mult$seasonal
arima_model_p4<-auto.arima(deseas_p4,seasonal = F)
summary(arima_model_p4)
model_fitted_p4 <- deseas_p4 - residuals(arima_model_p4)
model_fitted_p4_seasonality_trend<-model_fitted_p4*
  (decomposed_p4_mult$seasonal*decomposed_p4_mult$trend)
ts.plot(deseas_p4, xlab = "Time Index", ylab = "Random Part",main="Sold product4")
points(model_fitted_p4, type = "l", col = 2, lty = 2)
ts.plot(product4_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product4")

points(model_fitted_p4_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p4_seasonality_trend - product4_ts_train)^2)) / length(product4_ts_train)
p4_forecast_arima<-predict(arima_model_p4,n.ahead=10)$pred
p4_forecast_arima
start_seasonality_p4 <- start(p4_forecast_arima)[2]
end_seasonality_p4 <- end(p4_forecast_arima)[2]
if(end_seasonality_p4 > start_seasonality_p4){
  
  p4_forecast_arima<-
    p4_forecast_arima*last_trend*decomposed_p4_mult$figure[start_seasonality_p4:end_seasonality_p4]
}
if(start_seasonality_p4 > end_seasonality_p4){
  counter <- c(start_seasonality_p4:frequency_p4, 1:end_seasonality_p4)
  p4_forecast_arima<- p4_forecast_arima*last_trend*decomposed_p4_mult$figure[counter]
}
p4_forecast_arima
#arima_reg model
p4_visit_ts <- ts(product4$visit_turnover_rate, start= min(product4$event_date))
which.min(p4_visit_ts)
p4_visit_ts[p4_visit_ts==0]<-NA
plot(p4_visit_ts)
Box.test(p4_visit_ts)
p4_price_ts <- ts(product4$price, start= min(product4$event_date))
ts.plot(p4_price_ts)
p4_price_ts[p4_price_ts == -1] <- NA
plot(p4_price_ts)
p4_price_reg_matrix <- matrix(p4_price_ts[1:(length(p4_price_ts)-10)])
p4_visit_reg_matrix <- matrix(p4_visit_ts[1:(length(p4_visit_ts)-10)])
p4_reg_matrix <- cbind(p4_price_reg_matrix, p4_visit_reg_matrix)
colnames(p4_reg_matrix) <- c("price", "visit_turnover_rate")
p4_reg_model <- auto.arima(deseas_p4, xreg = p4_reg_matrix, seasonal = F)
summary(p4_reg_model)
model_fitted_reg_p4 <- deseas_p4 - residuals(p4_reg_model)
model_fitted_p4_reg_seasonality_trend<-model_fitted_reg_p4*
  (decomposed_p4_mult$seasonal*decomposed_p4_mult$trend)
ts.plot(deseas_p4, xlab = "Time Index", ylab = "Random Part",main="Sold Product4")
points(model_fitted_reg_p4, type = "l", col = 2, lty = 2)
ts.plot(product4_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product4")
points(model_fitted_p4_reg_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p4_reg_seasonality_trend - product4_ts_train)^2)) / length(product4_ts_train)
p4_reg_matrix_test_1 <- rep(p4_reg_matrix[nrow(p4_reg_matrix),1],10)
p4_reg_matrix_test_2 <- rep(p4_reg_matrix[nrow(p4_reg_matrix),2],10)
p4_reg_matrix_test <- cbind(p4_reg_matrix_test_1, p4_reg_matrix_test_2)
colnames(p4_reg_matrix_test) <- c("price", "visit_turnover_rate")
p4_forecast_reg<-forecast(p4_reg_model,h=10,xreg=p4_reg_matrix_test)$mean
p4_forecast_reg

start_seasonality_reg_p4 <- start(p4_forecast_reg)[2]
end_seasonality_reg_p4 <- end(p4_forecast_reg)[2]
if(end_seasonality_reg_p4 > start_seasonality_reg_p4){
  
  p4_forecast_reg<-
    p4_forecast_reg*last_trend*decomposed_p4_mult$figure[start_seasonality_reg_p4:end_seasonality_reg_p4]
}
if(start_seasonality_reg_p4 > end_seasonality_reg_p4){
  counter_reg <- c(start_seasonality_reg_p4:frequency_p4, 1:end_seasonality_reg_p4)
  p4_forecast_reg<- p4_forecast_reg*last_trend*decomposed_p4_mult$figure[counter_reg]
}
p4_forecast_reg

#etsmodel
p4_ets_model <- ets(deseas_p4)
autoplot(p4_ets_model)
summary(p4_ets_model)
residuals(p4_ets_model)
model_fitted_ets_p4 <- p4_ets_model$fitted
model_fitted_p4_ets_seasonality_trend<-model_fitted_ets_p4*
  (decomposed_p4_mult$seasonal*decomposed_p4_mult$trend)
accuracy(p4_ets_model)


p4_forecast_ets<-predict(p4_ets_model,h=10)$mean
p4_forecast_ets

start_seasonality_ets_p4 <- start(p4_forecast_ets)[2]
end_seasonality_ets_p4 <- end(p4_forecast_ets)[2]
if(end_seasonality_ets_p4 > start_seasonality_ets_p4){
  
  p4_forecast_ets<-
    p4_forecast_ets*last_trend*decomposed_p4_mult$figure[start_seasonality_ets_p4:end_seasonality_ets_p4]
}
if(start_seasonality_ets_p4 > end_seasonality_ets_p4){
  counter_ets <- c(start_seasonality_ets_p4:frequency_p4, 1:end_seasonality_ets_p4)
  p4_forecast_ets<- p4_forecast_ets*last_trend*decomposed_p4_mult$figure[counter_ets]
}
p4_forecast_ets

#naivemodel
p4_naive_model <- naive(deseas_p4,h=10)
autoplot(p4_naive_model)
summary(p4_naive_model)
residuals(p4_naive_model)
model_fitted_naive_p4 <- p4_naive_model$fitted
model_fitted_p4_naive_seasonality_trend<-model_fitted_naive_p4*
  (decomposed_p4_mult$seasonal*decomposed_p4_mult$trend)
accuracy(p4_naive_model)


p4_forecast_naive<-predict(p4_naive_model,h=10)$mean
p4_forecast_naive

start_seasonality_naive_p4 <- start(p4_forecast_naive)[2]
end_seasonality_naive_p4 <- end(p4_forecast_naive)[2]
if(end_seasonality_naive_p4 > start_seasonality_naive_p4){
  
  p4_forecast_naive<-
    p4_forecast_naive*last_trend*decomposed_p4_mult$figure[start_seasonality_naive_p4:end_seasonality_naive_p4]
}
if(start_seasonality_naive_p4 > end_seasonality_naive_p4){
  counter_naive <- c(start_seasonality_naive_p4:frequency_p4, 1:end_seasonality_naive_p4)
  p4_forecast_naive<- p4_forecast_naive*last_trend*decomposed_p4_mult$figure[counter_naive]
}
p4_forecast_naive


#------------------------------product4 matrix----------------------------

product4_valid_matrix <- data.frame(list(p4_arima = rep(0,10),p4_arima_reg = rep(0,10), p4_ets = rep(0,10), p4_naive = rep(0,10), p4_test =
                                           rep(0,10), p4_arima_error = rep(0,10), p4_reg_error = rep(0,10),p4_ets_error = rep(0,10),p4_naive_error = rep(0,10) ))

product4_valid_matrix$p4_arima <- p4_forecast_arima
product4_valid_matrix$p4_test <- product4_ts[(length(product4_ts)-9):(length(product4_ts))]
product4_valid_matrix$p4_arima_error <- product4_valid_matrix$p4_test-product4_valid_matrix$p4_arima
product4_valid_matrix$p4_arima_reg <-p4_forecast_reg
product4_valid_matrix$p4_reg_error<-product4_valid_matrix$p4_test-product4_valid_matrix$p4_arima_reg
product4_valid_matrix$p4_ets <-p4_forecast_ets
product4_valid_matrix$p4_ets_error<-product4_valid_matrix$p4_test-product4_valid_matrix$p4_ets
product4_valid_matrix$p4_naive <-p4_forecast_naive
product4_valid_matrix$p4_naive_error<-product4_valid_matrix$p4_test-product4_valid_matrix$p4_naive
mape_test_arima<-100*mean(abs(product4_valid_matrix$p4_arima_error)/product4_valid_matrix$p4_test)
mape_test_reg<- 100*mean(abs(product4_valid_matrix$p4_reg_error)/product4_valid_matrix$p4_test)
mape_test_ets<- 100*mean(abs(product4_valid_matrix$p4_ets_error)/product4_valid_matrix$p4_test)
mape_test_naive<- 100*mean(abs(product4_valid_matrix$p4_naive_error)/product4_valid_matrix$p4_test)
mape_test_arima
mape_test_reg
mape_test_ets
mape_test_naive

#choose arima reg model
decomposed_p4_mult<-decompose(product4_ts,type="multiplicative")
last_trend=decomposed_p4_mult$trend[max(which(!is.na(decomposed_p4_mult$trend)))]
last_trend
first_trend=decomposed_p4_mult$trend[min(which(!is.na(decomposed_p4_mult$trend)))]
first_trend
frequency_p4<-frequency(decomposed_p4_mult$trend)
decomposed_p4_mult$trend[1:(frequency_p4/2)]=first_trend
decomposed_p4_mult$trend[(length(decomposed_p4_mult$trend)-
                            (frequency_p4/2)+1):length(decomposed_p4_mult$trend)]=last_trend
detrend_p4<- product4_ts / decomposed_p4_mult$trend
deseas_p4<-detrend_p4 / decomposed_p4_mult$seasonal
p4_price_reg_matrix <- matrix(p4_price_ts[1:length(p4_price_ts)])
p4_visit_reg_matrix <- matrix(p4_visit_ts[1:length(p4_visit_ts)])
p4_reg_matrix <- cbind(p4_price_reg_matrix, p4_visit_reg_matrix)
colnames(p4_reg_matrix) <- c("price", "visit_turnover_rate")
p4_reg_model <- auto.arima(deseas_p4, xreg = p4_reg_matrix, seasonal = F)
model_fitted_reg_p4 <- deseas_p4 - residuals(p4_reg_model)
model_fitted_p4_reg_seasonality_trend<-model_fitted_reg_p4*
  (decomposed_p4_mult$seasonal*decomposed_p4_mult$trend)
ts.plot(deseas_p4, xlab = "Time Index", ylab = "Random Part",main="Sold product4")
points(model_fitted_reg_p4, type = "l", col = 2, lty = 2)
ts.plot(product4_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product4")
points(model_fitted_p4_reg_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p4_reg_seasonality_trend - product4_ts)^2)) / length(product4_ts)
p4_reg_matrix_test_1 <- rep(p4_reg_matrix[nrow(p4_reg_matrix),1],2)
p4_reg_matrix_test_2 <- rep(p4_reg_matrix[nrow(p4_reg_matrix),2],2)
p4_reg_matrix_test <- cbind(p4_reg_matrix_test_1, p4_reg_matrix_test_2)
colnames(p4_reg_matrix_test) <- c("price", "visit_turnover_rate")
p4_forecast_reg<-forecast(p4_reg_model,h=2,xreg=p4_reg_matrix_test)$mean
p4_forecast_reg
start_seasonality_reg_p4 <- start(p4_forecast_reg)[2]

end_seasonality_reg_p4 <- end(p4_forecast_reg)[2]
if(end_seasonality_reg_p4 > start_seasonality_reg_p4){
  p4_forecast_reg<-
    p4_forecast_reg*last_trend*decomposed_p4_mult$figure[start_seasonality_reg_p4:end_seasonality_reg_p4]
}
if(start_seasonality_reg_p4 > end_seasonality_reg_p4){
  counter_reg <- c(start_seasonality_reg_p4:frequency_p4, 1:end_seasonality_reg_p4)
  p4_forecast_reg<- p4_forecast_reg*last_trend*decomposed_p4_mult$figure[counter_reg]
}
p4_forecast_reg
names(p4_forecast_reg) <- c("1 day ahead forecast", "2 day ahead forecast")
p4_forecast_reg

#
#product 5
#
product5 <- product5[146:length(product5$sold_count)]
product5_ts = ts(product5$sold_count,start = min(product5$event_date), freq= 1)
product5_ts = ts(product5$sold_count,start = min(product5$event_date))
ts.plot(product5_ts)
acf(product5_ts)
#choose lag=16

product5_ts = ts(product5$sold_count,start = min(product5$event_date),frequency = 16)
plot.ts(product5_ts)
product5_ts[65:69] = NA
product5_ts[65:69] = mean(product5_ts[55:79],na.rm=TRUE)
product5_ts[49] = mean(product5_ts[55:79],na.rm=TRUE)
plot.ts(product5_ts)
product5_ts_train <- product5_ts[0:((length(product5_ts)-10))]
product5_ts_train <- ts(product5_ts_train, frequency = 16)
decomposed_p5_add<-decompose(product5_ts_train,type="additive")
plot(decomposed_p5_add)
decomposed_p5_mult<-decompose(product5_ts_train,type="multiplicative")
plot(decomposed_p5_mult)
#choose additive

last_trend=decomposed_p5_add$trend[max(which(!is.na(decomposed_p5_add$trend)))]
last_trend
first_trend=decomposed_p5_add$trend[min(which(!is.na(decomposed_p5_add$trend)))]
first_trend
frequency_p5<-frequency(decomposed_p5_add$trend)
decomposed_p5_add$trend[1:(frequency_p5/2)]=first_trend
decomposed_p5_add$trend[(length(decomposed_p5_add$trend)-
                           (frequency_p5/2)+1):length(decomposed_p5_add$trend)]=last_trend
head(decomposed_p5_add$trend,20)
tail(decomposed_p5_add$trend,20)
detrend_p5<- product5_ts_train - decomposed_p5_add$trend
deseas_p5<-detrend_p5 - decomposed_p5_add$seasonal
arima_model_p5<-auto.arima(deseas_p5,seasonal = F)
summary(arima_model_p5)

model_fitted_p5 <- deseas_p5 - residuals(arima_model_p5)
model_fitted_p5_seasonality_trend<-model_fitted_p5+
  (decomposed_p5_add$seasonal+decomposed_p5_add$trend)
ts.plot(deseas_p5, xlab = "Time Index", ylab = "Random Part",main="Sold product5")
points(model_fitted_p5, type = "l", col = 2, lty = 2)
ts.plot(product5_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product5")
p5_forecast_arima<-predict(arima_model_p5,n.ahead=10)$pred
# Finding seasonality factors for additive
start_seasonality_p5 <- (length(product5_ts)+1) %% frequency_p5
end_seasonality_p5 <- (length(product5_ts)+2) %% frequency_p5
p5_forecast_arima <- p5_forecast_arima + last_trend
+decomposed_p5_add$figure[start_seasonality_p5:end_seasonality_p5]
p5_forecast_arima

#arima_reg model
p5_visit_ts <- ts(product5$visit_turnover_rate, start= min(product5$event_date))
p5_visit_ts[(p5_visit_ts) ==0]<- NA
plot(p5_visit_ts)
Box.test(p5_visit_ts)
p5_price_ts <- ts(product5$price, start= min(product5$event_date))
ts.plot(p5_price_ts)
p5_price_ts[(p5_price_ts) ==-1]<- NA
plot(p5_price_ts)
p5_price_reg_matrix <- matrix(p5_price_ts[1:(length(p5_price_ts)-10)])
p5_visit_reg_matrix <- matrix(p5_visit_ts[1:(length(p5_visit_ts)-10)])
p5_reg_matrix <- cbind(p5_price_reg_matrix, p5_visit_reg_matrix)
colnames(p5_reg_matrix) <- c("price", "visit_turnover_rate")
p5_reg_model <- auto.arima(deseas_p5, xreg = p5_reg_matrix, seasonal = F)
summary(p5_reg_model)
model_fitted_reg_p5 <- deseas_p5 - residuals(p5_reg_model)
model_fitted_p5_reg_seasonality_trend<-model_fitted_reg_p5+
  (decomposed_p5_add$seasonal+decomposed_p5_add$trend)
ts.plot(deseas_p5, xlab = "Time Index", ylab = "Random Part",main="Sold Product5")
points(model_fitted_reg_p5, type = "l", col = 2, lty = 2)
ts.plot(product5_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product5")
points(model_fitted_p5_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p5_reg_matrix_test_1 <- rep(p5_reg_matrix[nrow(p5_reg_matrix),1],10)
p5_reg_matrix_test_2 <- rep(p5_reg_matrix[nrow(p5_reg_matrix),2],10)
p5_reg_matrix_test <- cbind(p5_reg_matrix_test_1, p5_reg_matrix_test_2)
colnames(p5_reg_matrix_test) <- c("price", "visit_turnover_rate")
p5_forecast_reg<-forecast(p5_reg_model,h=10,xreg=p5_reg_matrix_test)$mean
p5_forecast_reg
start_seasonality_reg_p5 <- (length(product5_ts_train)+1) %% frequency_p5
end_seasonality_reg_p5 <- (length(product5_ts_train)+2) %% frequency_p5
if(end_seasonality_reg_p5 > start_seasonality_reg_p5){
  p5_forecast_reg<-
    p5_forecast_reg+last_trend+decomposed_p5_add$figure[start_seasonality_reg_p5:end_seasonality_reg_p5]
}
if(start_seasonality_reg_p5 > end_seasonality_reg_p5){
  counter_reg <- c(start_seasonality_reg_p5:frequency_p5, 1:end_seasonality_reg_p5)
  p5_forecast_reg<- p5_forecast_reg+last_trend+decomposed_p5_add$figure[counter_reg]
}
p5_forecast_reg

#etsmodel
p5_ets_model <- ets(deseas_p5)
autoplot(p5_ets_model)
summary(p5_ets_model)
residuals(p5_ets_model)
model_fitted_ets_p5 <- p5_ets_model$fitted
model_fitted_p5_ets_seasonality_trend<-model_fitted_ets_p5+
  (decomposed_p5_add$seasonal+decomposed_p5_add$trend)
accuracy(p5_ets_model)
p5_forecast_ets<-predict(p5_ets_model,h=10)$mean
p5_forecast_ets
start_seasonality_ets_p5 <- (length(product5_ts_train)+1) %% frequency_p5
end_seasonality_ets_p5 <- (length(product5_ts_train)+2) %% frequency_p5
if(end_seasonality_ets_p5 > start_seasonality_ets_p5){
  p5_forecast_ets<-
    p5_forecast_ets+last_trend+decomposed_p5_add$figure[start_seasonality_ets_p5:end_seasonality_ets_p5]
}
if(start_seasonality_ets_p5 > end_seasonality_ets_p5){
  counter_ets <- c(start_seasonality_ets_p5:frequency_p5, 1:end_seasonality_ets_p5)
  p5_forecast_ets<- p5_forecast_ets+last_trend+decomposed_p5_add$figure[counter_ets]
}
p5_forecast_ets
#naivemodel
p5_naive_model <- naive(deseas_p5,h=10)
autoplot(p5_naive_model)
summary(p5_naive_model)
residuals(p5_naive_model)
model_fitted_naive_p5 <- p5_naive_model$fitted
model_fitted_p5_naive_seasonality_trend<-model_fitted_naive_p5+
  (decomposed_p5_add$seasonal+decomposed_p5_add$trend)
accuracy(p5_naive_model)
p5_forecast_naive<-predict(p5_naive_model,h=10)$mean
p5_forecast_naive
start_seasonality_naive_p5 <- (length(product5_ts_train)+1) %% frequency_p5
end_seasonality_naive_p5 <- (length(product5_ts_train)+2) %% frequency_p5
if(end_seasonality_naive_p5 > start_seasonality_naive_p5){
  p5_forecast_naive<-
    p5_forecast_naive+last_trend+decomposed_p5_add$figure[start_seasonality_naive_p5:end_seasonality_naive_p5]
}
if(start_seasonality_naive_p5 > end_seasonality_naive_p5){
  counter_naive <- c(start_seasonality_naive_p5:frequency_p5, 1:end_seasonality_naive_p5)
  p5_forecast_naive<- p5_forecast_naive+last_trend+decomposed_p5_add$figure[counter_naive]
}
p5_forecast_naive
#------------------------------product5 matrix----------------------------
product5_valid_matrix <- data.frame(list(p5_arima = rep(0,10),p5_arima_reg = rep(0,10), p5_ets = rep(0,10),
                                         p5_naive = rep(0,10), p5_test =
                                           rep(0,10), p5_arima_error = rep(0,10), p5_reg_error = rep(0,10),p5_ets_error =
                                           rep(0,10),p5_naive_error = rep(0,10) ))
product5_valid_matrix$p5_arima <- p5_forecast_arima
product5_valid_matrix$p5_test <- product5_ts[(length(product5_ts)-9):(length(product5_ts))]
product5_valid_matrix$p5_arima_error <- product5_valid_matrix$p5_test-product5_valid_matrix$p5_arima
product5_valid_matrix$p5_arima_reg <-p5_forecast_reg
product5_valid_matrix$p5_reg_error<-product5_valid_matrix$p5_test-product5_valid_matrix$p5_arima_reg

product5_valid_matrix$p5_ets <-p5_forecast_ets
product5_valid_matrix$p5_ets_error<-product5_valid_matrix$p5_test-product5_valid_matrix$p5_ets
product5_valid_matrix$p5_naive <-p5_forecast_naive
product5_valid_matrix$p5_naive_error<-product5_valid_matrix$p5_test-product5_valid_matrix$p5_naive
mape_test_arima<-100*mean(abs(product5_valid_matrix$p5_arima_error)/product5_valid_matrix$p5_test)
mape_test_reg<- 100*mean(abs(product5_valid_matrix$p5_reg_error)/product5_valid_matrix$p5_test)
mape_test_ets<- 100*mean(abs(product5_valid_matrix$p5_ets_error)/product5_valid_matrix$p5_test)
mape_test_naive<- 100*mean(abs(product5_valid_matrix$p5_naive_error)/product5_valid_matrix$p5_test)
mape_test_arima
mape_test_reg
mape_test_ets
mape_test_naive
#!!!!!choose arima with reg!!!!!!
decomposed_p5_add<-decompose(product5_ts,type="additive")
last_trend=decomposed_p5_add$trend[max(which(!is.na(decomposed_p5_add$trend)))]
first_trend=decomposed_p5_add$trend[min(which(!is.na(decomposed_p5_add$trend)))]
frequency_p5<-frequency(decomposed_p5_add$trend)
decomposed_p5_add$trend[1:(frequency_p5/2)]=first_trend
decomposed_p5_add$trend[(length(decomposed_p5_add$trend)-
                           (frequency_p5/2)+1):length(decomposed_p5_add$trend)]=last_trend
detrend_p5<- product5_ts - decomposed_p5_add$trend
deseas_p5<-detrend_p5 - decomposed_p5_add$seasonal
p5_price_reg_matrix <- matrix(p5_price_ts[1:length(p5_price_ts)])
p5_visit_reg_matrix <- matrix(p5_visit_ts[1:length(p5_visit_ts)])
p5_reg_matrix <- cbind(p5_price_reg_matrix, p5_visit_reg_matrix)
colnames(p5_reg_matrix) <- c("price", "visit_turnover_rate")
p5_reg_model <- auto.arima(deseas_p5, xreg = p5_reg_matrix, seasonal = F)
model_fitted_reg_p5 <- deseas_p5 - residuals(p5_reg_model)
model_fitted_p5_reg_seasonality_trend<-model_fitted_reg_p5+
  (decomposed_p5_add$seasonal+decomposed_p5_add$trend)
ts.plot(deseas_p5, xlab = "Time Index", ylab = "Random Part",main="Sold product5")
points(model_fitted_reg_p5, type = "l", col = 2, lty = 2)
ts.plot(product5_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product5")
points(model_fitted_p5_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p5_reg_matrix_test_1 <- rep(p5_reg_matrix[nrow(p5_reg_matrix),1],2)
p5_reg_matrix_test_2 <- rep(p5_reg_matrix[nrow(p5_reg_matrix),2],2)
p5_reg_matrix_test <- cbind(p5_reg_matrix_test_1, p5_reg_matrix_test_2)
colnames(p5_reg_matrix_test) <- c("price", "visit_turnover_rate")
p5_forecast_reg<-forecast(p5_reg_model,h=2,xreg=p5_reg_matrix_test)$mean
p5_forecast_reg
start_seasonality_reg_p5 <- (length(product5_ts)+1) %% frequency_p5
end_seasonality_reg_p5 <- (length(product5_ts)+2) %% frequency_p5
if(end_seasonality_reg_p5 > start_seasonality_reg_p5){
  p5_forecast_reg<-
    p5_forecast_reg+last_trend+decomposed_p5_add$figure[start_seasonality_reg_p5:end_seasonality_reg_p5]
}
if(start_seasonality_reg_p5 > end_seasonality_reg_p5){
  counter_reg <- c(start_seasonality_reg_p5:frequency_p5, 1:end_seasonality_reg_p5)
  p5_forecast_reg<- p5_forecast_reg+last_trend+decomposed_p5_add$figure[counter_reg]
}
p5_forecast_reg
names(p5_forecast_reg) <- c("1 day ahead forecast", "2 day ahead forecast")
p5_forecast_reg

#
#product 6
#
product6 <- product6[208:length(product6$sold_count)]
product6_ts = ts(product6$sold_count,start = min(product6$event_date), freq= 1)
product6_ts = ts(product6$sold_count,start = min(product6$event_date))
ts.plot(product6_ts)
acf(product6_ts)
#choose lag=8
product6_ts = ts(product6$sold_count,start = min(product6$event_date),frequency = 8)
plot.ts(product6_ts)
product6_ts[109:111] = NA
product6_ts[109:111] = mean(product6_ts,na.rm=TRUE)
plot.ts(product6_ts)
product6_ts_train <- product6_ts[0:(length(product6_ts)-10)]
product6_ts_train <- ts(product6_ts_train, frequency = 8)
decomposed_p6_add<-decompose(product6_ts_train,type="additive")
plot(decomposed_p6_add)
decomposed_p6_mult<-decompose(product6_ts_train,type="multiplicative")
plot(decomposed_p6_mult)
#choose multiplicative
last_trend=decomposed_p6_mult$trend[max(which(!is.na(decomposed_p6_mult$trend)))]
last_trend
first_trend=decomposed_p6_mult$trend[min(which(!is.na(decomposed_p6_mult$trend)))]
first_trend
frequency_p6<-frequency(decomposed_p6_mult$trend)
decomposed_p6_mult$trend[1:(frequency_p6/2)]=first_trend
decomposed_p6_mult$trend[(length(decomposed_p6_mult$trend)-
                            (frequency_p6/2)+1):length(decomposed_p6_mult$trend)]=last_trend
head(decomposed_p6_mult$trend,20)
tail(decomposed_p6_mult$trend,20)
detrend_p6<- product6_ts_train / decomposed_p6_mult$trend
deseas_p6<-detrend_p6 / decomposed_p6_mult$seasonal
arima_model_p6<-auto.arima(deseas_p6,seasonal = F)
summary(arima_model_p6)
model_fitted_p6 <- deseas_p6 - residuals(arima_model_p6)
model_fitted_p6_seasonality_trend<-model_fitted_p6*
  (decomposed_p6_mult$seasonal*decomposed_p6_mult$trend)
ts.plot(deseas_p6, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_p6, type = "l", col = 2, lty = 2)
ts.plot(product6_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_p6_seasonality_trend, type = "l", col = 2, lty = 2)
p6_forecast_arima<-predict(arima_model_p6,n.ahead=10)$pred
p6_forecast_arima
start_seasonality_p6 <- start(p6_forecast_arima)[2]
end_seasonality_p6 <- end(p6_forecast_arima)[2]
if(end_seasonality_p6 > start_seasonality_p6){
  p6_forecast_arima<-
    p6_forecast_arima*last_trend*decomposed_p6_mult$figure[start_seasonality_p6:end_seasonality_p6]
}
if(start_seasonality_p6 > end_seasonality_p6){
  counter <- c(start_seasonality_p6:16, 1:end_seasonality_p6)
  p6_forecast_arima<- p6_forecast_arima*last_trend*decomposed_p6_mult$figure[counter]
}
p6_forecast_arima
#arima_reg model
p6_visit_ts <- ts(product6$visit_turnover_rate, start= min(product6$event_date))
min(p6_visit_ts)
p6_visit_ts[p6_visit_ts == 0]<-NA
plot(p6_visit_ts)
p6_price_ts <- ts(product6$price, start= min(product6$event_date))
ts.plot(p6_price_ts)
p6_price_ts[p6_price_ts == -1]<-NA
plot(p6_price_ts)
p6_price_ts[which(p6_price_ts == min(p6_price_ts))] <- NA
p6_price_reg_matrix <- matrix(p6_price_ts[1:(length(p6_price_ts)-10)])
p6_visit_reg_matrix <- matrix(p6_visit_ts[1:(length(p6_visit_ts)-10)])
p6_reg_matrix <- cbind(p6_price_reg_matrix, p6_visit_reg_matrix)
colnames(p6_reg_matrix) <- c("price", "visit_turnover_rate")
p6_reg_model <- auto.arima(deseas_p6, xreg = p6_reg_matrix, seasonal = F)
summary(p6_reg_model)
model_fitted_reg_p6 <- deseas_p6 - residuals(p6_reg_model)
model_fitted_p6_reg_seasonality_trend<-model_fitted_reg_p6*
  (decomposed_p6_mult$seasonal*decomposed_p6_mult$trend)
ts.plot(deseas_p6, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_reg_p6, type = "l", col = 2, lty = 2)
ts.plot(product6_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_p6_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p6_reg_matrix_test_1 <- rep(p6_reg_matrix[nrow(p6_reg_matrix),1],10)
p6_reg_matrix_test_2 <- rep(p6_reg_matrix[nrow(p6_reg_matrix),2],10)
p6_reg_matrix_test <- cbind(p6_reg_matrix_test_1, p6_reg_matrix_test_2)
colnames(p6_reg_matrix_test) <- c("price", "visit_turnover_rate")
p6_forecast_reg<-forecast(p6_reg_model,h=10,xreg=p6_reg_matrix_test)$mean
p6_forecast_reg
start_seasonality_reg_p6 <- start(p6_forecast_reg)[2]
end_seasonality_reg_p6 <- end(p6_forecast_reg)[2]
if(end_seasonality_reg_p6 > start_seasonality_reg_p6){
  p6_forecast_reg<-
    p6_forecast_reg*last_trend*decomposed_p6_mult$figure[start_seasonality_reg_p6:end_seasonality_reg_p6]
}
if(start_seasonality_reg_p6 > end_seasonality_reg_p6){
  counter_reg <- c(start_seasonality_reg_p6:16, 1:end_seasonality_reg_p6)
  p6_forecast_reg<- p6_forecast_reg*last_trend*decomposed_p6_mult$figure[counter_reg]
}
p6_forecast_reg
#etsmodel
p6_ets_model <- ets(deseas_p6)
autoplot(p6_ets_model)
summary(p6_ets_model)
residuals(p6_ets_model)
model_fitted_ets_p6 <- p6_ets_model$fitted
model_fitted_p6_ets_seasonality_trend<-model_fitted_ets_p6*
  (decomposed_p6_mult$seasonal*decomposed_p6_mult$trend)
accuracy(p6_ets_model)
p6_forecast_ets<-predict(p6_ets_model,h=10)$mean
p6_forecast_ets
start_seasonality_ets_p6 <- start(p6_forecast_ets)[2]
end_seasonality_ets_p6 <- end(p6_forecast_ets)[2]
if(end_seasonality_ets_p6 > start_seasonality_ets_p6){
  p6_forecast_ets<-
    p6_forecast_ets*last_trend*decomposed_p6_mult$figure[start_seasonality_ets_p6:end_seasonality_ets_p6]
}
if(start_seasonality_ets_p6 > end_seasonality_ets_p6){
  counter_ets <- c(start_seasonality_ets_p6:16, 1:end_seasonality_ets_p6)
  p6_forecast_ets<- p6_forecast_ets*last_trend*decomposed_p6_mult$figure[counter_ets]
}
p6_forecast_ets
#naivemodel
p6_naive_model <- naive(deseas_p6,h=10)
autoplot(p6_naive_model)
summary(p6_naive_model)
residuals(p6_naive_model)
model_fitted_naive_p6 <- p6_naive_model$fitted
model_fitted_p6_naive_seasonality_trend<-model_fitted_naive_p6*
  (decomposed_p6_mult$seasonal*decomposed_p6_mult$trend)
accuracy(p6_naive_model)
p6_forecast_naive<-predict(p6_naive_model,h=10)$mean
p6_forecast_naive
start_seasonality_naive_p6 <- start(p6_forecast_naive)[2]
end_seasonality_naive_p6 <- end(p6_forecast_naive)[2]
if(end_seasonality_naive_p6 > start_seasonality_naive_p6){
  p6_forecast_naive<-
    
    p6_forecast_naive*last_trend*decomposed_p6_mult$figure[start_seasonality_naive_p6:end_seasonality_naive_p6]
}
if(start_seasonality_naive_p6 > end_seasonality_naive_p6){
  counter_naive <- c(start_seasonality_naive_p6:16, 1:end_seasonality_naive_p6)
  p6_forecast_naive<- p6_forecast_naive*last_trend*decomposed_p6_mult$figure[counter_naive]
}
p6_forecast_naive
#------------------------------product6 matrix----------------------------
product6_valid_matrix <- data.frame(list(p6_arima = rep(0,10),p6_arima_reg = rep(0,10), p6_ets = rep(0,10),
                                         p6_naive = rep(0,10), p6_test =
                                           rep(0,10), p6_arima_error = rep(0,10), p6_reg_error = rep(0,10),p6_ets_error =
                                           rep(0,10),p6_naive_error = rep(0,10) ))
product6_valid_matrix$p6_arima <- p6_forecast_arima
product6_valid_matrix$p6_test <- product6_ts[(length(product6_ts)-9):(length(product6_ts))]
product6_valid_matrix$p6_arima_error <- product6_valid_matrix$p6_test-product6_valid_matrix$p6_arima
product6_valid_matrix$p6_arima_reg <-p6_forecast_reg
product6_valid_matrix$p6_reg_error<-product6_valid_matrix$p6_test-product6_valid_matrix$p6_arima_reg
product6_valid_matrix$p6_ets <-p6_forecast_ets
product6_valid_matrix$p6_ets_error<-product6_valid_matrix$p6_test-product6_valid_matrix$p6_ets
product6_valid_matrix$p6_naive <-p6_forecast_naive
product6_valid_matrix$p6_naive_error<-product6_valid_matrix$p6_test-product6_valid_matrix$p6_naive
mape_test_arima<-100*mean(abs(product6_valid_matrix$p6_arima_error)/product6_valid_matrix$p6_test)
mape_test_reg<- 100*mean(abs(product6_valid_matrix$p6_reg_error)/product6_valid_matrix$p6_test)
mape_test_ets<- 100*mean(abs(product6_valid_matrix$p6_ets_error)/product6_valid_matrix$p6_test)
mape_test_naive<- 100*mean(abs(product6_valid_matrix$p6_naive_error)/product6_valid_matrix$p6_test)
mape_test_arima
mape_test_reg
mape_test_ets
mape_test_naive
product6_valid_matrix
#!!!!!choose arima with reg!!!!!!
decomposed_p6_mult<-decompose(product6_ts,type="multiplicative")
last_trend=decomposed_p6_mult$trend[max(which(!is.na(decomposed_p6_mult$trend)))]
last_trend
first_trend=decomposed_p6_mult$trend[min(which(!is.na(decomposed_p6_mult$trend)))]
first_trend
frequency_p6<-frequency(decomposed_p6_mult$trend)
decomposed_p6_mult$trend[1:(frequency_p6/2)]=first_trend
decomposed_p6_mult$trend[(length(decomposed_p6_mult$trend)-
                            (frequency_p6/2)+1):length(decomposed_p6_mult$trend)]=last_trend
detrend_p6<- product6_ts / decomposed_p6_mult$trend
deseas_p6<-detrend_p6 / decomposed_p6_mult$seasonal
p6_price_reg_matrix <- matrix(p6_price_ts[1:length(p6_price_ts)])
p6_visit_reg_matrix <- matrix(p6_visit_ts[1:length(p6_visit_ts)])
p6_reg_matrix <- cbind(p6_price_reg_matrix, p6_visit_reg_matrix)
colnames(p6_reg_matrix) <- c("price", "visit_turnover_rate")
p6_reg_model <- auto.arima(deseas_p6, xreg = p6_reg_matrix, seasonal = F)
model_fitted_reg_p6 <- deseas_p6 - residuals(p6_reg_model)
model_fitted_p6_reg_seasonality_trend<-model_fitted_reg_p6*
  (decomposed_p6_mult$seasonal*decomposed_p6_mult$trend)
ts.plot(deseas_p6, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_reg_p6, type = "l", col = 2, lty = 2)
ts.plot(product6_ts, xlab = "Time Index", ylab = "Random Part",main="Sold Product6")
points(model_fitted_p6_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p6_reg_matrix_test_1 <- rep(p6_reg_matrix[nrow(p6_reg_matrix),1],2)
p6_reg_matrix_test_2 <- rep(p6_reg_matrix[nrow(p6_reg_matrix),2],2)
p6_reg_matrix_test <- cbind(p6_reg_matrix_test_1, p6_reg_matrix_test_2)
colnames(p6_reg_matrix_test) <- c("price", "visit_turnover_rate")
p6_forecast_reg<-forecast(p6_reg_model,h=2,xreg=p6_reg_matrix_test)$mean
p6_forecast_reg
start_seasonality_reg_p6 <- start(p6_forecast_reg)[2]

end_seasonality_reg_p6 <- end(p6_forecast_reg)[2]
if(end_seasonality_reg_p6 > start_seasonality_reg_p6){
  p6_forecast_reg<-
    p6_forecast_reg*last_trend*decomposed_p6_mult$figure[start_seasonality_reg_p6:end_seasonality_reg_p6]
}
if(start_seasonality_reg_p6 > end_seasonality_reg_p6){
  counter_reg <- c(start_seasonality_reg_p6:frequency_p6, 1:end_seasonality_reg_p6)
  p6_forecast_reg<- p6_forecast_reg*last_trend*decomposed_p6_mult$figure[counter_reg]
}
p6_forecast_reg
names(p6_forecast_reg) <- c("1 day ahead forecast", "2 day ahead forecast")
p6_forecast_reg

#
#product 7
#
product7_ts = ts(product7$sold_count,start = min(product7$event_date), freq= 1)
product7_ts = ts(product7$sold_count,start = min(product7$event_date))
ts.plot(product7_ts)
acf(product7_ts)
#choose lag=6
product7_ts = ts(product7$sold_count,start = min(product7$event_date),frequency = 6)
plot(product7_ts)
plot.ts(product7_ts)
# product7_ts[190:220] = NA
product7_ts[50] = mean(product7_ts,na.rm=TRUE)
product7_ts[99] = mean(product7_ts,na.rm=TRUE)
plot.ts(product7_ts)
product7_ts_train <- product7_ts[0:((length(product7_ts)-10))]
product7_ts_train <- ts(product7_ts_train, frequency = 6)
decomposed_p7_add<-decompose(product7_ts_train,type="additive")
plot(decomposed_p7_add)
decomposed_p7_mult<-decompose(product7_ts_train,type="multiplicative")
plot(decomposed_p7_mult)
#choose additive
last_trend=decomposed_p7_add$trend[max(which(!is.na(decomposed_p7_add$trend)))]
last_trend
first_trend=decomposed_p7_add$trend[min(which(!is.na(decomposed_p7_add$trend)))]
first_trend
frequency_p7<-frequency(decomposed_p7_add$trend)
decomposed_p7_add$trend[1:(frequency_p7/2)]=first_trend
decomposed_p7_add$trend[(length(decomposed_p7_add$trend)-
                           (frequency_p7/2)+1):length(decomposed_p7_add$trend)]=last_trend
head(decomposed_p7_add$trend,20)
tail(decomposed_p7_add$trend,20)
detrend_p7<- product7_ts_train - decomposed_p7_add$trend
deseas_p7<-detrend_p7 - decomposed_p7_add$seasonal
arima_model_p7<-auto.arima(deseas_p7,seasonal = F)
summary(arima_model_p7)

model_fitted_p7 <- deseas_p7 - residuals(arima_model_p7)
model_fitted_p7_seasonality_trend<-model_fitted_p7+
  (decomposed_p7_add$seasonal+decomposed_p7_add$trend)
ts.plot(deseas_p7, xlab = "Time Index", ylab = "Random Part",main="Sold product7")
points(model_fitted_p7, type = "l", col = 2, lty = 2)
ts.plot(product7_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product7")
points(model_fitted_p7_seasonality_trend, type = "l", col = 2, lty = 2)
p7_forecast_arima<-predict(arima_model_p7,n.ahead=10)$pred
# Finding seasonality factors for additive
start_seasonality_p7 <- (length(product7_ts)+1) %% frequency_p7
end_seasonality_p7 <- (length(product7_ts)+2) %% frequency_p7
p7_forecast_arima <- p7_forecast_arima + last_trend
+decomposed_p7_add$figure[start_seasonality_p7:end_seasonality_p7]
p7_forecast_arima
#arima_reg model
p7_visit_ts <- ts(product7$visit_turnover_rate, start= min(product7$event_date))
plot(p7_visit_ts)
Box.test(p7_visit_ts)
p7_price_ts <- ts(product7$price, start= min(product7$event_date))
ts.plot(p7_price_ts)
p7_price_ts[p7_price_ts == -1] <- NA
plot(p7_price_ts)
#Price is not used in regression model since there are lots of NA's
p7_visit_reg_matrix <- matrix(p7_visit_ts[1:(length(p7_visit_ts)-10)])
p7_reg_matrix <- p7_visit_reg_matrix
names(p7_reg_matrix) <- "visit_turnover_rate"
p7_reg_model <- auto.arima(deseas_p7, xreg = p7_reg_matrix, seasonal = F)

summary(p7_reg_model)
model_fitted_reg_p7 <- deseas_p7 - residuals(p7_reg_model)
model_fitted_p7_reg_seasonality_trend<-model_fitted_reg_p7+
  (decomposed_p7_add$seasonal+decomposed_p7_add$trend)
ts.plot(deseas_p7, xlab = "Time Index", ylab = "Random Part",main="Sold Product7")
points(model_fitted_reg_p7, type = "l", col = 2, lty = 2)
ts.plot(product7_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product7")
points(model_fitted_p7_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p7_reg_matrix_test_2 <- rep(p7_reg_matrix[nrow(p7_reg_matrix)],10)
p7_reg_matrix_test <-  p7_reg_matrix_test_2
names(p7_reg_matrix_test) <- "visit_turnover_rate"
p7_forecast_reg<-forecast(p7_reg_model,h=10,xreg=p7_reg_matrix_test)$mean
p7_forecast_reg
start_seasonality_reg_p7 <- (length(product7_ts_train)+1) %% frequency_p7
end_seasonality_reg_p7 <- (length(product7_ts_train)+11) %% frequency_p7
if(end_seasonality_reg_p7 > start_seasonality_reg_p7){
  p7_forecast_reg<-
    p7_forecast_reg+last_trend+decomposed_p7_add$figure[start_seasonality_reg_p7:end_seasonality_reg_p7]
}
if(start_seasonality_reg_p7 > end_seasonality_reg_p7){
  counter_reg <- c(start_seasonality_reg_p7:frequency_p7, 1:end_seasonality_reg_p7)
  p7_forecast_reg<- p7_forecast_reg+last_trend+decomposed_p7_add$figure[counter_reg]
}
p7_forecast_reg

#etsmodel
p7_ets_model <- ets(deseas_p7)
autoplot(p7_ets_model)
summary(p7_ets_model)
residuals(p7_ets_model)
model_fitted_ets_p7 <- p7_ets_model$fitted
model_fitted_p7_ets_seasonality_trend<-model_fitted_ets_p7+
  (decomposed_p7_add$seasonal+decomposed_p7_add$trend)
accuracy(p7_ets_model)
p7_forecast_ets<-predict(p7_ets_model,h=10)$mean
p7_forecast_ets
start_seasonality_ets_p7 <- (length(product7_ts_train)+1) %% frequency_p7
end_seasonality_ets_p7 <- (length(product7_ts_train)+2) %% frequency_p7
if(end_seasonality_ets_p7 > start_seasonality_ets_p7){
  p7_forecast_ets<-
    p7_forecast_ets+last_trend+decomposed_p7_add$figure[start_seasonality_ets_p7:end_seasonality_ets_p7]
}
if(start_seasonality_ets_p7 > end_seasonality_ets_p7){
  counter_ets <- c(start_seasonality_ets_p7:frequency_p7, 1:end_seasonality_ets_p7)
  p7_forecast_ets<- p7_forecast_ets+last_trend+decomposed_p7_add$figure[counter_ets]
}
p7_forecast_ets
#naivemodel
p7_naive_model <- naive(deseas_p7,h=10)
autoplot(p7_naive_model)
summary(p7_naive_model)
residuals(p7_naive_model)
model_fitted_naive_p7 <- p7_naive_model$fitted
model_fitted_p7_naive_seasonality_trend<-model_fitted_naive_p7+
  (decomposed_p7_add$seasonal+decomposed_p7_add$trend)
accuracy(p7_naive_model)
p7_forecast_naive<-predict(p7_naive_model,h=10)$mean
p7_forecast_naive
start_seasonality_naive_p7 <- (length(product7_ts_train)+1) %% frequency_p7
end_seasonality_naive_p7 <- (length(product7_ts_train)+2) %% frequency_p7
if(end_seasonality_naive_p7 > start_seasonality_naive_p7){
  p7_forecast_naive<-
    p7_forecast_naive+last_trend+decomposed_p7_add$figure[start_seasonality_naive_p7:end_seasonality_naive_p7]
}
if(start_seasonality_naive_p7 > end_seasonality_naive_p7){
  counter_naive <- c(start_seasonality_naive_p7:frequency_p7, 1:end_seasonality_naive_p7)
  p7_forecast_naive<- p7_forecast_naive+last_trend+decomposed_p7_add$figure[counter_naive]
}
p7_forecast_naive
#------------------------------product7 matrix----------------------------
product7_valid_matrix <- data.frame(list(p7_arima = rep(0,10),p7_arima_reg = rep(0,10), p7_ets = rep(0,10),
                                         p7_naive = rep(0,10), p7_test =
                                           rep(0,10), p7_arima_error = rep(0,10), p7_reg_error = rep(0,10),p7_ets_error =
                                           rep(0,10),p7_naive_error = rep(0,10) ))
product7_valid_matrix$p7_arima <- p7_forecast_arima
product7_valid_matrix$p7_test <- product7_ts[(length(product7_ts)-9):(length(product7_ts))]
product7_valid_matrix$p7_arima_error <- product7_valid_matrix$p7_test-product7_valid_matrix$p7_arima
product7_valid_matrix$p7_arima_reg <-p7_forecast_reg
product7_valid_matrix$p7_reg_error<-product7_valid_matrix$p7_test-product7_valid_matrix$p7_arima_reg

product7_valid_matrix$p7_ets <-p7_forecast_ets
product7_valid_matrix$p7_ets_error<-product7_valid_matrix$p7_test-product7_valid_matrix$p7_ets
product7_valid_matrix$p7_naive <-p7_forecast_naive
product7_valid_matrix$p7_naive_error<-product7_valid_matrix$p7_test-product7_valid_matrix$p7_naive
mae_test_arima<-100*mean(abs(product7_valid_matrix$p7_arima_error))
mae_test_reg<- 100*mean(abs(product7_valid_matrix$p7_reg_error))
mae_test_ets<- 100*mean(abs(product7_valid_matrix$p7_ets_error))
mae_test_naive<- 100*mean(abs(product7_valid_matrix$p7_naive_error))
mae_test_arima
mae_test_reg
mae_test_ets
mae_test_naive

#!!!!!choose ets model!!!!!!
decomposed_p7_add<-decompose(product7_ts,type="additive")
last_trend=decomposed_p7_add$trend[max(which(!is.na(decomposed_p7_add$trend)))]
first_trend=decomposed_p7_add$trend[min(which(!is.na(decomposed_p7_add$trend)))]
frequency_p7<-frequency(decomposed_p7_add$trend)
decomposed_p7_add$trend[1:(frequency_p7/2)]=first_trend
decomposed_p7_add$trend[(length(decomposed_p7_add$trend)-
                           (frequency_p7/2)+1):length(decomposed_p7_add$trend)]=last_trend
detrend_p7<- product7_ts - decomposed_p7_add$trend
deseas_p7<-detrend_p7 - decomposed_p7_add$seasonal

p7_ets_model <- ets(deseas_p7)
autoplot(p7_ets_model)
summary(p7_ets_model)
residuals(p7_ets_model)
model_fitted_ets_p7 <- p7_ets_model$fitted
model_fitted_p7_ets_seasonality_trend<-model_fitted_ets_p7+
  (decomposed_p7_add$seasonal+decomposed_p7_add$trend)
accuracy(p7_ets_model)
p7_forecast_ets<-predict(p7_ets_model,h=2)$mean
p7_forecast_ets
start_seasonality_ets_p7 <- (length(product7_ts_train)+1) %% frequency_p7
end_seasonality_ets_p7 <- (length(product7_ts_train)+2) %% frequency_p7
if(end_seasonality_ets_p7 > start_seasonality_ets_p7){
  p7_forecast_ets<-
    p7_forecast_ets+last_trend+decomposed_p7_add$figure[start_seasonality_ets_p7:end_seasonality_ets_p7]
}
if(start_seasonality_ets_p7 > end_seasonality_ets_p7){
  counter_ets <- c(start_seasonality_ets_p7:frequency_p7, 1:end_seasonality_ets_p7)
  p7_forecast_ets<- p7_forecast_ets+last_trend+decomposed_p7_add$figure[counter_ets]
}
p7_forecast_ets

#
#product 8
#
product8_ts = ts(product8$sold_count,start = min(product8$event_date))
ts.plot(product8_ts)
acf(product8_ts, lag.max = 365)
#choose lag=21
product8_ts = ts(product8$sold_count,start = min(product8$event_date),frequency = 21)
plot.ts(product8_ts)
product8_ts[194] = mean(product8_ts)
plot.ts(product8_ts)
product8_ts_train <- product8_ts[0:((length(product8_ts)-10))]
product8_ts_train <- ts(product8_ts_train, frequency = 21)
decomposed_p8_add<-decompose(product8_ts_train,type="additive")
plot(decomposed_p8_add)
decomposed_p8_mult<-decompose(product8_ts_train,type="multiplicative")
plot(decomposed_p8_mult)
#choose additive
last_trend=decomposed_p8_add$trend[max(which(!is.na(decomposed_p8_add$trend)))]
last_trend
first_trend=decomposed_p8_add$trend[min(which(!is.na(decomposed_p8_add$trend)))]
first_trend
frequency_p8<-frequency(decomposed_p8_add$trend)
decomposed_p8_add$trend[1:(frequency_p8/2)]=first_trend
decomposed_p8_add$trend[(length(decomposed_p8_add$trend)-
                           (frequency_p8/2)+1):length(decomposed_p8_add$trend)]=last_trend
head(decomposed_p8_add$trend,20)
tail(decomposed_p8_add$trend,20)
detrend_p8<- product8_ts_train - decomposed_p8_add$trend
deseas_p8<-detrend_p8 - decomposed_p8_add$seasonal
arima_model_p8<-auto.arima(deseas_p8,seasonal = F)
summary(arima_model_p8)

model_fitted_p8 <- deseas_p8 - residuals(arima_model_p8)
model_fitted_p8_seasonality_trend<-model_fitted_p8+
  (decomposed_p8_add$seasonal+decomposed_p8_add$trend)
ts.plot(deseas_p8, xlab = "Time Index", ylab = "Random Part",main="Sold product8")
points(model_fitted_p8, type = "l", col = 2, lty = 2)
ts.plot(product8_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product8")
points(model_fitted_p8_seasonality_trend, type = "l", col = 2, lty = 2)
p8_forecast_arima<-predict(arima_model_p8,n.ahead=10)$pred
# Finding seasonality factors for additive
start_seasonality_p8 <- (length(product8_ts)+1) %% frequency_p8
end_seasonality_p8 <- (length(product8_ts)+2) %% frequency_p8
p8_forecast_arima <- p8_forecast_arima + last_trend
+decomposed_p8_add$figure[start_seasonality_p8:end_seasonality_p8]
p8_forecast_arima
#arima_reg model
p8_visit_ts <- ts(product8$visit_turnover_rate, start= min(product8$event_date))
which.max(p8_visit_ts)
p8_visit_ts[which.max(p8_visit_ts)] = mean(p8_visit_ts[13:23])
plot(p8_visit_ts)
Box.test(p8_visit_ts)
p8_price_ts <- ts(product8$price, start= min(product8$event_date))
ts.plot(p8_price_ts)
p8_price_ts[p8_price_ts == -1] <- NA
plot(p8_price_ts)

#Price is not used in regression model since there are lots of NA's
p8_visit_reg_matrix <- matrix(p8_visit_ts[1:(length(p8_visit_ts)-10)])
p8_reg_matrix <- p8_visit_reg_matrix
names(p8_reg_matrix) <- "visit_turnover_rate"
p8_reg_model <- auto.arima(deseas_p8, xreg = p8_reg_matrix, seasonal = F)
summary(p8_reg_model)
model_fitted_reg_p8 <- deseas_p8 - residuals(p8_reg_model)
model_fitted_p8_reg_seasonality_trend<-model_fitted_reg_p8+
  (decomposed_p8_add$seasonal+decomposed_p8_add$trend)
ts.plot(deseas_p8, xlab = "Time Index", ylab = "Random Part",main="Sold Product8")
points(model_fitted_reg_p8, type = "l", col = 2, lty = 2)
ts.plot(product8_ts_train, xlab = "Time Index", ylab = "Random Part",main="Sold Product8")
points(model_fitted_p8_reg_seasonality_trend, type = "l", col = 2, lty = 2)
p8_reg_matrix_test_2 <- rep(p8_reg_matrix[nrow(p8_reg_matrix)],10)
p8_reg_matrix_test <-  p8_reg_matrix_test_2
names(p8_reg_matrix_test) <- "visit_turnover_rate"
p8_forecast_reg<-forecast(p8_reg_model,h=10,xreg=p8_reg_matrix_test)$mean
p8_forecast_reg
start_seasonality_reg_p8 <- (length(product8_ts_train)+1) %% frequency_p8
end_seasonality_reg_p8 <- (length(product8_ts_train)+2) %% frequency_p8
if(end_seasonality_reg_p8 > start_seasonality_reg_p8){
  p8_forecast_reg<-
    p8_forecast_reg+last_trend+decomposed_p8_add$figure[start_seasonality_reg_p8:end_seasonality_reg_p8]
}
if(start_seasonality_reg_p8 > end_seasonality_reg_p8){
  counter_reg <- c(start_seasonality_reg_p8:frequency_p8, 1:end_seasonality_reg_p8)
  p8_forecast_reg<- p8_forecast_reg+last_trend+decomposed_p8_add$figure[counter_reg]
}
p8_forecast_reg


#etsmodel
p8_ets_model <- ets(deseas_p8)
autoplot(p8_ets_model)
summary(p8_ets_model)
residuals(p8_ets_model)
model_fitted_ets_p8 <- p8_ets_model$fitted
model_fitted_p8_ets_seasonality_trend<-model_fitted_ets_p8+
  (decomposed_p8_add$seasonal+decomposed_p8_add$trend)
accuracy(p8_ets_model)
p8_forecast_ets<-predict(p8_ets_model,h=10)$mean
p8_forecast_ets
start_seasonality_ets_p8 <- (length(product8_ts_train)+1) %% frequency_p8
end_seasonality_ets_p8 <- (length(product8_ts_train)+2) %% frequency_p8
if(end_seasonality_ets_p8 > start_seasonality_ets_p8){
  p8_forecast_ets<-
    p8_forecast_ets+last_trend+decomposed_p8_add$figure[start_seasonality_ets_p8:end_seasonality_ets_p8]
}
if(start_seasonality_ets_p8 > end_seasonality_ets_p8){
  counter_ets <- c(start_seasonality_ets_p8:frequency_p8, 1:end_seasonality_ets_p8)
  p8_forecast_ets<- p8_forecast_ets+last_trend+decomposed_p8_add$figure[counter_ets]
}
p8_forecast_ets
#naivemodel
p8_naive_model <- naive(deseas_p8,h=10)
autoplot(p8_naive_model)
summary(p8_naive_model)
residuals(p8_naive_model)
model_fitted_naive_p8 <- p8_naive_model$fitted
model_fitted_p8_naive_seasonality_trend<-model_fitted_naive_p8+
  (decomposed_p8_add$seasonal+decomposed_p8_add$trend)
accuracy(p8_naive_model)
p8_forecast_naive<-predict(p8_naive_model,h=10)$mean
p8_forecast_naive
start_seasonality_naive_p8 <- (length(product8_ts_train)+1) %% frequency_p8
end_seasonality_naive_p8 <- (length(product8_ts_train)+2) %% frequency_p8
if(end_seasonality_naive_p8 > start_seasonality_naive_p8){
  p8_forecast_naive<-
    p8_forecast_naive+last_trend+decomposed_p8_add$figure[start_seasonality_naive_p8:end_seasonality_naive_p8]
}
if(start_seasonality_naive_p8 > end_seasonality_naive_p8){
  counter_naive <- c(start_seasonality_naive_p8:frequency_p8, 1:end_seasonality_naive_p8)
  p8_forecast_naive<- p8_forecast_naive+last_trend+decomposed_p8_add$figure[counter_naive]
}
p8_forecast_naive
#------------------------------product8 matrix----------------------------
product8_valid_matrix <- data.frame(list(p8_arima = rep(0,10),p8_arima_reg = rep(0,10), p8_ets = rep(0,10),
                                         p8_naive = rep(0,10), p8_test =
                                           rep(0,10), p8_arima_error = rep(0,10), p8_reg_error = rep(0,10),p8_ets_error =
                                           rep(0,10),p8_naive_error = rep(0,10) ))
product8_valid_matrix$p8_arima <- p8_forecast_arima
product8_valid_matrix$p8_test <- product8_ts[(length(product8_ts)-9):(length(product8_ts))]
product8_valid_matrix$p8_arima_error <- product8_valid_matrix$p8_test-product8_valid_matrix$p8_arima
product8_valid_matrix$p8_arima_reg <-p8_forecast_reg
product8_valid_matrix$p8_reg_error<-product8_valid_matrix$p8_test-product8_valid_matrix$p8_arima_reg

product8_valid_matrix$p8_ets <-p8_forecast_ets
product8_valid_matrix$p8_ets_error<-product8_valid_matrix$p8_test-product8_valid_matrix$p8_ets
product8_valid_matrix$p8_naive <-p8_forecast_naive
product8_valid_matrix$p8_naive_error<-product8_valid_matrix$p8_test-product8_valid_matrix$p8_naive
mae_test_arima<-100*mean(abs(product8_valid_matrix$p8_arima_error))
mae_test_reg<- 100*mean(abs(product8_valid_matrix$p8_reg_error))
mae_test_ets<- 100*mean(abs(product8_valid_matrix$p8_ets_error))
mae_test_naive<- 100*mean(abs(product8_valid_matrix$p8_naive_error))
mae_test_arima
mae_test_reg
mae_test_ets
mae_test_naive
#!!!!!choose arima!!!!!!
decomposed_p8_add<-decompose(product8_ts,type="additive")
plot(decomposed_p8_add)
decomposed_p8_mult<-decompose(product8_ts,type="multiplicative")
plot(decomposed_p8_mult)
#choose additive
last_trend=decomposed_p8_add$trend[max(which(!is.na(decomposed_p8_add$trend)))]
first_trend=decomposed_p8_add$trend[min(which(!is.na(decomposed_p8_add$trend)))]
frequency_p8<-frequency(decomposed_p8_add$trend)
decomposed_p8_add$trend[1:(frequency_p8/2)]=first_trend
decomposed_p8_add$trend[(length(decomposed_p8_add$trend)-
                           (ceiling(frequency_p8/2))+1):length(decomposed_p8_add$trend)]=last_trend
head(decomposed_p8_add$trend,20)
tail(decomposed_p8_add$trend,20)
detrend_p8<- product8_ts - decomposed_p8_add$trend
deseas_p8<-detrend_p8 - decomposed_p8_add$seasonal
arima_model_p8<-auto.arima(deseas_p8,seasonal = F)
summary(arima_model_p8)
model_fitted_p8 <- deseas_p8 - residuals(arima_model_p8)
model_fitted_p8_seasonality_trend<-model_fitted_p8+
  (decomposed_p8_add$seasonal+decomposed_p8_add$trend)
ts.plot(deseas_p8, xlab = "Time Index", ylab = "Random Part",main="Sold product8")
points(model_fitted_p8, type = "l", col = 2, lty = 2)
ts.plot(product8_ts, xlab = "Time Index", ylab = "Random Part",main="Sold product8")
points(model_fitted_p8_seasonality_trend, type = "l", col = 2, lty = 2)
sqrt(mean((model_fitted_p8_seasonality_trend - product8_ts)^2)) / length(product8_ts)
p8_forecast_arima<-predict(arima_model_p8,n.ahead=2)$pred
# Finding seasonality factors for additive
start_seasonality_p8 <- (length(product8_ts)+1) %% frequency_p8
end_seasonality_p8 <- (length(product8_ts)+2) %% frequency_p8
p8_forecast_arima <- p8_forecast_arima + last_trend +decomposed_p8_add$figure[start_seasonality_p8:end_seasonality_p8]
p8_forecast_arima

#
# Assigning correct id's
#
predictions[1,2] <-max(0,p5_forecast_reg[2])
predictions[2,2] <-max(0,p6_forecast_reg[2])
predictions[3,2] <-max(0, p8_forecast_arima[2])
predictions[4,2] <- max(0,p2_forecast_arima[2])
predictions[5,2] <-max(0,p7_forecast_ets[2])
predictions[6,2] <- max(0,p3_forecast_reg[2])
predictions[7,2] <- max(0,p4_forecast_reg[2])
predictions[8,2] <- max(0,p1_forecast_reg[2])
predictions
#submission
send_submission(predictions, token, url=subm_url, submit_now=F)
