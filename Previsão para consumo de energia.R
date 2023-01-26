

######################################################################
### Previsão da do consumo de energia com modelo ARIMA - dados BCB ###
######################################################################

#Definindo diretório
setwd('C:/Users/Pedro/Documents/Processo-seletivo/Publicações do Linkedin/Previsão eletricidade/Modelo-ARIMA-Consumo-de-Energia')
getwd()


## Pacotes utilizados ##
install.packages("GetBCBData")
install.packages('ipeadatar')


library("GetBCBData")
library('dplyr')
library('urca')
library('forecast')
library('readxl')
library('lmtest')
library(ggplot2)
library(TSstudio)
library(seasonal)




######################################################################################################
### Puxando as séries de consumo de energia no site do Banco Central do Brasil e fazendo os gráficos #
######################################################################################################

consumo.energia <- gbcbd_get_series(id = c("comercial"=1402, "residencial"=1403, "industrial"=1404, "outros"=1405, "total"=1406),
                                    first.date = '1979-01-31',
                                    last.date = Sys.Date(),
                                    format.data = 'wide')
View(consumo.energia)

plot.consumo.energia <- ggplot(data=consumo.energia) +
                          labs(title = 'Consumo de energia elétrica no Brasil: 1979-2022', x="Data",y="Consumo de energia (GWh)",colour="Legenda") +
                            theme(plot.title=element_text(hjust = 0.5)) +
                              geom_line(aes(x=ref.date,y=total, col= "total")) +
                              geom_line(aes(x=ref.date,y=industrial,col="industrial")) +
                              geom_line(aes(x=ref.date,y=residencial,col="residencial")) +
                              geom_line(aes(x=ref.date,y=comercial,col="comercial")) +
                              geom_line(aes(x=ref.date,y=outros,col="outros")) 
                              
plot.consumo.energia




#######################################
### Transformando em série temporal ###
#######################################

consumo.total <- ts(consumo.energia$total, start= c(1979,1),end = c(2022,11),frequency = 12)

consumo.industrial <- ts(consumo.energia$industrial, start= c(1979,1),end = c(2022,11),frequency = 12)

consumo.residencial <- ts(consumo.energia$residencial, start= c(1979,1),end = c(2022,11),frequency = 12)

consumo.comercial <- ts(consumo.energia$comercial, start= c(1979,1),end = c(2022,11),frequency = 12)

consumo.outros <- ts(consumo.energia$outros, start= c(1979,1),end = c(2022,11),frequency = 12)

par(mfrow=c(3,2))
plot.ts(consumo.total)
plot.ts(consumo.industrial)
plot.ts(consumo.residencial)
plot.ts(consumo.comercial)
plot.ts(consumo.outros)





                          #####################################
                          ##### Modelando Consumo Industrial###
                          #####################################

### Teste Dickey Fuller Aumentando (ADF) para saber se a série é ou não estacionária ###

## Passo (1) - Testando com tendênica e constante
adf.industrial<- ur.df(consumo.industrial,type="trend", lags=27)
plot(adf.industrial)
summary(adf.industrial)

## Passo(2) - Testando com constante
adf2.industrial <- ur.df(consumo.industrial,type="drift",lags=27)
plot(adf2.industrial)
summary(adf2.industrial)

## Passo (3) - Testando sem constante e sem tendência
adf3.industrial <- ur.df(consumo.industrial,type="none",lags=27)
plot(adf3.industrial)
summary(adf3.industrial)

## De acordo com os resultados da estatística de teste, a série contém raiz unitária
## Em outras palavras é uma série não estacionária, que precisará ser integrada


### Modelagem Arima

auto.arima(consumo.industrial,1,1)

Arima.industrial <- Arima(consumo.industrial,order = c(0,1,1),seasonal = list(order=c(0,1,2)))


coeftest(Arima.industrial)
?tsdisplay(residuals(Arima.industrial))
autoplot(Arima.industrial)

y<-forecast(Arima.industrial,13)

  plot(y)
  title(main = NULL,
        sub = 'Previsão do consumo de energia pela industria jan/1979 - dez/2023',
        xlab = 'Tempo',
        ylab = 'Consumo de energia (GWh)')



  
  
                             ########################
                             ### Consumo Comercial###
                             ########################

  ### Teste Dickey Fuller Aumentando (ADF) para saber se a série é ou não estacionária ###
  
  ## Passo (1) - Testando com tendênica e constante ## 
  adf.comercial <- ur.df(consumo.comercial, type = "trend", lags = 16)
  plot(adf.comercial)
  summary(adf.comercial)
  
  ## Passo (2) - Testando apenas com constante ##
  adf2.comercial <- ur.df(consumo.comercial,type = "drift", lags = 16)  
  plot(adf2.comercial)
  summary(adf2.comercial)
  
  ## Passo (3) - Testando sem tendência e sem constante
  adf3.comercial <- ur.df(consumo.comercial,type = 'none', lags = 16)
  plot(adf3.comercial)
  summary(adf3.comercial)
  
  ## De acordo com os resultados da estatística de teste, a série contém raiz unitária
  ## Em outras palavras é uma série não estacionária, que precisará ser integrada
  
 
   # Modelagem Arima
  auto.arima(consumo.comercial,1,1)
  
  
  Arima.comercial <- Arima(consumo.comercial,order=c(2,1,2),seasonal = c(0,1,2))
  
  coeftest(Arima.comercial)
  tsdisplay(residuals(Arima.comercial))
  autoplot(Arima.comercial)
  
  z<-(forecast(Arima.comercial,13))
  
  plot(z)
   title(main= NULL,
         sub ='Previsão do consumo de energia pelo compercio jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   

   
   
   
   
                             ############################
                             ##### Consumo residencial###
                             ############################
   
   ### Teste Dickey Fuller Aumentando (ADF) para saber se a série é ou não estacionária ###
   
   ## Passo (1) - Testando com tendênica e constante
   adf.residencial<- ur.df(consumo.residencial,type="trend", lags=24)
   plot(adf.residencial)
   summary(adf.residencial)
   
   ## Passo(2) - Testando com constante
   adf2.residencial <- ur.df(consumo.residencial,type="drift",lags=24)
   plot(adf2.residencial)
   summary(adf2.residencial)
   
   ## Passo (3) - Testando sem constante e sem tendência
   adf3.residencial <- ur.df(consumo.residencial,type="none",lags=24)
   plot(adf3.residencial)
   summary(adf3.residencial)
   
   ## De acordo com os resultados da estatística de teste, a série contém raiz unitária
   ## Em outras palavras é uma série não estacionária, que precisará ser integrada
   
   
   ## Modelo Arima
   auto.arima(consumo.residencial,1,1)
   
   Arima.residencial <- Arima(consumo.residencial,order=c(5,1,1), seasonal = c(0,1,1)) 
   
   coeftest(Arima.residencial)
   tsdisplay(residuals(Arima.residencial))
   autoplot(Arima.residencial)
   
   w <-forecast(Arima.residencial,13)
   plot(w)
   
   
   
   
   
   
                                       
                                  ############################
                                  ##### Consumo outros #######
                                  ############################
  
    #modelo Arima
   Arima.outros<-auto.arima(consumo.outros)
   
   tsdisplay(residuals(Arima.outros))
   
   x<-forecast(Arima.outros,13)
   
   plot(x)
   
   
   
   
   
   
                                ############################
                                ##### Consumo outros #######
                                ############################
   
   
   #modelo Arima
   Arima.total<-auto.arima(consumo.total)
   
   tsdisplay(residuals(Arima.total))
   
   v<-forecast(Arima.total,13)
   
   plot(v)
   
   
   
  
   ####################################
   ## Concatenando os gráficos: ##
   ####################################
   
   par(mfrow=c(1,1))
   plot(v)
   title(main= NULL,
         sub ='Previsão do consumo de energia total jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   plot(y)
   title(main = NULL,
         sub = 'Previsão do consumo de energia pela industria jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   plot(z)
   title(main= NULL,
         sub ='Previsão do consumo de energia pelo comércio jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   plot(w)
   title(main= NULL,
         sub ='Previsão do consumo de energia residencial jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   plot(x)
   title(main= NULL,
         sub ='Previsão do consumo de energia outros jan/1979 - dez/2023',
         xlab = 'Tempo',
         ylab = 'Consumo de energia (GWh)')
   
 
  ###############################
  ### transformando em tabela ###
  ############################### 
   
   v2 <- as.data.frame(v) %>%
         rename("Consumo.total"= "Point Forecast") %>%
         select(Consumo.total)
  
   y2 <- as.data.frame(y) %>%
     rename("Consumo.industrial"= "Point Forecast") %>%
     select(Consumo.industrial)
   
   z2 <- as.data.frame(z) %>%
     rename("Consumo.comercio"= "Point Forecast") %>%
     select(Consumo.comercio)
   
   w2 <- as.data.frame(w) %>%
     rename("Consumo.residencial"= "Point Forecast") %>%
     select(Consumo.residencial)
   
   x2 <- as.data.frame(x) %>%
     rename("Consumo.outros"= "Point Forecast") %>%
     select(Consumo.outros)
   
   Data <- seq.Date(from = as.Date("2022/12/01"),
                    to = as.Date("2023/12/01"),
                    by = "month")
                    
   
   data <- data.frame(Data,v2,y2,z2,w2,x2) 
    View(data)       
   
   ### Exportando pra Excel ###
   write_xlsx(data,path = "C:/Users/Pedro/Documents/Processo-seletivo/Publicações do Linkedin/Previsão eletricidade/Modelo-ARIMA-Consumo-de-Energia/Tabela-previsão.xlsx")

 
   
   
   

