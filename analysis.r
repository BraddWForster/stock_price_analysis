##    Programme:  R
##
##    Objective: Show Juan a bit of code that deals with exchange rate risk and time-value of money for those wishing to own US stocks.
##
##    Plan of  : 
##    Attack   :  
##
##               1. Set up parameters
##               2. Load in risk free-rate and foreign exchange
##               3. Load in stock prices and calculate sharpe-ratios
##
##    Author   :  Bradd Forster
##
##  Clear the decks and load up some functionality
    ##
    rm(list=ls(all=TRUE))

##  Core libraries
    ##
    library(dplyr)
    library(tidyr)
    library(readxl)
    library(writexl)
    library(stringr)
    library(lubridate)
    library(ggplot2)

##  Optional libraries
    ##
    library(quantmod)
    library(vars)
    library(seasonal)
    library(urca)
    library(forecast)
    library(zoo)

##  Set up paths for working directories
    ##
    userid <- "name"
    data <- paste0("C:/Users/", userid, "/Desktop")  # You need to choose some path on your PC
    
##  Setting the working directory
    ##
    setwd(data)

    
################################################################################
## 1.                              Parameters                                 ##
################################################################################
    
df <- data.frame(Tckr = "Test",
                 Sharpe = 0)
#tckr <- c("NVDA", "MSFT")
 tckr <- c(
   "AAPL", "MSFT", "AMZN", "GOOG", "GOOGL", "META", "NVDA", "TSLA", "PEP", "AVGO",
   "COST", "CSCO", "ADBE", "TXN", "CMCSA", "NFLX", "QCOM", "INTC", "AMD", "HON",
   "AMGN", "INTU", "SBUX", "AMAT", "MDLZ", "ISRG", "BKNG", "ADI", "PYPL", "LRCX",
   "GILD", "ADP", "FISV", "REGN", "MU", "CSX", "MRNA", "VRTX", "KLAC", "PANW", 
   "MAR", "SNPS", "ADSK", "MELI", "MNST", "CTSH", "IDXX", "NXPI", "FTNT", "KDP", 
   "CDNS", "LULU", "ORLY", "TEAM", "ROST", "WBA", "CTAS", "BIIB", "EXC", "FAST",
   "DLTR", "ODFL", "PAYX", "PCAR", "VRSK", "SIRI", "ANSS", "AZN", "EA", "ALGN", 
   "WDAY", "BKR", "CHTR", "CPRT", "DDOG", "DOCU", "ENPH", "KHC", "LCID", "MTCH", 
   "NTES", "OKTA", "PDD", "RIVN", "VRSN", "ZM", "ZS", "ARM", "GEHC", "ON", "TTD", 
   "CDW", "CCEP", "DASH", "MDB", "RPRX", "TTWO"
)
measure <- "Open"    # chose the price that you want to calculate the HPR from

current_date <- as.character(Sys.Date())
date_last_year <- as.character(as.Date(format(Sys.Date(), "%Y-%m-%d")) - 365)

################################################################################
## 2.                           Risk-free and Forex                           ##
################################################################################
##  Download the risk-free rate of return
    ##
    rf <- read_excel("hb3.xlsx")
    
##  Download the exchange rate
    ##
    forex <- read_excel("hb1-monthly.xlsx")
    us_column <- grep("united states",
                      forex[1,],
                      ignore.case = TRUE)

    row <- which(apply(forex,
                       1,
                       function (x) any(x == "Series Id")),
                       arr.ind = TRUE)
    
    forex <- forex[row+1:nrow(forex), c(1, us_column)]
    
    colnames(forex) <- c("Date", "UsdNzd")
    
    forex <- forex[complete.cases(forex),]
    
    forex$Date <- as.Date(as.numeric(forex$Date), origin = "1899-12-30")    # make the date variable a date format
    
    forex$UsdNzd <- as.numeric(forex$UsdNzd)
    
##  Tidy the RBNZ data on commercial term deposit rates
    ##
    term_deposit_column <- grep("term deposit",
                                names(rf),
                                ignore.case = TRUE)    # the column you want
    
    row <- which(apply(rf,
                       1,
                       function (x) any(x == "Series Id")),
                        arr.ind = TRUE)    # the row that you want to follow

    rf <- rf[row+1:nrow(rf),c(1,term_deposit_column)]    # shorten the data
    
    colnames(rf) <- c("Date", "Rate")    # rename the columns
    
    rf <- rf[complete.cases(rf),]    # Remove the missing rates

    rf$Date <- as.Date(as.numeric(rf$Date), origin = "1899-12-30")    # make the date variable a date format

    rf$Rate <- as.numeric(rf$Rate)/100    # make the rate variable in decimal format

##  Adjust for the exchange rate
        ##
        forex$Year <- year(forex$Date)
        forex$Month <- month(forex$Date)
      
##  Calculating the effective annual rates for risk free: (1+EAR)^T = Return => EAR = Return^1/T -1
    ##
    colnames(rf) <- c("Date", "Ear")
    
    rf$Quoted <- (1 + rf$Ear)^(1/2) -1    # the actual rate of return for 6-months
    
    rf$Year <- as.numeric(format(rf$Date, "%Y"))
    rf$Month <- as.numeric(format(rf$Date, "%m"))
    
    
################################################################################
##  3.                             Stock Price                                ##
################################################################################

for (i in tckr) {
  getSymbols(i, src = "yahoo", from = date_last_year, to = current_date)
  sp <- data.frame(Date = index(get(i)), coredata(get(i)))
  div <- getDividends(i, src = "yahoo", from = date_last_year, to = current_date)
  div <- data.frame(Date = index(div), Dividend = coredata(div))
  ##  Rename the columns 
    ##
    colnames(sp) <- str_remove_all(colnames(sp), pattern = paste0(i,"."))    # renaming the columns and removing the .`TCKR`
    colnames(div) <- str_to_title(str_remove_all(colnames(div), pattern = paste0(i,".")))
  ##  Calculate stock price "monthly" holding period returns as this is the frequency of the risk free rate data
      ##
      sp$Year <- as.numeric(format(sp$Date, "%Y"))
      sp$Month <- as.numeric(format(sp$Date, "%m"))
      sp <- sp[order(sp[[1]]),]
      sp <- do.call(rbind, lapply(split(sp, list(sp$Year, sp$Month)), function (x) {
        x[c(which.max(x$Date)), ]
      } ))    # get the price on the first and last day of each month
      sp <- sp[order(sp[[1]]),]    # order the data frame by the first column
      div$Year <- as.numeric(format(div$Date, "%Y"))
      div$Month <- as.numeric(format(div$Date, "%m"))
      sp <- merge(sp, div, by = c("Year", "Month"), all.x = TRUE)    # bring dividends to the dataframe
      colnames(sp) <- str_replace_all(str_replace_all(string = colnames(sp), pattern = ".x", replacement = ""), pattern = ".y", replacement = "Div")
      sp$Div <- ifelse(is.na(sp$Div), 0, sp$Div)
      ##  Cut off
          ##
          if (as.Date(paste0("2024-", if (month(Sys.Date()) == 12) {1} else {month(Sys.Date())+1} ,"-01"), "%Y-%m-%d")-1 - Sys.Date()>3) {
            sp <- sp[1:(nrow(sp)-1),]
          }
      sp <- merge(sp, forex, by = c("Year", "Month"), all.x = TRUE, suffixes = c("", "_y"))
      sp <- sp[,!grepl("_y", names(sp))]
      sp[,measure] <- sp[,measure]/sp[,"UsdNzd"]
      sp[,"Div"] <- sp[,"Div"]/sp[,"UsdNzd"]
      sp <- sp[order(sp[[3]]),]
      sp$Hpr <- (sp[,measure] - c(NA, sp[-nrow(sp),measure]) + sp[,"Div"])/ c(NA, sp[-nrow(sp),measure])    # percentage change from last period
      sp <- sp[ifelse(is.na(c(as.numeric(sp$Date) - c(NA, sp[-nrow(sp),"Date"])) > 20), TRUE, c(as.numeric(sp$Date) - c(NA, sp[-nrow(sp),"Date"])) > 20),]    # removing the HPR with less than 20 days between start and end
      sp <- merge(sp, rf, by = c("Year", "Month"), all.x = TRUE)
      sp <- sp[!is.na(sp$Ear),]
      colnames(sp) <- str_replace_all(str_replace_all(string = colnames(sp), pattern = ".x", replacement = ""), pattern = ".y", replacement = "Rf")
      sp$MonthQuoted <- sp$Quoted/6
      sp <- sp[!is.na(sp$Hpr), ]
      sp$ExcessReturn <- sp$Hpr - sp$MonthQuoted
      risk_premium <- mean(sp$ExcessReturn)
      std_risk_premium <- sd(sp$ExcessReturn)
      sharpe_ratio <- risk_premium/std_risk_premium
  df <- rbind(df, data.frame(Tckr = i,
                            Sharpe = sharpe_ratio))
}
    
df <- df[-1,]

View(df)
