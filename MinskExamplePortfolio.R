#Filippo Macaluso
# PortfolioLoader script pool historical data from a data source
# Yahoo
rm(list = ls())
#if you do not have installed those packages,please run this line of code 
# install.packages(.....)
#install.packages(c("quantmod","FRAPO","lattice","PerformanceAlytics","dplyr",
#                   "ggplot2","reshape2","WriteXLS"))
library(quantmod)
library(FRAPO)
library(lattice)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(reshape2)
library(WriteXLS)
##########################################
#Historical Data Dowload   ###############
##########################################
#Manually enter the tickers
Tickers<-c("AAPL","MA","IBM","GOOGL","NSRGY","NKE")
start.date<-"2005-01-01"
end.date<- "2019-08-31"
source.database<-'yahoo'
sampling.period="monthly"
###Get data from yahoo###################
getSymbols(Tickers,src=source.database, from=start.date,to=end.date,
           periodicity = sampling.period)
for(symbol in Tickers) {
  x <- get(symbol)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  x <- x[,6]
  assign(symbol,x)
}

# merge price histories into one data set   
prices.database <- do.call(merge, lapply(Tickers, get))
colnames(prices.database) <-c(Tickers)




#############
### save the data in excel spreadsheet 
#####
prices.database<-as.data.frame(prices.database)
WriteXLS(prices.database,"../Data/PricesPortfolio.xls",row.names = TRUE)

#### dirty method to align the starting date of stocks
prices.database_tidy<-prices.database[-(1:16),]  

#---------------#
# Stylized Fact
#---------------#
prices.database_tidy_long<- prices.database_tidy %>%
  mutate(TIME=as.Date(row.names(prices.database_tidy))) %>%
                                  melt(id.vars="TIME",
                                  measure.vars=c("AAPL","MA","IBM",
                                    "GOOGL","NKE"),
                                    value.name="VALUE",
                                     variable.name="STOCKS"  ) 


StocksPlot<-prices.database_tidy_long %>% 
  ggplot(aes(x=TIME,y=VALUE)) +
  geom_line(color="blue") +
  ylab("Price ")+
  xlab("Date")  +
  scale_x_date(date_breaks = "8 months") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_grid(STOCKS~.,scales="free_y") 

StocksPlot

Returns <- returnseries(prices.database_tidy, method = "discrete", trim = TRUE,percentage = FALSE)

boxplot(Returns)

VarCov <- cov(Returns)
## Portfolio Optimizations
GMVw <- Weights(PGMV(Returns,percentage =TRUE))
### MSR weights were compute by excel spreadsheet PortfolioConstruction.xls

#MSRw<-c (0.0971936483,0.3480627147,5.64003963406084e-08,4.2481425039992e-07,
#        0.360840071,0.1939030848) *100 
########
MDPw <- Weights(PMD(Returns,percentage = TRUE))
MTDw <- Weights(PMTD(Returns,percentage =TRUE))
ERCw <- Weights(PERC(VarCov))
#Combine weights
W<-cbind(GMVw,MSRw,MDPw, MTDw, ERCw)
#Marginal Risk Contribution

MRC<-apply(W,2,mrc , Sigma = VarCov)
rownames(MRC)<-Tickers
colnames(MRC)<-c("GMV","MSR","MDP","MTD","ERC")
#Concentation Ratio
CR<-apply(W,2,cr , Sigma = VarCov)
#Diversification Ratio
DR<-apply(W,2,dr , Sigma = VarCov)

##  Allocations under different portfolio contruction methods 
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(3, 2))
dotchart(GMVw, xlim = c(0, 40), main = "GMV Allocation", pch = 19)
dotchart(MSRw-GMVw,xlim = c(-20, 20), main = "MSR vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(MDPw - GMVw, xlim = c(-20, 20), main = "MDP vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(MTDw - GMVw, xlim = c(-20, 20), main = "MTD vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(ERCw - GMVw, xlim = c(-20, 20), main = "ERC vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
par(oldpar)

#### MR contribution by stocks and by portfolio plots
nAssets <- factor(rep(rownames(MRC), dim(W)[2]),
                 levels = sort(rownames(MRC)))
Port <- factor(rep(colnames(MRC), each = length(Tickers)),
               levels = colnames(MRC))
MRCdf <- data.frame(MRC = c(MRC), Port, nAssets)
dotplot(nAssets ~ MRC | Port, groups = Port, data = MRCdf,
        xlab = "Percentages",
        main = "MR Contributions by Stocks per Portfolio",
        col = "blue", pch = 19)
dotplot(Port ~ MRC | nAssets, groups = nAssets, data = MRCdf,
        xlab = "Percentages",
        main = "MR Contributions by Portfolio per Stocks",
        col = "blue", pch = 19)


## Portfolio Risk Measures and Characteristics
par(mfrow = c(1, 1))
PReturns <- apply(W, 2, function(x) as.matrix(Returns) %*% x / 100)
EXPRET<-apply(PReturns, 2, mean) * 100  *12
SD <- apply(PReturns, 2, sd) * 100 *sqrt(12)

SR<-(EXPRET-1.5)/SD
ES95 <- apply(PReturns, 2, function(x)
  abs(ES(R = x, method = "historical") * 100))
VaR95 <- apply(PReturns, 2, function(x)
  abs(VaR(R = x, method = "historical") * 100))
DR <- apply(W, 2, dr, Sigma = VarCov)
CR <- apply(W, 2, cr, Sigma = VarCov)
## Summarising results
Res <- rbind(EXPRET,SD,SR,ES95,VaR95, DR, CR)
Res
# Plot Performance Portfolio

PReturns_tidy <- PReturns %>% as.data.frame %>%
                  mutate(TIME=as.Date(row.names(prices.database)[-(1:17)]))%>%
                  mutate(MSRGMV=MSRw/GMVw-1) %>%
                  mutate(MDPGMV=MDPw/GMVw-1) %>%
                  mutate (MTDGMV=MTDw/GMVw-1) %>%
                  mutate (ERCGMV=ERCw/GMVw-1)

