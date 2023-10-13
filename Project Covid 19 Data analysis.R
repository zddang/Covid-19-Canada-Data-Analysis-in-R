#library
install.packages("covid19.analytics")
library(covid19.analytics)




#Data
#World
# obtain all the records combined for " confirmed ", " deaths " and " recovered " cases
# for the global ( worldwide ) * aggregated * data
ag<-covid19.data(case='aggregated')
View(ag)
str(ag)
head(ag)
#Check missing value
is.null(ag)


#Alternative way to achieve the global ( worldwide ) * aggregated * data 
covid19.data.ALLcases <- covid19.data ()
head(covid19.data.ALLcases)

# obtain "time series" data for global combined for " confirmed ", " deaths " and " recovered " cases
tsa<-covid19.data(case = 'ts-All')
View(tsa)

# obtain "time series" data for global " confirmed " cases
tsc<-covid19.data(case='ts-Confirmed')
View(tsc)

# obtain "time series" data for global " death " cases
tsd<-covid19.data(case = 'ts-deaths')
View(tsd)


is.null(tsa)
is.null(tsc)



#Canada
#obtain Canada records combined for " confirmed ", " deaths " and " recovered " cases
can_ag<-covid19.Canada.data(case-'aggregated')
View(can_ag)
#Canada confirm cases
can_tsc<-covid19.Canada.data(case-'ts-Confirmed')
View(can_tsc)


#Summary

##World
summary(ag)
##Summarize the number of top case display
report.summary(Nentries = 10, graphical.output = T)

##Canada
summary(can_ag)
##Summarize the current situation in Canada
report.summary(geo.loc = 'canada', graphical.output = TRUE, saveReport = FALSE)




#Linear Regression
##Linear Regression for total confirmed cases in Canada
tots.per.location(tsc, geo.loc = "canada")
##Linear Regression for total confirmed cases in Ontario province
tots.per.location(tsc, geo.loc = "ontario")
##Total confirmed cases in different countries
tots.per.location(tsc, geo.loc = c("canada", "US", "india", "china"))

#Growth rate
library(pheatmap)
library(gplots)
## compute changes and growth rates per location for all the countries
growth.rate(tsa) #too large
## compute changes and growth rates per location for Canada
##confirmed cases
growth.rate(tsc, geo.loc = 'canada')
##death case
growth.rate(tsd, geo.loc = 'canada')
## compute changes and growth rates per location for different countries
growth.rate(tsc, geo.loc = c('canada','US','india','china'),
            staticPlt = TRUE, 
            interactiveFig = TRUE)




#Trends
## single location trend , in this case using data from the City of Toronto
###Trend of Active cases in Toronto
tor.data <- covid19.Toronto.data ()
single.trend(tor.data[tor.data$status =="Active Cases",])
itrends (tor.data[,- ncol (tor.data)])

## single trend data from the province of Ontario
###Trend of confirmed cases in Ontario
ont.data <- tsc[tsc$Province.State == "Ontario",]
single.trend(ont.data)
itrends(ont.data)
###Trend of death cases in Ontario
ont.death.data <- tsd[tsd$Province.State == "Ontario",]
single.trend(ont.death.data)
itrends(ont.death.data)

## single trend data from Canada
###Trend of confirmed cases in Canada
Can.data <- tsc[tsc$Country.Region == "Canada",]
single.trend(Can.data)

###Trend of death cases in Canada
Can.death.data <- tsd[tsd$Country.Region == "Canada",]
single.trend(Can.death.data)



#Modeling the Virus Spread by using SIR model for a given geographical location
##Canada SIR model
generate.SIR.model(tsc,'Canada', tot.population = 38250000, add.extras = TRUE)
##Ontario SIR model
generate.SIR.model(tsc ,"Ontario",tot.population = 14570000 , add.extras = TRUE)


## modelling the spread for Canada, storing the model 
Can.SIR.model <- generate.SIR.model(tsc,'Canada', tot.population = 38250000, add.extras = TRUE)
##and generating an interactive visualization
## plotting and visualizing the model
plt.SIR.model(Can.SIR.model,"Canada", interactiveFig =TRUE , 
              fileName ="Can.SIR.model", 
              add.extras = TRUE )

#Plots
##Total Plots
totals.plt()

##Plot total cases in Canada
totals.plt(tsa, "Canada")
totals.plt(tsa, "Ontario")

#World Map
live.map(tsa)
live.map(tsc)
live.map(tsd)

#covid19.analytics explorer dashboard
covid19Explorer(locn = NULL)



