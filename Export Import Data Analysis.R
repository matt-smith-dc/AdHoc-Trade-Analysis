
library(comtradr)
library(httr)
library(jsonlite)
library(wbstats)
library(dplyr)
library(tidyverse)
library(psych)
library(rnaturalearth)
library(rnaturalearthdata)

#Meant to partially replicate following analysis: 
#https://www.trade.gov/data-visualization/tradestats-express-us-trade-product

#Set Comtrade API key
subscription.key = "XXXXXXXXXXXXXXXXXXXXXXXXXX"

#Open pop-up to enter API key
set_primary_comtrade_key(subscription.key)


#define Not In operation
`%ni%` <- Negate(`%in%`)

#Function to request trade data from Comtrade database using API, using HS-2 code
get_trade_data <- function(reporter, commodity.code, partner, year, flow) {
  return (ct_get_data(
    reporter = reporter,
    partner = partner,
    commodity_code = commodity.code,
    start_date = year,
    end_date = year,
    flow_direction = flow
  ))
}

#Create world map highlighting top trade partners, using HS-2 code
plot_trade_map <- function(reporter, commodity.code, year, flow) {
  title.flow = paste(toupper(substr(flow,1,1)), substr(flow,2,nchar(flow)), sep="")
  comtrade.data <- get_trade_data(reporter, commodity.code, 'everything', year, flow)
  commodity.desc <- unique(comtrade.data$cmd_desc)
  
  #Comtrade data, ignoring values that won't match
  comtrade.data <- comtrade.data %>%
    select(partner_code, partner_desc, partner_iso, primary_value) %>%
    filter(partner_iso %ni% c('S19', 'W00'))
  #Country sf data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    select(name, iso_a3_eh, geometry)
  #Merge UN Comtrade data with World SF data
  merged.name <- merge(x=comtrade.data, y=world, 
                       by.x="partner_iso", by.y="iso_a3_eh", 
                       all.x=FALSE, all.y=TRUE)
  #Map Plot
  merged.name %>%
    ggplot() +
    geom_sf(aes(geometry=geometry, fill = primary_value)) +
    scale_fill_viridis_c(option = "plasma", na.value="white")+
    labs(title=paste("Top U.S. ", title.flow, " Partners ", year, " - ", 
                     commodity.desc, " (HS Code ", commodity.code, ")", sep=""),
         xlab="Lat.", ylab = "Long.", fill=paste(title.flow, " Value", sep=""))
}

#Create bar graph highlighting top traded goods
plot_top_commodities <- function(reporter, partner, year, flow) {
  title.flow = paste(toupper(substr(flow,1,1)), substr(flow,2,nchar(flow)), sep="")
  comtrade.data <- get_trade_data(reporter, 'everything', partner, year, flow)
  comtrade.data <- comtrade.data %>% filter(aggr_level==2) %>%
    mutate(ranking=dense_rank(desc(primary_value)),
           commodity = paste0(cmd_code, " ", cmd_desc)) %>%
    filter(ranking <= 5)
  
  comtrade.data %>% ggplot(aes(x=reorder(cmd_code, -primary_value), y=primary_value)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title=paste("Top U.S. ", title.flow, " Goods ", year, " - ", 
                     partner, sep="")) +
         xlab("Commodity Code (HS2)") + ylab(paste(title.flow, " Value", sep="")) +
    geom_text(aes(label=primary_value), position=position_dodge(width=0.9), vjust=-0.25)

}

#Create pivot data of trading partner data
create_partner_pivot <- function(reporter, commodity.code, flow, start.year, end.year) {
  for(i in start.year:end.year) {
    comtrade.data <- get_trade_data(reporter, commodity.code, 'everything', i, flow)
    if (i==start.year){
      total.data <- comtrade.data
    } else {
      total.data <- rbind(total.data, comtrade.data)
    }
  }
  total.data <- total.data %>% select(partner_desc, period, primary_value)
  
  pivot <- reshape(total.data, timevar='period', idvar = 'partner_desc', direction='wide',
          v.names='primary_value') %>% arrange_at(ncol(.), desc)
  print(pivot)
  
}

#Create pivot data of traded product data
create_product_pivot <- function(reporter, partner, flow, start.year, end.year) {
  for(i in start.year:end.year) {
    comtrade.data <- get_trade_data(reporter, 'everything', partner, i, flow)
    comtrade.data <- comtrade.data %>% filter(aggr_level==2)
    if (i==start.year){
      total.data <- comtrade.data
    } else {
      total.data <- rbind(total.data, comtrade.data)
    }
  }
  total.data <- total.data %>% select(cmd_code, cmd_desc, period, primary_value)
  
  pivot <- reshape(total.data, timevar='period', idvar = c('cmd_code', 'cmd_desc'), direction='wide',
                   v.names='primary_value') %>% arrange_at(ncol(.), desc)
  print(pivot)
}


reporter <- 'USA'
commodity.code <- '02'  #HS code
year <- 2023
flow <- 'export'

start.year <- 2018
end.year <- 2023

#Create map plot
plot_trade_map(reporter, commodity.code, year, flow)

#Create pivot for multiple years
create_partner_pivot(reporter, commodity.code, flow, start.year, end.year)

partner = 'World'

#Graph of top 5 commodities
plot_top_commodities(reporter, partner, year, flow)

#Pivot of commodity code values
create_product_pivot(reporter, partner, flow, start.year, end.year)

