library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
setwd("/Users/jonathanloke/Desktop/UChicago DPSS/Capstone Project")
spending_timeseries_total <- read_csv("spending_timeseries_total.csv")

#Fig. 1
spending_timeseries_total2 <- spending_timeseries_total %>%
  separate(date_smart, into = c("month", "day")) %>% mutate(year = as.numeric(year)) %>% mutate(month = as.numeric(month)) %>% mutate(day = as.numeric(day)) %>%
  unite(date, year, month, day, sep = '-') %>% mutate(date=as.Date(`date`))

ggplot(spending_timeseries_total2, aes(x=date, y=spending, color=top_category)) + geom_line() + labs(x="Date", y="Daily spending", title="Daily time series figure of spending by retail category", color="Retail category") + scale_x_continuous(breaks=seq(ymd("2022-02-01"), ymd("2022-03-31"), 7)) + theme_bw() + theme(legend.position="bottom") #Gives daily time series figure of spending by retail category

#Fig. 2
spending_timeseries_total3 <- spending_timeseries_total2 %>% mutate(day_of_week=wday(date,week_start=1))

spending_day_of_week <- spending_timeseries_total3 %>% group_by(day_of_week,top_category) %>% summarize(mean_spending=mean(spending))

spending_timeseries_total4 <- spending_timeseries_total3 %>% left_join(spending_day_of_week,by=c("day_of_week","top_category")) %>% mutate(deseasonalised_spending=spending-mean_spending)

ggplot(spending_timeseries_total4, aes(x=date, y=deseasonalised_spending, color=top_category)) + geom_line() + labs(x="Date", y="Deseasonalised daily spending", title="Deseasonalised daily time series figure of spending by retail category", color="Retail category") + scale_x_continuous(breaks=seq(ymd("2022-02-01"), ymd("2022-03-31"), 7)) + theme_bw() + theme(legend.position="bottom") #Gives deseasonalised daily time series figure of spending by retail category

#Fig. 3
spending_timeseries_per <- read_csv("spending_timeseries_per.csv")

spending_timeseries_per2 <- spending_timeseries_per %>% 
  mutate(year = as.numeric(year)) %>% mutate(month = as.numeric(month)) %>% mutate(day = as.numeric(day)) %>%
  unite(date, year, month, day, sep = '-') %>% mutate(date=as.Date(`date`))

spending_timeseries_per3 <- spending_timeseries_per2 %>% mutate(day_of_week=wday(date,week_start=1))

spending_ptpd_day_of_week <- spending_timeseries_per3 %>% group_by(day_of_week,top_category) %>% summarize(mean_spending_ptpd=mean(spending_ptpd))

spending_timeseries_per4 <- spending_timeseries_per3 %>% left_join(spending_ptpd_day_of_week, by=c("day_of_week","top_category")) %>% mutate(deseasonalised_spending_ptpd=spending_ptpd-mean_spending_ptpd)

ggplot(spending_timeseries_per4, aes(x=date, y=deseasonalised_spending_ptpd, color=top_category)) + geom_line() + labs(x="Date", y="Deseasonalised daily spending per transaction", title="Deseasonalised daily time series figure of spending per transaction by retail category", color="Retail category") + scale_x_continuous(breaks=seq(ymd("2022-02-01"), ymd("2022-03-31"), 7)) + theme_bw() + theme(legend.position="bottom") #Gives deseasonalised daily time series figure of spending per transaction by retail category

#Fig 4
us_gdf <- st_read("tl_2016_us_county.shp")
cali_gdf <- us_gdf %>% filter(STATEFP=="06") %>% mutate(COUNTYFP=as.character(COUNTYFP))

spending_maps <- read_csv("spending_maps.csv")
cali_maps <- spending_maps %>% filter(STATEFP=="06") %>% mutate(COUNTYFP=as.character(COUNTYFP))
cali_gdf <- left_join(cali_gdf,cali_maps,by=c("STATEFP","COUNTYFP"))

cali_gdf_qtile <- cali_gdf %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.02)

ggplot() +
  geom_sf(
    data = cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in retail spending in California by county, deciles")

#Fig 5a
clothing_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Clothing Stores") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.1)
ggplot() +
  geom_sf(
    data = clothing_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Clothing Stores retail spending in California by county, deciles")

#Fig 5b
gas_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Gasoline Stations") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.1)
ggplot() +
  geom_sf(
    data = gas_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Gasoline Stations retail spending in California by county, deciles")

#Fig 5c
groc_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Grocery Stores") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.1)
ggplot() +
  geom_sf(
    data = groc_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Grocery Stores retail spending in California by county, deciles")

#Fig 5d
trav_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Traveler Accommodation") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.3)
ggplot() +
  geom_sf(
    data = trav_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Traveler Accommodation retail spending in California by county, deciles")

#Fig 5e
dep_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Department Stores") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.3)
ggplot() +
  geom_sf(
    data = dep_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Department Stores retail spending in California by county, deciles")

#Fig 5f
gen_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="General Merchandise Stores including Warehouse Clubs and Supercenters") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.2)
ggplot() +
  geom_sf(
    data = gen_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in General Merchandise Stores retail spending in California by county, deciles")

#Fig 5g
rest_cali_gdf_qtile <- cali_gdf %>% filter(top_category=="Restaurants and Other Eating Places") %>%
  mutate(qtile = factor(ntile(delta, 10))) %>% sample_frac(0.02)
ggplot() +
  geom_sf(
    data = rest_cali_gdf_qtile,
    aes(color = qtile, fill = qtile)) + labs(title="Change in Restaurants retail spending in California by county, deciles")

#Fig 6
spending_shift_in_baskets <- read_csv("spending_shift_in_baskets.csv")

model <- lm(delta_STD_gen~delta_STD_gas, spending_shift_in_baskets)
summary(model) 

#Fig 7
model_fe <- lm(delta_STD_gen~delta_STD_gas + factor(STATEFP), data=spending_shift_in_baskets)
summary(model_fe) 
