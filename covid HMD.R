### code relating to blogpost on http://ronaldrichman.co.za/2020/05/21/hmd-weekly-data/

require(data.table)
require(dplyr)
require(ggplot2)
require(ggpubr)
require(forecast)

dat = fread("https://www.mortality.org/Public/STMF/Outputs/stmf.csv")
dat = dat %>% melt.data.table(id.vars = c("CountryCode", "Year", "Week","Sex", "Split", "SplitSex", "Forecast"))

country_spec = dat[CountryCode == "GBRTENW"]
country_spec %>% ggplot(aes(x=Week, y= value))+geom_line(aes(group = Year, colour = Year))+facet_wrap(Sex~variable, scales="free")

country_spec = country_spec[Sex == "b" & variable == "RTotal"]

country_spec %>% 
  ggplot(aes(x=Week, y= value))+geom_line(aes(group = Year, colour = as.factor(Year)))+facet_wrap(Sex~variable, scales="free")

start_year = country_spec[,min(Year)]

country_spec$value %>%
  ts(frequency = 52, start = c(start_year,1)) %>% 
  mstl() %>% 
  autoplot()

fit = country_spec$value %>%
  ts(frequency = 52, start = c(start_year,1)) %>% 
  auto.arima()

forecasts = fit %>% forecast(level = c(.95, .995))
forecasts = data.table(mean = forecasts$mean, lower = forecasts$lower, upper = forecasts$upper)
forecasts[, Week:=1:.N]


dat2020 = dat[CountryCode == "GBRTENW"][Sex == "b" & variable == "RTotal"&Year >= 2020] %>% setkey(Week)
forecasts = forecasts[Week<dat2020[,max(Week)]] %>% setkey(Week)
dat2020 = dat2020 %>% merge(forecasts)
dat2020[,actual:=value]
dat2020[,value:=NULL]
dat2020[,variable :=NULL]

dat2020 %>% 
  ggplot()+
  geom_line(aes(x=Week, y= actual), colour="red", size=1)+
  geom_line(aes(x=Week, y= mean), colour="darkblue", size=1)+
  geom_ribbon(aes(x=Week,ymin = `lower.95%`, ymax =`upper.95%`), fill = "lightblue", alpha = 0.3)+
  geom_ribbon(aes(x=Week,ymin = `lower.99.5%`, ymax =`upper.99.5%`), fill = "lightblue", alpha = 0.3)+
  annotate("text", x = 15, y = 0.016, label = "2020 rates")+
  annotate("text", x = 15, y = 0.01, label = "Mean forecast rates")+
  annotate("text", x = 10, y = 0.0125, label = "Upper End of 99.5% band")