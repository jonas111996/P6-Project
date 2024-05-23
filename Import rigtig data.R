
##################### Importere det faktiske elforbrug


res <- GET("https://api.energidataservice.dk/dataset/productionconsumptionsettlement?start=2023-08-21T00:00&end=2023-08-31T00:00&filter={\"PriceArea\":\"DK1\"}")
rescontent <- content(res, as = "text")

# Parse JSON content
strjson <- fromJSON(rescontent)

# Convert to data frame
Elforbrug_rigtig <- strjson$records



Elforbrug_rigtig <- Elforbrug_rigtig[order(Elforbrug$HourDK),]
rownames(Elforbrug_rigtig) <- NULL  # Fjerner oprindeligt obs. nummer


#NB NB NB NB 
Elforbrug_rigtig$Week <- strftime(Elforbrug_rigtig$HourDK, format = "%Y-%V") 



# Group by week and calculate mean
weekly_means_forbrug_rigtig <- Elforbrug_rigtig %>%
  group_by(Week) %>%
  summarise(MeanConsumptions = mean(GrossConsumptionMWh, na.rm = TRUE))


weekly_mean_forbrug_rigtig <- weekly_means_forbrug_rigtig$MeanConsumptions


weekly_mean_forbrug_rigtig_ts <- ts(weekly_mean_forbrug_rigtig, frequency = 52, start = c(2017, which(week(ymd("2016-01-01")) == weekly_means_forbrug_rigtig$Week[1])))

print(weekly_mean_forbrug_rigtig_ts)






