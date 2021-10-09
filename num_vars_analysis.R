data <- data %>%  
  mutate(SALE = ifelse(SALE == 1,'YES','NO'))
data$SALE<-as.factor(data$SALE)

##NUMERICAL DATA

#PREMIUM
# Decrease graph size from standard
options(repr.plot.width = 10, repr.plot.height = 3)
cdplot(SALE ~ PREMIUM, data=data)
ggplot(
  data = data,
  aes(y = PREMIUM, x = SALE, color = SALE)
) +
  #  theme +
  geom_boxplot()
hist(data$PREMIUM)


#### LICENCEYEARSHELD
cdplot(SALE ~ LICENCEYEARSHELD, data=data)
# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  data %>%
    filter(SALE == "YES") %>%
    group_by(LICENCEYEARSHELD) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round((n / sum(n))*100, 3)
    ) %>%
    
    # group_by(LICENCEYEARSHELD, SALE) %>%
    # summarize(
    #   n = n()
    # ) %>%
    # ungroup(SALE) %>% 
    # mutate(
    #   sum = sum(n),
    #   Percentage = round((n / sum)*100, 2)
    # ) %>%
    # filter(SALE =="YES") %>% 
    
    # Create plot
    ggplot(
      aes(x = LICENCEYEARSHELD, y = Percentage, color = LICENCEYEARSHELD)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    #  theme +
    labs(
      x = "LICENCEYEARSHELD", y = "SALE (%)"
    ),
  
  ggplot(
    data = data,
    aes(y = LICENCEYEARSHELD, x = SALE, color = SALE)
  ) +
    #  theme +
    geom_boxplot()
  , align = "h")

hist(data$LICENCEYEARSHELD)


####NOCLAIMSDISCOUNT
cdplot(SALE ~ NOCLAIMSDISCOUNT, data=data)
# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  data %>%
    # filter(SALE == "YES") %>%
    # group_by(NOCLAIMSDISCOUNT) %>%
    # summarize(
    #   n = n()
    # ) %>%
    # mutate(
    #   Percentage = round((n / sum(n))*100, 3)
    # ) %>%
    group_by(NOCLAIMSDISCOUNT, SALE) %>%
    summarize(
      n = n()
    ) %>%
    ungroup(SALE) %>% 
    mutate(
      sum = sum(n),
      Percentage = round((n / sum)*100, 2)
    ) %>%
    filter(SALE =="YES") %>% 
    # Create plot
    ggplot(
      aes(x = NOCLAIMSDISCOUNT, y = Percentage, color = NOCLAIMSDISCOUNT)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    #  theme +
    labs(
      x = "NOCLAIMSDISCOUNT", y = "SALE (%)"
    ),
  
  ggplot(
    data = data,
    aes(y = NOCLAIMSDISCOUNT, x = SALE, color = SALE)
  ) +
    #  theme +
    geom_boxplot()
  , align = "h")
hist(data$NOCLAIMSDISCOUNT)

#### VEHICLEAGE
hist(data$VEHICLEAGE)
cdplot(SALE ~ VEHICLEAGE, data=data)
ggplot(
    data = data,
    aes(y = VEHICLEAGE, x = SALE, color = SALE)
  ) +
    #  theme +
    geom_boxplot()

#### VEHICLEVALUE
cdplot(SALE ~ VEHICLEVALUE, data=data)

  ggplot(
    data = data,
    aes(y = VEHICLEVALUE, x = SALE, color = SALE)
  ) +
    #  theme +
    geom_boxplot()



hist(data$VEHICLEVALUE)


cdplot(SALE ~ INCEPTIONDATE, data=data)
