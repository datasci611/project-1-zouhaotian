library(tidyverse)

data("ChickWeight")
data("ToothGrowth")
data("Seatbelts")
data("airquality")

ds_1 <- as.tibble(ChickWeight) %>% 
  group_by(Chick) %>% 
  mutate(baseweight=min(weight), wt_diff=weight-baseweight)
ds_1 %>%
  group_by(Diet) %>% 
  filter(Time==21) %>% 
  summarise(mean=mean(wt_diff))
ds_1 %>% 
  ggplot(mapping=aes(x=Time, y=wt_diff, group=Chick)) +
  geom_point() + 
  geom_line(mapping=aes(color=Diet)) +
  xlab("Time (Days)") +
  ylab("Weight Growth from Baseline")

ds_2 <- as.tibble(ToothGrowth) %>% 
  group_by(supp) %>% 
  arrange(dose)
ds_2 %>% 
  group_by(dose, supp) %>% 
  summarise(mean=mean(len))
ds_2 %>% 
  ggplot(mapping=aes(x=as.factor(dose), y=len, color=supp)) +
    geom_boxplot() +
    xlab("dose (mg/day)") +
    ylab("tooth length (cm)")

ds_3 <- as.tibble(Seatbelts) %>% 
  mutate(month=1:nrow(ds_3) %% 12)
ds_3$month <- sapply(ds_3$month, function(x) if (x==0) 12 else x)
ds_3 %>% 
  group_by(month) %>% 
  summarise(mean=mean(drivers))
ds_3 %>% 
  ggplot(mapping=aes(x=as.factor(month))) +
    geom_point(mapping=aes(y=drivers, color='drivers')) + 
    geom_point(mapping=aes(y=DriversKilled, color='DriversKilled')) +
    geom_point(mapping=aes(y=front, color='front')) +
    geom_point(mapping=aes(y=rear, color='rear')) +
    xlab('Month') + 
    ylab('Drivers Killed or Injured')

ds_4 <- as.tibble(airquality)
ds_4 %>% 
  group_by(Month) %>% 
  summarise(mean=mean(Ozone, na.rm=TRUE))
ds_4 %>% 
  filter(!is.na(Ozone)) %>% 
  ggplot(mapping=aes(x=as.factor(Month), y=Ozone)) + 
    geom_boxplot() + 
    xlab('Month') + 
    ylab('Mean Ozone (ppb)')