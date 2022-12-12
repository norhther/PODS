library(tidyverse)
library(janitor)
library(tidytext)
library(lubridate)

dc <- read_csv("/home/norhther/Descargas/Detail Change.csv")

dc <- dc %>%
  janitor::clean_names()

dc %>%
  count(ci_type_aff) %>%
  arrange(desc(n)) %>%
  mutate(ci_type_aff = fct_reorder(ci_type_aff, n)) %>%
  ggplot(aes(x = ci_type_aff, y = n)) + geom_col(fill = "0011ff") + coord_flip()

dc %>%
  count(activity) %>%
  arrange(desc(n)) %>%
  mutate(activity = fct_reorder(activity, n)) %>%
  top_n(30) %>%
  ggplot(aes(x = activity, y = n)) + geom_col(fill = "0011ff") + coord_flip()


dc %>%
  count(activity, originated_from) %>%
  arrange(desc(n)) %>%
  mutate(activity = reorder_within(activity, n, originated_from, sep = " ")) %>%
  top_n(40) %>%
  ggplot(aes(x = activity, y = n)) + geom_col(fill = "pink", show.legend = FALSE) + coord_flip() +
  facet_wrap(~originated_from, scales = "free_y") + 
  labs(title = "Number of activities, originated by incident or problem")


actual_start_time <- dc %>%
  filter(!is.na(actual_start)) %>%
  mutate(date = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')))%>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(date < dmy("01-01-2020", "%d-%m-%Y")) %>%
  mutate(type = "actual_start")


actual_end_time <- dc %>%
  filter(!is.na(actual_end)) %>%
  mutate(date = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')))%>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(date < dmy("01-01-2020", "%d-%m-%Y")) %>%
  mutate(type = "actual_end")


  rbind(actual_start_time, actual_end_time) %>%
  ggplot(aes(x = date, y = n, color = type)) + geom_point(alpha = 0.6) + ylim(0, 400) + 
  ggtitle("Number of actual starts and ends vs Time") 
  
  
  
  
dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         log_day_diff = log(as.numeric(actual_end - actual_start))) %>%
  arrange(desc(log_day_diff)) %>%
  select(log_day_diff) %>%
  ggplot(aes(x = log_day_diff)) + geom_histogram(bins = 10)



dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  select(day_diff) %>%
  ggplot(aes(x = day_diff)) + geom_histogram(bins = 30)


dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  ggplot(aes(x = day_diff, color = ci_type_aff)) + geom_boxplot()



dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  ggplot(aes(x = day_diff, color = risk_assessment)) + geom_boxplot()
         

dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  ggplot(aes(x = day_diff, color = originated_from)) + geom_boxplot()


dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  ggplot(aes(x = day_diff, color = emergency_change)) + geom_boxplot()


dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start) * 24)) %>%
  group_by(emergency_change) %>%
  summarize(hours_median = median(day_diff, na.rm = T),
            hours_mean = mean(day_diff, na.rm = T))


dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start))) %>%
  arrange(desc(day_diff)) %>%
  ggplot(aes(x = day_diff, color = activity)) + geom_boxplot()




dc %>%
  count(service_component_wbs_aff, originated_from) %>%
  arrange(desc(n)) %>%
  mutate(activity = reorder_within(service_component_wbs_aff, n, originated_from, sep = " ")) %>%
  top_n(40) %>%
  ggplot(aes(x = activity, y = n)) + geom_col(fill = "pink", show.legend = FALSE) + coord_flip() +
  facet_wrap(~service_component_wbs_aff, scales = "free_y") + 
  labs(title = "Number of activities, originated by incident or problem. X is service_component_wbs_aff")




dc %>%
  filter(!is.na(actual_start), !is.na(actual_end)) %>%
  mutate(actual_start = as_date(strptime(actual_start, format = '%d-%m-%Y %H:%M')),
         actual_end = as_date(strptime(actual_end, format = '%d-%m-%Y %H:%M')), 
         day_diff = (as.numeric(actual_end - actual_start) * 24)) %>%
  group_by(service_component_wbs_aff, originated_from) %>%
  summarize(hours_mean = mean(day_diff, na.rm = T)) %>%
  arrange(desc(hours_mean)) %>%
  head(20) %>%
  ggplot(aes(x = service_component_wbs_aff, y = hours_mean)) + geom_col() + 
  facet_wrap(~originated_from, scales = "free_y") + coord_flip()
