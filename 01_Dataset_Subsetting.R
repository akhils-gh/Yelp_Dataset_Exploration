library(tidyverse)
library(data.table)
library(stringr)

rm(list= ls())

location = "IL"
business_data = fread("E:\\Projects\\R\\Yelp\\yelp_academic_dataset_business.csv")
checking_data = fread("E:\\Projects\\R\\Yelp\\yelp_academic_dataset_checkin.csv")
tip_data = fread("E:\\Projects\\R\\Yelp\\yelp_academic_dataset_tip.csv")
reviews_data = read.csv("E:\\Projects\\R\\Yelp\\yelp_academic_dataset_review.csv", colClasses = "character")
user_data = fread("E:\\Projects\\R\\Yelp\\yelp_academic_dataset_user.csv")

business = business_data[business_data$state == location, ]
reviews = business %>% left_join(y = reviews_data, by = "business_id")
tip = business %>% left_join(y = tip_data, by = "business_id")
checkin = business %>% left_join(y = checking_data, by = "business_id")
user = user_data %>% filter(user_id %in% reviews$user_id)

write.csv(x = business, file = "business_subset.csv", row.names = FALSE)
write.csv(x = reviews, file = "reviews_subset.csv", row.names = FALSE)
write.csv(x = tip, file = "tip_subset.csv", row.names = FALSE)
write.csv(x = checkin, file = "checkin_subset.csv", row.names = FALSE)
write.csv(x = user, file = "user_subset.csv", row.names = FALSE)
