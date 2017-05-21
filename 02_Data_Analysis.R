library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)
library(qdap)
library(RWeka)
library(lubridate)
library(ggmap)

## reading the subsetted files
business = read.csv("data/business_subset.csv")
reviews = read.csv("data/reviews_subset.csv")
tips = read.csv("data/tip_subset.csv")
checkin = read.csv("data/checkin_subset.csv")
user = read.csv("data/user_subset.csv")

##---------------- cleaning business file

business$categories = str_replace_all(business$categories, "[[-'/()&\\[\\]]]", "") # categories are now separated by a comma
business$attributes = str_replace_all(business$attributes, "[[!\"\\$%&'()*+\\-./;<=>?@\\[\\]^_`{|}~]]", "") #attributes are also separated by comma. A colon sign is also present 
business$is_food = tolower(str_trim(business$categories)) %>%  str_detect(pattern = "food|restaurants")
business = business[business$is_food == TRUE,]
write.csv(x = business, file = "business_clean.csv", row.names = F)

##---------------- Cleaning reviews file

reviews = rename(reviews, net_rating = stars.x)
reviews = rename(reviews, user_rating = stars.y)
reviews$hours = reviews$hours %>% str_replace_all(pattern = "[[''\\[\\]]]", replacement = "")
reviews$weekday = reviews$date %>% as.Date() %>% weekdays()
reviews$is_weekend = ifelse(reviews$weekday %in% c("Saturday", "Sunday"), "Yes", "No")
reviews$attributes = str_replace_all(reviews$attributes, "[[!\"\\$%&'()*+\\-./;<=>?@\\[\\]^_`{|}~]]", "")
reviews$categories = str_replace_all(reviews$categories, "[[-'/()&\\[\\]]]", "")
reviews$is_food = tolower(str_trim(reviews$categories)) %>%  str_detect(pattern = "food|restaurants")
reviews = reviews[reviews$is_food == TRUE,]

useful_reviews = reviews[reviews$useful>0, ] # creating data only with useful reviews

write.csv(x = reviews, file = "reviews_clean.csv", row.names = F)

##---------------- Cleaning Chekin File

checkin = checkin[,c("time", "type.y", "business_id")]
checkin = checkin[!is.na(checkin$time), ]
checkin$time = checkin$time %>% str_replace_all(pattern = "[[\\[\\]']]", replacement = "")
checkin_time = checkin$time %>% str_split(pattern = ",", simplify = T)
checkin = cbind(checkin, checkin_time)
checkin = checkin %>% gather(key = "key", value = "value", 4:ncol(checkin))
checkin$key = NULL
checkin = checkin %>% separate(col = value, into = c("day", "day_time"), sep = "-")
checkin = checkin %>% separate(day_time, into = c("checkin_time", "checkin_count"), sep = ":")
checkin = checkin[,c("business_id", "type.y","day", "checkin_time", "checkin_count")]

write.csv(x = checkin, file = "checkin_clean.csv", row.names = F)

##----------------- Cleaning Users file

user = as.data.frame(user)

user$yelping_since = as.Date(user$yelping_since)
user = user[, c("user_id", setdiff(colnames(user), "user_id"))]
user$elite = user$elite %>% str_replace_all(pattern = "[[''\\[\\]]]", "")
user$friends = user$friends %>% str_replace_all(pattern = "[[''\\[\\]]]", "")
user$elite[user$elite == "None"] = NA
#user$yelping_since_days = as.numeric(today() - user$yelping_since)
#user$yelping_since_weeks = as.numeric(difftime(today(), user$yelping_since , units = "weeks"))
#user$yelping_since_qtr = as.numeric(user$yelping_since_weeks/13)
#user$yelping_since_months = as.numeric(user$yelping_since_qtr*3)
#user$friend_count = (str_count(user$friends, pattern = ",") +1)

write.csv(x = user, file = "user_clean.csv", row.names = F)


##---------------- lets visualize data on maps!

library(ggmap)

map_obj = get_map(location = "Champaign", zoom = 13) # city in IL
map = ggmap(map_obj)

ct_points = c(0, 5, 10, 15, 20, 50,100, 10000)
business$review_level = cut(business$review_count, ct_points, labels = c("L1", "L2", "L3", "L4", "L5", "L6", "L7")) #review level defines how many reviews have been written for a place, L7 being highest

map + geom_point(aes(x = longitude, y= latitude, color = factor(review_level), size = review_count*15), data = business, alpha = 0.7) + 
        scale_size_continuous(guide = FALSE) +
        scale_color_manual(name = "# Reviews", 
                           labels = c("low", "", "", "", "", "", "high"),
                           values = c(brewer.pal(n = 7, "PuRd")))

qmplot(longitude, latitude, data = reviews, maptype = "toner-background", color = net_rating) + facet_wrap(~ net_rating) # visual to represent how restuarants with different ratings are distributed across city. Mostof the restuarants are present near center of the city, so location may not be a very promising factor to be used for feature engineering. Lets quickly check whether it differs from ratings in reviews marked as useful

qmplot(longitude, latitude, data = useful_reviews, maptype = "toner-background", color = net_rating) + facet_wrap(~ net_rating) # distribution of ratings seems similar for both useful and non-useful reviews


#------------------ Step 1 - Assumption testing

## Assumption 01 - Longer reviews would be more useful and would have extreme ratings
# Result - Reviews with 1 or 2 ratings are longest

reviews$review_len = sapply(reviews$text, function (x) str_count(x)) # entire dataset
useful_reviews$review_len = sapply(useful_reviews$text, function(x) str_count(x)) # useful reviews 

rev_len = cbind(tapply(reviews$review_len, reviews$user_rating, mean) , tapply(reviews$review_len[reviews$useful>0], reviews$user_rating[reviews$useful>0], mean))
rev_len = as.data.frame(rev_len)
colnames(rev_len) = c("All_Reviews", "Useful_Reviews")
rev_len$rating = row.names(rev_len)

rev_len %>% gather(key = "key", value = "Value", 1:2) %>% 
        ggplot(aes(x = rating,  y= Value, group = key)) + 
        geom_line(aes(col = key), size = 1.5) +
        labs(y = "Review Length", x = "User Rating", title = "Mean Review Length vs User rating") +
        theme(legend.title = element_blank()) +
        theme_minimal() # Length of useful reviews is greater than average length of all the reviews but both follow a similar distribution i.e. reviews with rating 1 and 2 are longest. This can also be infered from density plots below


reviews %>% select(business_id, review_count, review_len, user_rating) %>% unique() %>% 
        ggplot(aes(x = review_len)) +
        geom_density(aes(fill=factor(user_rating, levels = c("1", "2", "3", "4", "5"), ordered = T)),color = "white", alpha = 0.25) +
        theme_minimal() +
        labs(x = "Review Length", y="", title ="Density Plot - Review Length vs Ratings") +
        scale_fill_discrete("Ratings")


useful_reviews %>% select(business_id, review_count, review_len, user_rating) %>% unique() %>% 
        ggplot(aes(x = review_len)) +
        geom_density(aes(fill=factor(user_rating, levels = c("1", "2", "3", "4", "5"), ordered = T)),color = "white", alpha = 0.25) +
        theme_minimal() +
        labs(x = "Review Length", y="", title ="Density Plot - Review Length vs Ratings (Useful Reviews only)") +
        scale_fill_discrete("Ratings")


#--------
## Assumption 02 - High rated business get more reviews
# Result - Restaurants with rating above 3 receive significantly high number of reviews.

review_pct = reviews %>% select(business_id, net_rating, review_count) %>%
        unique() %>%
        group_by(net_rating) %>% summarise(total_reviews = sum(review_count))
review_pct$pct_reviews = review_pct$total_reviews/sum(review_pct$total_reviews)
review_pct$net_rating = as.factor(review_pct$net_rating)

restaurants_pct =  reviews %>% select(business_id, net_rating) %>%
        unique() %>%
        group_by(net_rating) %>% summarise(total_restuarants = n())
restaurants_pct$pct_restuarants = restaurants_pct$total_restuarants/sum(restaurants_pct$total_restuarants)
restaurants_pct$net_rating = as.factor(restaurants_pct$net_rating)

inner_join(x = review_pct, y = restaurants_pct, "net_rating") %>% gather(key = "feature", value = "pct", c(pct_reviews, pct_restuarants)) %>% 
        ggplot(aes(x = net_rating, y= pct)) +
        geom_bar(stat = "identity", aes(fill = feature), position = "dodge", alpha = 0.8) +
        theme_minimal() +
        labs(x = "Ratings", y = "", title = "# Reviews written vs # Restaurants") +
        theme(legend.title = element_blank())
        

#--------
## Assumption 03 - Most reviews would be submitted on weekends
# Result - False

position = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
reviews %>% ggplot(aes(weekday)) +  
        geom_bar() + 
        scale_x_discrete(limits = position) +
        theme_minimal()
# maximum reviews are written on Sundays and Mondays, but there is not a significant difference

#--------
## Assumption - 04 - Ratings are impacted by mood of people. Or people tend to give higher ratings on weekends
# Result - False

tapply(reviews$user_rating, reviews$weekday, mean) %>%
        plot(type = "h", ylim = c(3,4), col = "red", lwd = 3) #ratings are smilar across days

#---------
## Assumption 05 - few locations are popular on weekends and other on weekdays
# Can not be inferred from results because reviews are written with a delay. From the data, there is no stark distinction

map + 
        geom_jitter(data = reviews, aes(x = longitude, y = latitude, color = is_weekend, shape = is_weekend), alpha = 0.5, size = 2) +
        theme(axis.title = element_blank())
        

#--------
## Assumption 06 - Resturants opened till late night will be more popular or vice-versa

open_hours = as.data.frame(str_split(string = reviews$hours, pattern = ",", simplify = T))
open_hours = as.data.frame(lapply(X = open_hours, function(x) str_trim(x)))
open_hours = cbind(reviews$business_id, open_hours)

colnames(open_hours) = c("business_id", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

open_hours = open_hours[,1:8]
open_hours = unique(open_hours)
open_hours = open_hours %>% gather(key = "day", "day_time", 2:ncol(open_hours))
open_hours$day = NULL
open_hours = open_hours %>% separate(col = day_time, into = c("day", "time"), sep =  " ")
open_hours = open_hours %>% separate(time, into = c("open_time", "close_time"), sep = "-")
open_hours = open_hours[!is.na(open_hours$open_time), ]
open_hours$day = str_trim(open_hours$day)

reviews_subset = reviews[, c('business_id', "net_rating", "review_count") ]
reviews_subset = reviews_subset %>% unique()
write.csv(reviews_subset, "reviews_subset.csv", row.names = F)

open_hours = inner_join(x = open_hours, y = reviews_subset, by = "business_id")

# is there is relation in day vs close_time vs net_rating
extract_string = function(x){
        open = 1
        pos = (length(x) - str_locate(string = x, pattern = ":")[,1])
        end = length(x) - pos - 1
        str_sub(string = x, start = open, end = end)
}

open_hours$open_time_numeric = as.data.frame(lapply(open_hours[,3:4], function(x) extract_string(x)))[,1]
open_hours$close_time_numeric = as.data.frame(lapply(open_hours[,3:4], function(x) extract_string(x)))[,2]

open_hours$open_time = NULL
open_hours$close_time = NULL
open_hours$open_time_numeric = as.integer(as.character(open_hours$open_time_numeric))
open_hours$close_time_numeric = as.integer(as.character(open_hours$close_time_numeric))
open_hours$day = factor(open_hours$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = T)
open_hours = open_hours %>% arrange(day)


# Overall impact of closing time on restaurant's rating
open_hours %>% select(close_time_numeric, net_rating) %>% 
        group_by(close_time_numeric) %>% summarise(average_rating = mean(net_rating)) %>% 
        ggplot(aes(x = close_time_numeric, y= average_rating)) +
        geom_density(stat = "identity", fill = "orange", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(x = "Closing Time", y= "Average Rating", title = "Closing Time vs Average Rating")

# Impact of closng time vs day vs ratings
# Food joints closing after dinner seems to have slightly higher average rating
open_hours %>% select(day, close_time_numeric, net_rating) %>% 
        group_by(day, close_time_numeric) %>% summarise(average_rating = mean(net_rating)) %>% 
        ggplot(aes(x = close_time_numeric, y= average_rating)) +
        geom_density(stat = "identity", aes(fill = day, color = day), alpha = 0.8) +
        theme_minimal() +
        facet_wrap(~day) +
        theme(legend.position = "none") +
        labs(x = "Closing Time", y= "Average Rating", title = "Closing Time vs Average Rating")
        
# Impact of closing time on number of reviews
# Speciality joins focused on either lunch, dinner, breakfast, or late parties have attract more reviews
open_hours %>% select(close_time_numeric, review_count) %>% 
        group_by(close_time_numeric) %>% summarise(avg_review_count = mean(review_count)) %>% 
        ggplot(aes(x = close_time_numeric, y= avg_review_count)) +
        geom_density(stat = "identity", fill = "orange", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(x = "Closing Time", y= "# Reviews", title = "Closing Time vs Average # Reviews")

# Hour by hour comparision of average rating vs closing time
open_hours %>% select(day, close_time_numeric, net_rating) %>% 
        group_by(day, close_time_numeric) %>% summarise(average_rating = mean(net_rating)) %>% 
        ggplot(aes(x = day, y = close_time_numeric, fill = average_rating)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low ="red", high = "green") +
        theme_minimal() +
        theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 90)) +
        labs(y = "Closing time", title = "Rating variation with Closing time")

# creating buckets of time - Early, After dinner, Very late
time_cuts = c(-1, 11, 18, 22, 25)
open_hours$close_time_bucket = cut(open_hours$close_time_numeric, breaks = time_cuts, labels = c("Late Night Joints/Clubs", "Lunch/Early Dinner", "Family Dinner", "Late Night Joints"))

open_hours %>% select(day, close_time_bucket, net_rating) %>% 
        group_by(day, close_time_bucket) %>% summarise(average_rating = mean(net_rating)) %>% 
        ggplot(aes(x = day, y = close_time_bucket, fill = average_rating)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low ="red", high = "green") +
        theme_minimal() +
        theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 90)) +
        labs(y = "Closing time", title = "Rating variation with Closing time")
# Resturants closing early or after dinner time have higher ratings on an average 
# on all the days, resturants closing early have higher median rating.

#---------------
## Assumption 07 - People will usually checkin at end of the day
# Result - Correct!

checkin_time = checkin %>% arrange(checkin_time)
checkin_time$checkin_time = as.integer(checkin_time$checkin_time)
checkin_time$checkin_count = as.integer(checkin_time$checkin_count)
checkin_time = checkin_time %>% arrange(checkin_time)
checkin_time$checkin_time = factor(checkin_time$checkin_time, ordered = T)
checkin_time = checkin_time[!is.na(checkin_time$checkin_count),]
checkin_time = checkin_time %>% arrange(checkin_time, checkin_count)

checkin_time %>% ggplot(aes(x = checkin_time, y = checkin_count)) +
        geom_bar(stat = "identity", fill = "orange") +
        theme_minimal() +
        labs(x = "Hour of the day", 
             y = "# Check-ins",
             title = "# check-ins vs hour of the day")

#-------------
## Assumption 08 - People generally make a checkin into a good resturant
#Assumption - Checkin data was collected from a social network

total_checkin = checkin_time %>% group_by(business_id) %>% summarise(total_checkin = sum(checkin_count, na.rm = T))
total_checkin = inner_join(x = total_checkin, y = reviews_subset, "business_id")

total_checkin %>% ggplot(aes(x = review_count, y = total_checkin, fill = net_rating)) +
        geom_point() +
        xlim(c(0, 300)) + ylim(0, 1000) +
        theme_minimal() #all the points are near zero. Using box cox transformation to standardize the data

bc_total_checkin = BoxCoxTrans(y = total_checkin$total_checkin)
bc_review_count = BoxCoxTrans(y = total_checkin$review_count)
total_checkin$total_checkin_bc = predict(bc_total_checkin, total_checkin$total_checkin)
total_checkin$review_count_bc = predict(bc_review_count, total_checkin$review_count)

total_checkin %>% ggplot(aes(x = review_count_bc, y = total_checkin_bc)) +
        geom_point(aes(color = net_rating), size = 2, pch = 19) +
        scale_color_gradient(low = "red", high = "green") +
        theme_minimal() +
        labs(x = "# Reviews",
             y = "# Check-ins",
             title = "# Check-ins vs # Reviews(Standardized Data)") +
        theme(axis.text = element_blank()) +
        geom_smooth()


#--------- 

## Assumption 09 - Few attributes have a higher impact on ratings
# Results - Top 50 attributes impacting the ratings can be identified and converted to features

business_attributes = business %>% select(business_id, attributes)
business_attributes = unique(business_attributes)

business_attributes_ = business_attributes$attributes %>% str_split(pattern = ",", simplify = T)
business_attributes = cbind(business_attributes, business_attributes_)
business_attributes = as.data.frame(business_attributes)

business_attributes$attributes = NULL
business_attributes = business_attributes %>% gather(key = "key", value = "attribute", 2:ncol(business_attributes))
business_attributes$key = NULL

business_attributes = business_attributes %>% 
        separate(col = attribute, into = c("attribute_property", "value"), sep = ":")

business_attributes = business_attributes[business_attributes$attribute_property != "",]
business_attributes$value = str_trim(business_attributes$value)

business_attributes$attribute_property %>% unique() %>% length() #86 distinct attributes in this subset of data

business_attributes$value %>% unique() # a lot of attribute entries seems repetitive
business_attributes$value[business_attributes$value == "yes"] = TRUE
business_attributes$value[business_attributes$value == "True"] = TRUE
business_attributes$value[business_attributes$value == "no"] = FALSE
business_attributes$value[business_attributes$value == "none"] = FALSE
business_attributes$value[business_attributes$value == "False"] = FALSE

business_attributes = business_attributes %>% left_join(y = reviews_subset, by = "business_id")
business_attributes = business_attributes[, c("business_id", "net_rating", "review_count", "attribute_property", "value")]

business_attributes = business_attributes %>% spread(key = attribute_property, value = value)
business_attributes[is.na(business_attributes)] = FALSE

## lets check which attributes are most important

business_attributes$net_rating = factor(business_attributes$net_rating)
x = business_attributes[, 4:ncol(business_attributes)]
y = business_attributes$net_rating

x = lapply(x, as.factor)
x = as.data.frame(x)

rf1 = train(x = x, y = y, data = business_attributes, method = "rf",
            trControl = trainControl("cv", number = 5))

plot(varImp(rf1), top = 30) # noise level and price range have greatest impact on ratings followed by WiFi, Wheelchair accessible, outdoor seating,  parking lot, Delivery option, and alcohol seems to be the attributes having greatest impact on ratings

#----------------------- Sentiment Analysis

## Assumption 10 - Some key factors hurt the ratings most
# Customer service and friendly staff can potentially drive the rating up or down

reviews_rating = reviews %>% select(name, user_rating, text, review_id, user_id)
useful_reviews_rating = useful_reviews %>% select(name, user_rating, text, review_id, user_id)

custom_reader <- readTabular(mapping = list(content="text",id="review_id",author="user_id"))

create_text_corpus = function(df = reviews_rating){

        reviews_rating_1 = df[df$user_rating==1,]
        reviews_rating_2 = df[df$user_rating==2,]
        reviews_rating_3 = df[df$user_rating==3,]
        reviews_rating_4 = df[df$user_rating==4,]
        reviews_rating_5 = df[df$user_rating==5,]
        
        text_corpus_1 <- VCorpus(DataframeSource(reviews_rating_1), readerControl = list(reader=custom_reader))
        text_corpus_2 <- VCorpus(DataframeSource(reviews_rating_2), readerControl = list(reader=custom_reader))
        text_corpus_3 <- VCorpus(DataframeSource(reviews_rating_3), readerControl = list(reader=custom_reader))
        text_corpus_4 <- VCorpus(DataframeSource(reviews_rating_4), readerControl = list(reader=custom_reader))
        text_corpus_5 <- VCorpus(DataframeSource(reviews_rating_5), readerControl = list(reader=custom_reader))

        return(list(text_corpus_1, text_corpus_2, text_corpus_3, text_corpus_4, text_corpus_5))
}

clean_corpus  = function(corpus){
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus <- tm_map(corpus, content_transformer(tolower)) 
        #corpus <- tm_map(corpus, removeNumbers)
        new_stopwords=c("food","place", "will")
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"),new_stopwords))
        return(corpus)
}

create_wordcloud = function(corpus, color = "Reds") {
        pal = brewer.pal(n = 7, name = color)
        corpus = clean_corpus(corpus)
        tokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 3))}
        corpus_tdm = TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
        corpus_tdm = removeSparseTerms(x = corpus_tdm, sparse = 0.99)
        corpus_tdm = as.data.frame(as.matrix(corpus_tdm))
        corpus_tdm_freq = rowSums(corpus_tdm)
        wordcloud(names(corpus_tdm_freq), freq = corpus_tdm_freq, scale = c(2.5, 0.25), colors = pal, min.freq = 5) 
}

color_pal = c("Reds", "YlOrRd", "YlGnBu", "GnBu", "Greens")
all_review_text_corpus_list = create_text_corpus()
useful_review_text_corpus_list = create_text_corpus()

lapply(all_review_text_corpus_list, function(x) create_wordcloud(corpus = x)) # lets check if text in useful reviews is different
lapply(useful_review_text_corpus_list, function(x) create_wordcloud(corpus = x, color = "Greens"))


# 1-star = customer serive and time taken to bring the order seems to be biggest factor for 1 reviews
# 2- Star = again customer service seems to be the biggest factor, but few of the comments are neutral and have highlighted progress
# 3- Star = Positive and neutral sentiments dominate reviews 3 stars
# 4- star = Friendly staff, reasonable price, good service, and ice cream !
# 5-star = great service, friendly staff, some specality, and ice cream again !

#----------------- Sentiment Analysis
## Assumption 11 - Positive to negative word ratio increases with user rating
# Sentiment score and postive to negative word count can also be used as a feature for further analysis 

afinn_dict = get_sentiments("afinn") # getting sentiment dictionary
afinn_dict$score[afinn_dict$score <0] = afinn_dict$score[afinn_dict$score <0]*2

review_text = reviews %>% select(review_id, text)
review_text$text = as.character(review_text$text)

review_sentiment = review_text %>%
        unnest_tokens(output = word, input = text) %>% 
        anti_join(stop_words) %>% 
        inner_join(afinn_dict) %>% 
        group_by(review_id) %>%
        mutate(pos_words = ifelse(as.integer(score) >0, 1, 0), neg_words = ifelse(as.integer(score) <0, 1, 0)) %>% 
        summarise(sentiment_score = sum(as.integer(score)), pos_words = sum(pos_words), neg_words = sum(neg_words))

review_sentiment$pos_neg_word_diff = review_sentiment$pos_words - review_sentiment$neg_words

review_sentiment = review_sentiment %>% 
        inner_join(y = unique(reviews[,c("review_id", "user_rating")]), by = "review_id")

review_sentiment %>% ggplot(aes(x = factor(user_rating), y = sentiment_score)) +
        geom_boxplot(aes(fill = factor(user_rating))) +
        theme_minimal() +
        labs(x = "User Rating", y = "Sentiment Score") +
        scale_fill_discrete(guide = F) #there are few reviews rated one with very positive sentiment score. these may need further investigation

review_sentiment %>% ggplot(aes(x = factor(user_rating), y = pos_neg_word_diff)) +
        geom_boxplot(aes(fill = factor(user_rating))) +
        theme_minimal() +
        labs(x = "User Rating", y = "Positive to Negative Words") +
        scale_fill_discrete(guide = F) #reviews rated three or lower has more negative words. Also, revies rated 1 or 5 have extreme number of e=negative or positive words


#--------------------
## Assumption 12 - Users with certain attributes give a higher overall rating

user$yelping_since = as.Date(user$yelping_since)
user$yelping_since_days = as.numeric(today() - user$yelping_since) 
user$elite_count = str_count(user$elite, ",") +1 # counting number of times user was classified as elite
user$elite_count[is.na(user$elite_count)] = 0
user_numeric = user[,sapply(user, is.numeric)]


y_var = "average_stars"
x = user_numeric[, !(colnames(user_numeric) %in% y_var)]
y = user_numeric$average_stars

rf_user = train(average_stars~., data = user_numeric, method = "rf", 
                trControl = trainControl(method = "cv", number = 5), importance = T)

plot(varImp(rf_user)) # review count, compliment hot, and useful seems to be the most related attributes

#-------------

"Key Inferences -

1. Reviews with rating 1 or 2 are longest. Reviews with greater length are thereforemore critical
2. Joints with rating above 3 have significantly more # reviews
3. Speciality joints focused on either lunch, dinner, breakfast, or late parties attract more reviews
4. Joints closing after dinner have higher ratings, probabaly because they offer better services
5. # reviews and checkin for a place are correlated. Business having more checkin is also expected to be more popular
6. While joints are concentrated in some parts of the city, using location as a feature to determine rating may not work well. This will impact how we cluster the food joints to recommend next steps
7. There are more checkins in late hours. Crowd be the reason why joints serving till late night may not be able to provide good service, leading to lower net rating. This will also impact the user feedback on  sumitted review
8. There are few attributes including noise level, accessibility, and alcohol that impact rating the most.
9. Customer service and friendliness of staff are most important factors for receiving a positive review
10. Positive to negative word ratio and sentiment score complement each other

"
