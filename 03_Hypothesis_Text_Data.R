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

#--------- 

## Hypothesis 09 - Few attributes have a higher impact on ratings
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

#-----------------------
## Hypothesis 10 - Some key factors hurt the ratings most
# Customer service and frindly staff can potentially drive the rating up or down

reviews_rating = reviews %>% select(name, user_rating, text, review_id, user_id)

custom_reader <- readTabular(mapping = list(content="text",id="review_id",author="user_id"))

pal <- brewer.pal(9,"Reds")
pal <- pal[-(1:4)]

reviews_rating_1 = reviews_rating[reviews_rating$user_rating==1,]
reviews_rating_2 = reviews_rating[reviews_rating$user_rating==2,]
reviews_rating_3 = reviews_rating[reviews_rating$user_rating==3,]
reviews_rating_4 = reviews_rating[reviews_rating$user_rating==4,]
reviews_rating_5 = reviews_rating[reviews_rating$user_rating==5,]

text_corpus_1 <- VCorpus(DataframeSource(reviews_rating_1), readerControl = list(reader=custom_reader))
text_corpus_2 <- VCorpus(DataframeSource(reviews_rating_2), readerControl = list(reader=custom_reader))
text_corpus_3 <- VCorpus(DataframeSource(reviews_rating_3), readerControl = list(reader=custom_reader))
text_corpus_4 <- VCorpus(DataframeSource(reviews_rating_4), readerControl = list(reader=custom_reader))
text_corpus_5 <- VCorpus(DataframeSource(reviews_rating_5), readerControl = list(reader=custom_reader))
        
clean_corpus  = function(corpus){
                corpus <- tm_map(corpus, removePunctuation)
                corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
                corpus <- tm_map(corpus, content_transformer(tolower)) 
                #corpus <- tm_map(corpus, removeNumbers)
                new_stopwords=c("food","place", "will")
                corpus <- tm_map(corpus, removeWords, c(stopwords("en"),new_stopwords))
                return(corpus)
}


create_wordcloud = function(corpus, color) {
                pal = brewer.pal(n = 7, name = color)
                corpus = clean_corpus(corpus)
                tokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 3))}
                corpus_tdm = TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
                corpus_tdm = removeSparseTerms(x = corpus_tdm, sparse = 0.99)
                corpus_tdm = as.data.frame(as.matrix(corpus_tdm))
                corpus_tdm_freq = rowSums(corpus_tdm)
                wordcloud(names(corpus_tdm_freq), freq = corpus_tdm_freq, scale = c(2.5, 0.25), colors = pal, min.freq = 5) 
}

create_wordcloud(corpus = text_corpus_1, "Reds") #customer serive and time taken to bring the order seems to be biggest factor for 1 reviews

create_wordcloud(corpus = text_corpus_2, "YlOrRd") #again customer service seems to be the biggest factor, but few of the comments are neutral and have highlighted progress

create_wordcloud(corpus = text_corpus_3, "YlGnBu") # Positive and neutral sentiments dominate reviews 3 stars
create_wordcloud(corpus = text_corpus_4, "GnBu") # Friendly staff, reasonable price, good service, and ice cream !
create_wordcloud(corpus = text_corpus_5, "Greens") # great service, friendly staff, some specality, and ice cream again !


#-----------------
## Hypothesis 11 - Positive to negative word ratio increases with user rating
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

#-------------