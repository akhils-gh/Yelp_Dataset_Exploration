library(tidyverse)
library(stringr)
library(tidytext)
library(reshape2)

reviews = read.csv("E:\\Projects\\R\\Yelp\\data\\reviews_clean.csv", nrows = 5)

review_text = reviews[c("review_id", "text")]
review_text$text = as.character(review_text$text)
review_text$text = tolower(review_text$text)

#------------------- Creating aspects data frame
price=c("value","worth","price","quality", "expensive", "cheap", "budget")
service=c("service","waiter","help", "manager", "table", "floor")
food=c("chicken","breakfast","dinner","food","lunch", "dessert", "side", "menu")
ambience=c("view","lights", "atmosphere", "mood", "feel", "tone", "look", "confortable")

aspect_df = melt(list("price" = price, "service" = service, "food" = food, "ambience" = ambience))
colnames(aspect_df) = c("term", "aspect")

aspect_df = transform(aspect_df,aspect_id=as.numeric(factor(aspect)))
aspect_df$aspect_id = as.character(aspect_df$aspect_id)


#------------------- Creating Vocabulary
vocab = review_text %>% 
        unnest_tokens(word, text) %>% 
        anti_join(stop_words) %>% 
        group_by(word) %>% 
        summarise(word_count = n())

#------------------ Splitting Reviews into sentences        
sentence_split <- function (df) {
        df = cbind(df$review_id, as.data.frame(str_split_fixed(string = df$text, pattern = "[\\.!?]+", n = Inf)))
        df = df %>% gather(key = "key", value = "text", 2:ncol(df))
        df$key = NULL
        return(df)
}

review_text = sentence_split(review_text)
colnames(review_text) = c("review_id", "sentence") 

review_text$sentence_id = seq(nrow(review_text)) #adding sentence id 
review_text$sentence = as.character(review_text$sentence)

#----------------- Getting count of each word in each sentence
RSWFrequencey = review_text %>% 
        unnest_tokens(word, sentence) %>% 
        group_by(review_id, sentence_id, word) %>% 
        summarise(word_count = n())

RSWFrequencey = RSWFrequencey %>% anti_join(y = stop_words, by = "word") # removing stop words

#-----------------------------------------------------------------------------------
#------- Functions for Aspect Segmentation -----------------------------------------
#-----------------------------------------------------------------------------------

#------------ Function 01 - Getting aspect id for each word

assign_word_aspect = function(rswf_df, asp_df) {
        rswf_df = rswf_df %>% 
                left_join(y = asp_df, by = c("word" = "term")) %>% 
                select(-aspect)

        rswf_df$aspect_id[is.na(rswf_df$aspect_id)] = 0 # replacing NA aspect ID with 0
        return(data.frame(rswf_df))
}

#---------Function 02 - Assigning aspect Id to each sentence

assign_sentence_aspect = function(rs_wa_df){     
        # Getting senteces with positive matches to aspect list
        NZS_aspect = rs_wa_df %>% group_by(sentence_id, aspect_id) %>% 
                summarise(word_sum = sum(word_count)) %>% 
                filter(aspect_id != "0") %>% 
                filter(word_sum == max(word_sum)) %>% 
                data.frame()
        
        # Getting senteces with 0 matches to aspect list
        ZS_aspect = rs_wa_df %>% group_by(sentence_id, aspect_id) %>% 
                summarise(word_sum = sum(word_count)) %>% 
                filter(aspect_id == "0") %>% 
                data.frame() %>% anti_join(y = NZS_aspect, by = "sentence_id")
        
        rsa_wa_df = union(NZS_aspect, ZS_aspect)
        return(rsa_wa_df)
}


#----------- Function 03 - Getting Chi Stat value of each word

get_chi_df = function(rs_wa_df, rsa_wa_df, asp_df){
        review_text_chi = rsa_wa_df %>% select(sentence_id, aspect_id) %>% 
                inner_join(y = rs_wa_df, by = c("sentence_id", "aspect_id")) %>% 
                select(aspect_id, sentence_id, word, word_count) %>% 
                data.frame()
        
        unique_words = unique(review_text_chi$word)
        unique_aspect_id = c(unique(asp_df$aspect_id),0)
        
        
        # calculating chi sq characterstics
        chi_df = NULL
        
        for (i in unique_words){
                for (j in unique_aspect_id) {
                        c1 = sum(review_text_chi$word_count[review_text_chi$aspect_id == j & review_text_chi$word ==i])
                        c2 = sum(review_text_chi$word_count[review_text_chi$aspect_id != j & review_text_chi$word ==i])
                        c3 = length(unique(review_text_chi$sentence_id[review_text_chi$aspect_id == j & review_text_chi$word !=i]))
                        c4 = length(unique(review_text_chi$sentence_id)) - (length(unique(review_text_chi$sentence_id[review_text_chi$aspect_id == j | review_text_chi$word ==i])))
                        c = sum(review_text_chi$word_count[review_text_chi$word ==i])
                        temp_df = data.frame(word = i, aspect_id = j, c= c, c1 = c1, c2 = c2, c3 =c3, c4 = c4)
                        chi_df = rbind(chi_df, temp_df)
                        }
        }
        
        chi_df$chi = (chi_df$c*((chi_df$c1*chi_df$c4 - chi_df$c2*chi_df$c3)^2))/((chi_df$c1 + chi_df$c3)*(chi_df$c2 + chi_df$c4)*(chi_df$c1 + chi_df$c2)*(chi_df$c3 + chi_df$c4))
        return(chi_df)
}

#---------- Function 04 - Get new words to be added to each aspect list

get_aspect_words = function(n, asp_df, ch_df) {
        temp_aspect_df = ch_df %>% group_by(aspect_id) %>% 
                arrange(aspect_id, desc(chi)) %>% 
                mutate(rank = seq_along(aspect_id)) %>% 
                filter(!(word %in% asp_df$term)) %>% 
                filter(rank <n & is.na(chi) == FALSE & aspect_id  != 0) %>% 
                select(word, aspect_id) %>% 
                data.frame()
        
        return(temp_aspect_df)
}


#----------- Function 05 - Update aspect word lists with new words

update_aspect_wordlist = function(n, asp_df, additional_aspect_words){
        colnames(additional_aspect_words) = c("term", "aspect_id")
        tba_aspect_words = additional_aspect_words %>% left_join(y =unique(asp_df[c("aspect_id", "aspect")]), by = "aspect_id")
        asp_df = rbind(asp_df, tba_aspect_words)
        
        return(asp_df)
}

#------------ Running loop to assign each word and sentence to particular aspect

for(counter in 1:10){
        RS_WA_df = assign_word_aspect(rswf_df = RSWFrequencey, asp_df = aspect_df)
        RSA_WA_df = assign_sentence_aspect(rs_wa_df = RS_WA_df)
        chi_df = get_chi_df(rs_wa_df = RS_WA_df, rsa_wa_df= RSA_WA_df, asp_df= aspect_df)
        additional_aspect_words = get_aspect_words(n = 10, asp_df = aspect_df, ch_df = chi_df)
        aspect_df = update_aspect_wordlist(n = 10, asp_df = aspect_df, additional_aspect_words= additional_aspect_words)
}