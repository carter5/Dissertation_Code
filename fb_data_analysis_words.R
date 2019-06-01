#Facebook Text Data Analysis - Words----

#libraries----
library(ggplot2)
library(readr)
library(tidytext)
library(dplyr)
library(SnowballC)

#read in data----
raw_data <- read_csv("FB_Mined_Data_Stripped.csv", locale = locale(encoding = "ASCII"))
View(raw_data)
names(raw_data) <- c("text", "domain")
#raw_df <- as.data.frame(raw_data)

#format data to tidytext----
tidy_word <- raw_data %>%
  unnest_tokens(word, text) #no stopwords removed

#remove stopwords----
tidy_word_c <- tidy_word %>%
  anti_join(stop_words) #can also make own vector list of stopwords

#count word frequencies----
tidy_word_freq <- tidy_word %>%
  count(word) %>%
  arrange(desc(n))
tidy_word_c_freq <- tidy_word_c %>%
  count(word) %>%
  arrange(desc(n)) #'c' denotation means cleaned version

#tidy_word_c_freq_df <- as.data.frame(tidy_word_c_freq)
#write_csv(tidy_word_c_freq_df, "tidy_word_c_freq.csv") if needed to create csv

#remove numbers----
tidy_word <-tidy_word[-grep("\\b\\d+\\b", tidy_word$word),]
tidy_word_c <- tidy_word_c[-grep("\\b\\d+\\b", tidy_word_c$word),]

#remove whitespaces----
tidy_word$word <- gsub("\\s+","",tidy_word$word)
tidy_word_c$word <- gsub("\\s+","",tidy_word_c$word)

#stemming----
tidy_word <-tidy_word %>%
  mutate_at("word", funs(wordStem((.), language="en")))
tidy_word_c <-tidy_word_c %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#total words in each domain, total of each word in each domain----
domain_words <- tidy_word %>%
  unnest_tokens(word, word) %>%
  count(domain, word, sort = TRUE)

total_domain_words <- domain_words %>% 
  group_by(domain) %>% 
  summarize(total = sum(n))

tidy_words <- left_join(domain_words, total_domain_words) #non-cleaned text

domain_words_c <- tidy_word_c %>%
  unnest_tokens(word, word) %>%
  count(domain, word, sort = TRUE)

total_domain_words_c <- domain_words_c %>% 
  group_by(domain) %>% 
  summarize(total = sum(n))

tidy_words_c <- left_join(domain_words_c, total_domain_words_c) #cleaned text

#total lines calculations----
tidy_line <- raw_data %>%
  unnest_tokens(line, text, token = "lines")

total_domain_lines <- tidy_line %>%
  group_by(domain) %>%
  count(domain, line, sort = TRUE) %>%
  summarize(total=sum(n))

#tf-idf----
test <- tf_idf_c[-c(190),]#removed a word from a row that I did not want included
test <- tf_idf_c[-c(17),]
tf_idf_clean <- test

tf_idf_c <- domain_words_c %>%
  bind_tf_idf(word, domain, n) %>%
  arrange(desc(tf_idf))

tf_idf_clean %>%
  group_by(domain) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill=factor(domain))) +
  geom_col(show.legend = FALSE, colour="black", width=0.75) +
  facet_wrap(~ domain, scales = "free", ncol=2, labeller = as_labeller(list)) +
  ylab("Tf-idf Score") + xlab("Word") +
  theme(axis.text = element_text(colour="black"), axis.text.x=element_text(hjust =0.75)) + 
  ggtitle("Tf-idf Scores for the Top Words in each Domain") +
  coord_flip() #visualize top 10 terms for each domain

list <- c(`2`="Animals",`5`="Finance, Economics, and Trade",`1`="Crops and Soils",`10`="Family and Community Affairs",
          `3`="Farm Equipment",`6`="Environment and Natural Resources",`8`="Communication/Information Technology",
          `9`="Family and Community Health/Nutrition",`7`="Farm Technology",`4`="Energy")

#plotting top word frequencies in all domains----
tidy_word_c_freq %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", fill="mediumpurple3", colour="black") +
  geom_text(aes(label=n), vjust=1.5) +
  xlab("Word") + ylab("Count") + theme(axis.text = element_text(colour="black")) +
  ggtitle("Top 10 Word Frequencies")

#plotting total words in each domain----
total_domain_words_c %>%
  ggplot(aes(x = reorder(domain, -total), y = total)) + 
  geom_bar(stat = "identity", fill="mediumpurple3", colour="black") +
  geom_text(aes(label=total), vjust=1.5) +
  xlab("Domain") + ylab("Count") + theme(axis.text = element_text(colour="black")) + 
  ggtitle("Word Totals by Domain") + 
  scale_x_discrete(labels=c("Animals","Finance","Crops","Family","Equipment","Environment","Comm. Tech","Health","Farm Tech","Energy"))

#plotting top word frequencies in each domain----
domain1 <- subset(domain_words_c, domain=="1")
domain2 <- subset(domain_words_c, domain=="2")
domain3 <- subset(domain_words_c, domain=="3")
domain4 <- subset(domain_words_c, domain=="4")
domain5 <- subset(domain_words_c, domain=="5")
domain6 <- subset(domain_words_c, domain=="6")
domain7 <- subset(domain_words_c, domain=="7")
domain8 <- subset(domain_words_c, domain=="8")
domain9 <- subset(domain_words_c, domain=="9")
domain10 <- subset(domain_words_c, domain=="10")

domain1 %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", fill="springgreen4", colour="black") +
  geom_text(aes(label=n), nudge_y =1.0) +
  xlab("Word") + ylab("Count") + theme(axis.text = element_text(colour="black")) +
  ggtitle("Crops and Soils: Top 10 Word Frequencies") +
  coord_flip()
