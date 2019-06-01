#Facebook Text Data Analysis - Bigrams----

#libraries----
library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(SnowballC)
library(igraph)
library(ggraph)
library(stringr)

#read in data----
raw_data <- read_csv("FB_Mined_Data_Stripped.csv",
                     locale = locale(encoding = "ASCII"))
names(raw_data) <- c("text", "domain")
View(raw_data)
#raw_df <- as.data.frame(raw_data) don't run this unless specifically needed

#format data to tidytext----
tidy_ngrm <- raw_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) #bigrams, no stopwords removed

#split bigrams and remove stopwords----
bigrams_separated <- tidy_ngrm %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) #bigrams_filtered is now clean

bigrams_filtered$word1 <- gsub("\\s+","",bigrams_filtered$word1) #remove whitespaces
bigrams_filtered$word2 <- gsub("\\s+","",bigrams_filtered$word2)

#stemmed bigrams----
bigram_stem <- bigrams_filtered %>%
  mutate_at(c("word1", "word2"), funs(wordStem((.), language="en")))

#unite the bigrams back together----
bigrams_united <- bigram_stem %>%
  unite(bigram, word1, word2, sep = " ") 

#bigram counts----
bigram_freq_dom <- bigrams_united %>%
  count(domain, bigram, sort = TRUE) #split by domain

bigram_freq_all <- bigrams_united %>%
  count(bigram, sort = TRUE) #not split by domain

bigram_dom_total <- bigram_freq_dom %>% 
  group_by(domain) %>% 
  summarize(total = sum(n)) #totals for each domain, no actual bigrams

#plot bigram top 10----
bigram_freq_all %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(bigram, -n), y = n)) + 
  geom_bar(stat = "identity", fill="mediumpurple3", colour="black") +
  geom_text(aes(label=n), vjust=1.5) +
  xlab("Bigram") + ylab("Count") + theme(axis.text = element_text(colour="black")) +
  scale_y_continuous(breaks=seq(0,14,2)) +
  ggtitle("Top 10 Bigram Frequencies")

#plotting total bigrams in each domain----
bigram_dom_total %>%
  ggplot(aes(x = reorder(domain, -total), y = total)) + 
  geom_bar(stat = "identity", fill="mediumpurple3", colour="black") +
  geom_text(aes(label=total), vjust=1.5) +
  xlab("Domain") + ylab("Count") + theme(axis.text = element_text(colour="black"))+
  ggtitle("Bigram Totals by Domain") + scale_x_discrete(labels=c("Animals","Finance","Crops","Family","Equipment","Environment","Comm. Tech","Health","Energy","Farm Tech"))

#top bigrams in each domain----
domain1 <- subset(bigram_freq_dom, domain=="1")
domain2 <- subset(bigram_freq_dom, domain=="2")
domain3 <- subset(bigram_freq_dom, domain=="3")
domain4 <- subset(bigram_freq_dom, domain=="4")
domain5 <- subset(bigram_freq_dom, domain=="5")
domain6 <- subset(bigram_freq_dom, domain=="6")
domain7 <- subset(bigram_freq_dom, domain=="7")
domain8 <- subset(bigram_freq_dom, domain=="8")
domain9 <- subset(bigram_freq_dom, domain=="9")
domain10 <- subset(bigram_freq_dom, domain=="10")

#prepare for bigram network----
bigram_net <- bigram_stem[,c('word1', 'word2', 'domain')]

bigram_net <- bigram_net %>%
  count(word1, word2, domain, sort = TRUE)

bigram_net_c <- bigram_net[-c(28),]#removed a word from a row that I did not want included
bigram_net_c <- bigram_net_c[-c(162),] #removed 12 different pairs

#count word occurance----
sort(str_count(bigram_net$word1), decreasing = TRUE)

#bigram network creation----
names(bigram_net_c) <- c("word1","word2","domain","count")

bigram_graph <- bigram_net_c %>%
  filter(count>1) %>% #and can specify by domain=="1"
  graph_from_data_frame()

set.seed(2019)

V(bigram_graph)$degree <- degree(bigram_graph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = count), width=1) +
  geom_node_point(aes(size = degree), colour = 'mediumpurple3') +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggforce::theme_no_axes() +
  labs(title = "Bigram Network")
