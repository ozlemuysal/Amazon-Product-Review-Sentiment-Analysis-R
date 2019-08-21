

#	Load prerequistive libraries----
#


# Amazon libraries
library(devtools)
devtools::install_github("56north/Rmazon")
library(Rmazon)

library(dplyr)
library(stringi)
library(tm)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(mgsub)
library(scales) 
library(stringr)
library(textdata)
library(widyr)

# Scrape ----
# Set your working directory with setwd() command as your project home directory.

#
# Following code scrapes Amazon product review data, 
# B0117RGG8E : Amazon Product ID for Bose.
# You can find the Product ID in the url when you are in the product page.
# For this example, product url is https://www.amazon.com/Bose-SoundLink-around-ear-wireless-headphones/dp/B0117RGG8E

bose.headphones.reviews <- get_reviews("B0117RGG8E")

# You can save the data set for later use
save(bose.headphones.reviews, file = "data/bose.headphones.reviews.RData")


# You can load the saved data with the following code 
load("data/bose.headphones.reviews.Rdata")

# Ignore the reviewers that didn't buy the product. 
# Verified_Purchase should be TRUE
bose.headphones.reviews <- filter(bose.headphones.reviews, Verified_Purchase==TRUE)



# Clean the reviews text, prepare for analysis.----
#			 

# Load "Stop Words" from the tidytext package
data("stop_words")


bose.headphones.reviews.text <- bose.headphones.reviews %>%
  select(reviewText)

# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- sapply(bose.headphones.reviews.text$reviewText,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))

# Lowecase all text
bose.headphones.reviews.text$reviewText <- tolower(bose.headphones.reviews.text$reviewText)

# make wasn't=was not, can't=can not, etc..
bose.headphones.reviews.text$reviewText <- gsub("wasn[\u2019']t", "was not", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("won[\u2019']t", "will not", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("can[\u2019']t", "can not", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("didn[\u2019']t", "did not", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("don[\u2019']t", "do not", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("I[\u2019']m", "I am", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("[\u2019']ve", " have", bose.headphones.reviews.text$reviewText) 
bose.headphones.reviews.text$reviewText <- gsub("[\u2019|']s", "", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("[\u2019']re", " are", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("[\u2019']ll", " will", bose.headphones.reviews.text$reviewText)

# If you view common typos during your analysis, fix them here.
bose.headphones.reviews.text$reviewText<- gsub("canceling", "cancelling", bose.headphones.reviews.text$reviewText)
bose.headphones.reviews.text$reviewText <- gsub("cancellation", "cancelling", bose.headphones.reviews.text$reviewText)

# omit the following two lines if you have not loaded the tm package
# Remove numbers in the text
bose.headphones.reviews.text$reviewText <- removeNumbers(bose.headphones.reviews.text$reviewText)
# Remove punctuations in the text
bose.headphones.reviews.text$reviewText <- removePunctuation(bose.headphones.reviews.text$reviewText)


# Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)
# Run the following to view Shifted sentiments sorted by polarity point
shifted.words <- bose.headphones.reviews.text %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))%>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))

shifted.words

# Pick the most effective sentiment shifters
negated.phrases <- c("not worth", 
                     "not noise",
                     "no issues",
                     "no complaints",
                     "not disappoint",
                     "not disappointed",
                     "not cheap",
                     "no regrets"
                     
)
# Find synonyms for the phrases above to replace
synonyms <- c("expensive",
              "functional",
              "cool",
              "satisfied",
              "satisfied",
              "satisfied",
              "expensive",
              "satisfied"
)
# Replace the negations with their synonyms.
bose.headphones.reviews.text <- mgsub(bose.headphones.reviews.text$reviewText, negated.phrases, synonyms) %>%
  dplyr::as_data_frame() %>%
  rename(reviewText = value)


# if you want to ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("sound", "bose", "headphones","noise", "soundlink"))

# create the words freq table
word.freq.table<- bose.headphones.reviews.text %>% 
  unnest_tokens(word, reviewText) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
word.freq.table

# Plotting a Wordcloud
word.freq.table %>% 
  filter(n>40) %>%
  with(wordcloud(word, n,
                 scale = c(5,0.3),
                 colors = brewer.pal(8, "Dark2")))


# Most Common Bigrams
bose.headphones.reviews.text %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  filter(n>7) %>%
  unite(word, word1:word2, sep = " ") %>%
  with(wordcloud(word, n,
                 scale = c(3,0.5),
                 colors = brewer.pal(8, "Dark2")))


# Most common Positive and Negative words using Bing
bose.headphones.reviews.text %>% 
  unnest_tokens(word, reviewText) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(percent = round(n/sum(n), 3)) %>%
  ggplot(aes(x = word, y = percent, fill = sentiment, label = percent)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  geom_text(aes(y = 0.7*percent)) +
  labs(title = "Bose Headphones Word Polarity (bing)") +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Bing
bing.mean.score <- word.freq.table %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  summarise(mean = mean(sentiment))

# rescale the range to 5 star range.
bing.mean.score<-rescale(bing.mean.score$mean, to = c(1,5), from = c(-1,1))
  

# Afinn scores are from -5 to 5.
afinn.mean.score <- word.freq.table %>% 
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean = mean(value))

# rescale the range to 5 star range.
afinn.mean.score<-rescale(afinn.mean.score$mean, to = c(1,5), from = c(-5,5))


  

# Correlation Terms
# The correlation of appearing together in a review
bose.correlation.terms <- bose.headphones.reviews.text %>%
  mutate(review = row_number()) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word) %>%
  filter(n() >= 5)%>%
  pairwise_cor(word, review, sort = TRUE)
bose.correlation.terms

bose.correlation.terms %>%
  filter(correlation >= 0.50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "igraph", algorithm = "kk") +
  geom_edge_link(aes(alpha = correlation), 
                 show.legend = FALSE)+
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


bigrams.network.df<-bose.headphones.reviews.text %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 5)

bigrams.network <- graph_from_data_frame(bigrams.network.df)

# now we find the centrality measures of the network
# degree:the number of its adjacent edges (measure of direct influence)
deg <- degree(bigrams.network, mode = "all")

#K-core decomposition allows us to identify the core and the periphery of the network. A k-core is a maximal subnet of a network such that all nodes have at least degree K.
core <- coreness(bigrams.network, mode = "all")

# betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
betw <- betweenness(bigrams.network)

#Eigenvector centrality is a measure of being well-connected connected to the well-connected. First eigenvector of the graph adjacency matrix. Only works with undirected networks.
eigen <- eigen_centrality(bigrams.network, directed = TRUE)
members <- cluster_walktrap(bigrams.network)

bigrams.network <- simplify(bigrams.network, 
                            remove.multiple = FALSE,
                            remove.loops = TRUE)
V(bigrams.network)$color <- members$membership+1

# Using "Coreness" as size
# Coreness -> mean (average distance to all the other nodes, diffusion of information)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = core*10, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (Bose Headphones)"
)
mtext("Coreness")

# Using "Degree" as size
# degree=mode (number of edges of the node, in-degree:prestige

plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = deg, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (Bose Headphones)"
)
mtext("Degree")

# Using "Eigenvector Centrality" as size
# centrality (the most connected words)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = eigen$vector*20, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "black",
     main = "Bigram Communities (Bose Headphones)"
)
mtext("Eigenvector Centrality")

# Using "Betweenness" as size
# Betweenness -> median (weighted # of paths going through the node)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = betw, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "lightgrey",
     main = "Bigram Communities (Bose Headphones)"
)
mtext("Betweenness")


