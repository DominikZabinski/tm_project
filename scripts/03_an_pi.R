# Analysis of parliament interpolations
# libraries ----
library(DBI)
library(tidyverse)
library(textmineR)
library(wordcloud)
library(cluster)
library(textnets)
library(topicmodels)
library(broom)
# functions ----
source("scripts/__functions.R")

dtm_from_text <- function(to_analyze) {
  CreateDtm(doc_vec = to_analyze$text, # character vector of documents
            doc_names = to_analyze$id, # document names
            ngram_window = c(1, 2), # minimum and maximum n-gram length
            stopword_vec = polish_stop_words(),
            lower = FALSE, # lowercase - this is the default value
            remove_punctuation = FALSE, # punctuation - this is the default
            remove_numbers = TRUE, # numbers - this is the default
            verbose = FALSE, # Turn off status bar for this demo
            cpus = 2) # def
}

calc_cdist <- function(dtm, tf_mat) {
  tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
  tfidf <- t(tfidf)
  csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
  csim <- csim %*% t(csim)
  cdist <- as.dist(1 - csim)
  cdist
}

show_top <- function(tf_mat) {
  # a doc_freq?
  tf_mat %>% 
    top_n(n = 10, wt = term_freq) %>% 
    ggplot(mapping = aes(x=term, y=term_freq)) + 
    geom_col() +
    coord_flip() +
    ggtitle("Review Word Counts")
}

calc_clust <- function(cdist, n_clust = 5) {
  hc <- hclust(cdist, "ward.D")
  clustering <- cutree(hc, n_clust)
  clustering
}

show_hierarch <- function(cdist, n_clust = 5) {
  # agglomerative hierarchical clustering
  # Ward's method as a merge rule
  # cut the dendrogram for 5 clusters for example
  clustering <- calc_clust(cdist, n_clust)
  
  plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
       ylab = "", xlab = "", yaxt = "n")
  
  rect.hclust(hc, n_clust, border = "red")
}

# data ----
# data is stored in .sqlite files: pi_* with raw data and tagged_pi_* with stemmed content
pi_files <- list.files("data", pattern = "^(pi_)")
all_pi <- do.call(rbind, lapply(X = pi_files, FUN = function(i){
  mydb <- dbConnect(RSQLite::SQLite(), file.path("data", i))
  this_cont <- dbGetQuery(conn = mydb, statement = "select metadata.*, ipcontent.content from ipcontent left join metadata on metadata.id = ipcontent.id")
  dbDisconnect(mydb)
  this_cont$period <- gsub("pi_", replacement = "", x = gsub(pattern = ".sqlite", replacement = "", x = i))
  this_cont
}))

glimpse(all_pi)

# add information about length of each document
all_pi <- all_pi %>% 
  mutate(len = nchar(CONTENT))

# basic information
all_pi %>% group_by(period) %>% 
  summarise(count = n(), autohrs = length(unique(AUTHOR)), av_len = mean(len), sd_len = sd(len), min_len = min(len), max_len = max(len))

# there are documents with 0 length - remove those
all_pi <- all_pi %>% filter(len > 0)

# basic information once more
all_pi %>% group_by(period) %>% 
  summarise(count = n(), autohrs = length(unique(AUTHOR)), av_len = mean(len), sd_len = sd(len), min_len = min(len), max_len = max(len))

# those supposed to be interpelacje poselskie. w Polskim Sejmie jest ich 460, zas w kazdym z lat jest wiecej
posl_json <- jsonlite::fromJSON(txt = "dicts/poslowie.json")
posl_df <- do.call(rbind, args = lapply(1:length(posl_json), FUN = function(i) data.frame(AUTHOR = unlist(posl_json[i]), ugr = names(posl_json)[i])))
row.names(posl_df) <- NULL
# do I iave duplicates
sum(duplicated(posl_df$AUTHOR)) > 0
posl_df[posl_df$AUTHOR == posl_df$AUTHOR[duplicated(posl_df$AUTHOR)], ]

# let see which authors do not rest in posel
authors <- all_pi %>% filter(period == "1997-2001") %>% 
  left_join(posl_df, by = "AUTHOR")
authors %>% group_by(AUTHOR, ugr) %>% summarise(k = n()) %>% filter(is.na(ugr)) %>% head()
# two things: posel sometimes uses second name
# couple of posels cosign one interpellation

# how many interpellation are not accounted for author
authors %>% group_by(not_found = is.na(ugr)) %>% summarise(k = length(unique(AUTHOR)), m = n(), mm = m / nrow(authors))

# 1 in 6

# I've tagged already
pi_tagged_files <- list.files("data", pattern = "^(tagged_pi_)")
all_pi_tagged <- do.call(rbind, lapply(X = pi_tagged_files, FUN = function(i){
  mydb <- dbConnect(RSQLite::SQLite(), file.path("data", i))
  this_cont <- dbGetQuery(conn = mydb, statement = "select * from tagged")
  dbDisconnect(mydb)
  this_cont$period <- gsub("tagged_pi_", replacement = "", x = gsub(pattern = ".sqlite", replacement = "", x = i))
  this_cont
}))

head(all_pi_tagged)

# the data is stripped from whitespaces and removed punctuation, lowercased

# data 2 ----
# create content
all_pi_tagged <- all_pi_tagged %>% 
  group_by(period, ID) %>% 
  summarise(text = paste0(TAGGED, collapse = " ")) %>% 
  mutate(id = paste0(period, ID)) %>% 
  left_join(y = all_pi %>% select(ID, period, AUTHOR), by = c("ID", "period"))
head(all_pi_tagged)

to_analyze <- all_pi_tagged[1:100, ]
## Clusterization ----
dtm <- dtm_from_text(to_analyze)

# develop the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

cdist <- calc_cdist(dtm, tf_mat)

show_top(tf_mat)

show_hierarch(cdist, n_clust = 2)

# inspect these clusters
# use the probability difference method
p_words <- colSums(dtm) / sum(dtm)

clustering <- calc_clust(cdist, n = 2)
cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary

wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")

kfit <- kmeans(cdist, n_clust, nstart=100)
cluster::clusplot(as.matrix(cdist), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# network ----
prepped_sotu <- textnets::PrepText(to_analyze,
                                   groupvar = "AUTHOR",
                                   textvar = "text",
                         node_type = "groups",
                         tokenizer = "words",
                         pos = "nouns",
                         remove_stop_words = FALSE,
                         compound_nouns = FALSE)
sotu_text_network <- CreateTextnet(tidytextobject = prepped_sotu)

# na jakiej podstawie alpha i label_degree_cut? potrzebuje rozkladow dla obiektu zawierajacego graf
# label_degreecut - connected with at least 20% of others
sotu_text_network[[1]]
sotu_text_network[[2]]
VisTextNet(text_network = sotu_text_network, label_degree_cut = .2 * length(unique(sotu_first_speeches$AUTHOR)))
# plot(sotu_text_network)
# hist(link$weight)
# mean(links$weight)
# sd(links$weight)


sotu_communities <- TextCommunities(sotu_text_network)
head(sotu_communities)

sotu_communities %>% group_by(modularity_class) %>% 
  summarise(n_m = n(), o = paste0(group, collapse = "; "))

# kolorem ugrupowanie?
top_words_modularity_classes <- InterpretText(sotu_text_network, prepped_sotu)
head(top_words_modularity_classes)

top_words_modularity_classes %>% group_by(modularity_class) %>% 
  summarise(o = paste0(lemma, collapse = "; "))

text_centrality <- TextCentrality(sotu_text_network)
text_centrality 
# do zapoznania sie nt. centrality: https://cbail.github.io/textasdata/text-networks/rmarkdown/Text_Networks.html

# niektroe interpelacje podpisane sa przez kilku poslow - moze klasteryzacja dla takich grup zeby zobaczyc do kogo blizej jest tym wspolnym interpelacjom ?
# czy poslowie, ktorzy pisza wspolnie interpelacje sa blisko siebie na grafie?
# a klasteryzacja? czy partie sa blisko siebie? czy tematy sa blisko siebie (np. interpelacje do ministra zdrowia)

# topic modelling ----
lda_out <- LDA(
  dtm,
  k = 2,
  method = "Gibbs",
  control = list(seed=42)
)

lda_topics <- lda_out %>%
  tidy(matrix = "beta")

word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

ggplot(
  word_probs,
  aes(term2, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
