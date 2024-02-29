# sentiment
library(DBI)
library(textmineR)

ii <- function(dtm, tf_mat) {
    tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
    tfidf <- t(tfidf)
    csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
    csim <- csim %*% t(csim)
    cdist <- as.dist(1 - csim)
}
setwd("term_project/")
mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")

pp_data <- dbGetQuery(conn = mydb, statement = "select metadatapp.date, ppcontent.* from ppcontent left join metadatapp on metadatapp.id = ppcontent.id")
head(pp_data)

# remove #komentarz
library(tidyverse)
pp_data <- pp_data %>% filter(AUTHOR != "#komentarz")

# group by u-xxx
pp_data <- pp_data %>% mutate(jj = stringr::str_extract(string = ID_INT, pattern = "^u-\\d+"))

pp_data_grouped <- pp_data %>% 
    mutate(ID = paste0(ID, "-", jj)) %>% 
    group_by(DATE, ID, AUTHOR) %>% 
    summarise(CONTENT = paste0(CONTENT, collapse = " "))

pp_data_grouped_pp <- pp_data %>% 
    group_by(DATE, ID) %>% 
    summarise(CONTENT = paste0(CONTENT, collapse = " "))

library(tm)

polish_stop_words <- function() {
    base_stop <- c('a', 'aby', 'ach', 'acz', 'aczkolwiek', 'aj', 'albo', 'ale', 'ależ', 'ani',
      'aż', 
      'art', # short from 'article' as in legal documents
      'bardziej', 'bardzo', 'bo', 'bowiem', 'by', 'byli', 'bynajmniej',
      'być', 'był', 'była', 'było', 'były', 'będzie', 'będą', 
      'cali', 'cała',
      'cały', 'ci', 'cię', 'ciebie', 'co', 'cokolwiek', 'coś', 'czasami',
      'czasem', 'czemu', 'czy', 'czyli', 'daleko', 'dla', 'dlaczego', 'dlatego',
      'do', 'dobrze', 'dokąd', 'dość', 'dużo', 'dwa', 'dwaj', 'dwie', 'dwoje',
      'dziś', 'dzisiaj', 'gdy', 'gdyby', 'gdyż', 'gdzie', 'gdziekolwiek',
      'gdzieś', 'i', 'ich', 'ile', 'im', 'inna', 'inne', 'inny', 'innych', 'iż',
      'ja', 'ją', 'jak', 'jakaś', 'jakby', 'jaki', 'jakichś', 'jakie', 'jakiś',
      'jakiż', 'jakkolwiek', 'jako', 'jakoś', 'je', 'jeden', 'jedna', 'jedno',
      'jednak', 'jednakże', 'jego', 'jej', 'jemu', 'jest', 'jestem', 'jeszcze',
      'jeśli', 'jeżeli', 'już', 'ją', 'każdy', 'kiedy', 'kilka', 'kimś', 'kto',
      'ktokolwiek', 'ktoś', 'która', 'które', 'którego', 'której', 'który',
      'których', 'którym', 'którzy', 'ku', 'lat', 'lecz', 'lub', 'ma', 'mają',
      'mało', 'mam', 'mi', 'mimo', 'między', 'mną', 'mnie', 'mogą', 'moi', 'moim',
      'moja', 'moje', 'może', 'możliwe', 'można', 'mój', 'mu', 'musi', 'my', 'na',
      'nad', 'nam', 'nami', 'nas', 'nasi', 'nasz', 'nasza', 'nasze', 'naszego',
      'naszych', 'natomiast', 'natychmiast', 'nawet', 'nią', 'nic', 'nich', 'nie',
      'niech', 'niego', 'niej', 'niemu', 'nigdy', 'nim', 'nimi', 'niż', 'no', 'o',
      'obok', 'od', 'około', 'on', 'ona', 'one', 'oni', 'ono', 'oraz', 'oto',
      'owszem', 'pan', 'pana', 'pani', 'po', 'pod', 'podczas', 'pomimo', 'ponad',
      'ponieważ', 'powinien', 'powinna', 'powinni', 'powinno', 'poza', 'prawie',
      'przecież', 'przed', 'przede', 'przedtem', 'przez', 'przy', 'roku',
      'również', 'sam', 'sama', 'są', 'się', 'skąd', 'sobie', 'sobą', 'sposób',
      'swoje', 'ta', 'tak', 'taka', 'taki', 'takie', 'także', 'tam', 'te', 'tego',
      'tej', 'temu', 'ten', 'teraz', 'też', 'to', 'tobą', 'tobie', 'toteż',
      'trzeba', 'tu', 'tutaj', 'twoi', 'twoim', 'twoja', 'twoje', 'twym', 'twój',
      'ty', 'tych', 'tylko', 'tym', 'u', 'w', 'wam', 'wami', 'was', 'wasz', 'zaś',
      'wasza', 'wasze', 'we', 'według', 'wiele', 'wielu', 'więc', 'więcej', 'tę',
      'wszyscy', 'wszystkich', 'wszystkie', 'wszystkim', 'wszystko', 'wtedy',
      'wy', 'właśnie', 'z', 'za', 'zapewne', 'zawsze', 'ze', 'zł', 'znowu',
      'znów', 'został', 'żaden', 'żadna', 'żadne', 'żadnych', 'że', 'żeby',
      '$', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '_')
    c(base_stop, c("a", "aby", "ad", "vocem", "że", "iż", "by", "o", "i", "się", "jest", "czy"))
}

# The tm_map() function is also used to further clean the text
# a) to remove unnecessary spaces, punctuation and numbers
docs <- Corpus(VectorSource(pp_data_grouped_pp$CONTENT))
# remove unnecessary spaces
docs <- tm_map(docs, stripWhitespace)
# remove unnecessary punctuation
docs <- tm_map(docs, removePunctuation)
# remove unnecessary numbers
docs <- tm_map(docs, removeNumbers)
# b) change letters to lower case
# change to lowercase
docs <- tm_map(docs, content_transformer(tolower))
# remove Polish stopwords
docs <- tm_map(docs, removeWords, polish_stop_words())

top_me <- function(docs) {
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
}

top_me(docs)

dtm <- CreateDtm(doc_vec = pp_data_grouped_pp$CONTENT, # character vector of documents
                 doc_names = pp_data_grouped_pp$ID, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = polish_stop_words(),
                 # stopword_vec = c(stopwords::stopwords("pl"), # stopwords from tm
                 #                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

# develop the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

hc <- hclust(ii(dtm, tf_mat), "ward.D")
n_clus <- 2
clustering <- cutree(hc, n_clus)

plot(hc, main = "Hierarchical clustering of %s NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, n_clus, border = "red")

reviewsDTM <- DocumentTermMatrix(docs)
# library(tidytext)
# reviewsDTM_tidy <- tidy(reviewsDTM)
# cleaned_documents <- reviewsDTM_tidy %>%
#     group_by(document) %>% 
#     mutate(terms = toString(rep(term, count))) %>%
#     select(document, terms) %>%
#     unique()
# head(cleaned_documents)

pp_data_grouped$CONTENT[1]

# zeby wkoynac anlize sentymentu potrzebuje:
# a) lematyzacji
# b) slownika lemat-sentyment
# dla kazdej wypowiedzi moge okreslic jaki jest jej sentyment oraz jaki % slow ma okreslony sentyment

# zeby wkoynac network analysis
# do kazdego tekstu (czyt. interpelacja poselska, wypowiedz na komisji) potrzebuje danych takich jak:
# autor wypowiedzi
# przynaleznosc partyjna autora

# slowosiec: http://plwordnet.pwr.wroc.pl/wordnet/download
