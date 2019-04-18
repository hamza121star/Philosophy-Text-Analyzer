# Text Mining Project For Medium
rm(list=ls())

require(quanteda)
require(readtext)
require(wordcloud)
require(gutenbergr)
require(stringi)
library(ggplot2)
library(ggpubr)
library(topicmodels)
library(tidyverse)      
library(stringr)        
library(tidytext) 
library(dplyr)
library(igraph)
#First part of the project: Analysis text of one of the books from Gutenberg project
#Book Selected: Beyond Good and Evil By Friedrich Nietzsche Translated by Helen Zimmern
#Beyond Good and Evil
data_fredrich <- texts(readtext("http://www.gutenberg.org/cache/epub/4363/pg4363.txt"))
names(data_fredrich) <- "Beyond Good and Evil"

#Geneology of Morals
data_gene <- texts(readtext("http://www.gutenberg.org/files/52319/52319-0.txt"))
names(data_gene) <- "The Genealogy of Morals"

#Next importing book data relevant to other philosophical books by a different author
#Essays by Ralph Waldo Emerson
data_ralph <- texts(readtext("http://www.gutenberg.org/cache/epub/2944/pg2944.txt"))
names(data_ralph) <- "Essays By Ralph Waldo Emerson"

#Rene Descartes philosophical book
data_descartes <- texts(readtext("http://www.gutenberg.org/cache/epub/4391/pg4391.txt"))
names(data_descartes) <- "Principles of Philosophy by Renes Descartes"

#The Communist Manifesto By Karl Marx
data_communist <- texts(readtext("http://www.gutenberg.org/cache/epub/61/pg61.txt"))
names(data_communist) <- "The Communist Manifesto by Karl Marx"

data_kant <- texts(readtext("http://www.gutenberg.org/files/4280/4280-0.txt"))
names(data_kant) <- "The Critique of Pure Reason"


# Clean the book data, removing unnecessary pre-text and post-text
start_ralph <- stri_locate_first_fixed(data_ralph, "By Ralph Waldo Emerson")[1]
end_ralph <- stri_locate_last_fixed(data_ralph, "supplements and continuations of the material creation.")[1]
data_ralph_cut <- char_tolower(stri_sub(data_ralph, start_ralph, end_ralph))

start_des <- stri_locate_first_fixed(data_descartes, "SELECTIONS FROM THE PRINCIPLES OF PHILOSOPHY")[1]
end_des <- stri_locate_last_fixed(data_descartes, "evidence of reason.")[1]
data_des_cut <- char_tolower(stri_sub(data_descartes, start_des, end_des))

start_com <- stri_locate_first_fixed(data_communist, "MANIFESTO OF THE COMMUNIST PARTY")[1]
end_com <- stri_locate_last_fixed(data_communist, "WORKING MEN OF ALL COUNTRIES, UNITE!")[1]
data_com_cut <- char_tolower(stri_sub(data_communist, start_com, end_com))

start_fredrich <- stri_locate_first_fixed(data_fredrich, "The following is a reprint of the Helen")[1]
end_fredrich <- stri_locate_last_fixed(data_fredrich, "Light and Dark were one that wedding-morn.")[1]
data_fredrich_cut <- char_tolower(stri_sub(data_fredrich, start_fredrich, end_fredrich))

start_gene <- stri_locate_first_fixed(data_gene, "In 1887, with the view")[1]
end_gene <- stri_locate_last_fixed(data_gene, "she, too, does not betray us!")[1]
data_gene_cut <- char_tolower(stri_sub(data_gene, start_gene, end_gene))
Encoding(data_gene_cut) <- "latin1"
data_gene_cut <- iconv(data_gene_cut, "latin1", "ASCII",sub='')

start_kant <- stri_locate_first_fixed(data_kant, "Human reason, in one sphere of its cognition")[1]
end_kant <- stri_locate_last_fixed(data_kant, "End of the Project Gutenberg")[1]
data_kant_cut <- char_tolower(stri_sub(data_kant, start_kant, end_kant))


rm(start_ralph, end_ralph, start_des, end_des,start_com, end_com, start_fredrich, end_fredrich, start_gene, end_gene)


#######    Extracting Chapters from Nietzsche's books: Beyond Good and Evil    #######
#######                                                          #######
# extract Lecture on The Free Spirit
start_a <- stri_locate_first_fixed(data_fredrich, "O sancta simplicitiatas! In")[1]
end_a <- stri_locate_last_fixed(data_fredrich, "coming ones? ye NEW philosophers?")[1]
freeSpirit_corpus <- char_tolower(stri_sub(data_fredrich, start_a, end_a))

#Extract Lecture on The Religious Mood
start_b <- stri_locate_first_fixed(data_fredrich, "The human soul and its limits")[1]
end_b <- stri_locate_last_fixed(data_fredrich, "mediocre, the European of the present day.")[1]
religious_corpus <- char_tolower(stri_sub(data_fredrich, start_b, end_b))

#Extract Lecture on The Natural History of Morals
start_c <- stri_locate_first_fixed(data_fredrich, "The moral sentiment in Europe at present")[1]
end_c <- stri_locate_last_fixed(data_fredrich, "--and perhaps also a new MISSION!")[1]
moralshistory_corpus <- char_tolower(stri_sub(data_fredrich, start_c, end_c))

#Extract Lecture on Virtue
start_d <- stri_locate_first_fixed(data_fredrich, "OUR Virtues?--It is probable that we,")[1]
end_d <- stri_locate_last_fixed(data_fredrich, "CHAPTER VIII. PEOPLES AND COUNTRIES")[1]
virtue_corpus <- char_tolower(stri_sub(data_fredrich, start_d, end_d))

#combine corpus
beyond_good_evil <- corpus(c(freeSpirit_corpus, religious_corpus, moralshistory_corpus, virtue_corpus))
rownames(beyond_good_evil$documents) <- c("Free Spirit", "Religious Mood","History of Morals", "Virtue")
View(beyond_good_evil$documents)


#######                 Genealogy of Morals By Nietzsche                                         #######
# extract Lecture on The Good and Evil
start_w <- stri_locate_first_fixed(data_gene, "Those English psychologists, who up to")[1]
end_w <- stri_locate_last_fixed(data_gene, "that he has to fix the _hierarchy of values_.")[1]
goodevil_corpus <- char_tolower(stri_sub(data_gene, start_w, end_w))

#Extract Lecture on Guilt
start_x <- stri_locate_first_fixed(data_gene, "The breeding of an animal that _can")[1]
end_x <- stri_locate_last_fixed(data_gene, "_Zarathustra, Zarathustra the godless._")[1]
guilt_corpus <- char_tolower(stri_sub(data_gene, start_x, end_x))

#Extract Lecture on The Ascetic Ideals
start_y <- stri_locate_first_fixed(data_gene, "Careless, mocking, forceful")[1]
end_y <- stri_locate_last_fixed(data_gene, "rather than not wish _at all_.")[1]
ascetic_corpus <- char_tolower(stri_sub(data_gene, start_y, end_y))


#combine corpus
morals_combined <- corpus(c(goodevil_corpus, guilt_corpus, ascetic_corpus))
rownames(morals_combined$documents) <- c("Good and Evil", "Guilt","Ascetic Ideals")
View(morals_combined$documents)
####################################################################################

#######    Extracting Chapters from Ralph Waldo's Essys          #######
#######                                                          #######
#Extract Lecture on Spiritual Law
start_e <- stri_locate_first_fixed(data_ralph, "When the act of reflection takes place in the mind")[1]
end_e <- stri_locate_last_fixed(data_ralph, "the true fire through every one of its million disguises.")[1]
spiritual_corpus <- char_tolower(stri_sub(data_ralph, start_e, end_e))

#Extract Lecture on Self Reliance
start_aa <- stri_locate_first_fixed(data_ralph, "I READ the other day some verses written by an eminent painter which")[1]
end_aa <- stri_locate_last_fixed(data_ralph, "but the triumph of principles.")[1]
selfReliance_corpus <- char_tolower(stri_sub(data_ralph, start_aa, end_aa))

#Extract Lecture on Compensation
start_bb <- stri_locate_first_fixed(data_ralph, "Ever since I was a boy I have wished to write a discourse")[1]
end_bb <- stri_locate_last_fixed(data_ralph, "yielding shade and fruit to wide neighborhoods of men.")[1]
compensation_corpus <- char_tolower(stri_sub(data_ralph, start_bb, end_bb))

#Extract Lecture on Love
start_cc <- stri_locate_first_fixed(data_ralph, "Every promise of the soul has innumerable fulfilments; each of its joys")[1]
end_cc <- stri_locate_last_fixed(data_ralph, "beautiful, and so on
for ever.")[1]
love_corpus <- char_tolower(stri_sub(data_ralph, start_cc, end_cc))

combined_ralph <- corpus(c(spiritual_corpus, compensation_corpus, selfReliance_corpus, love_corpus))
rownames(combined_ralph$documents) <- c("Spirtual Law", "Compensation","Self Reliance", "Love")
View(combined_ralph$documents)
####################################################################################


#######    Extracting Chapters from The Communist Manifesto      #######
#######                                                          #######
#Extract Lecture on Bourgeios
start_f <- stri_locate_first_fixed(data_communist, "The history of all hitherto existing societies is the history")[1]
end_f <- stri_locate_last_fixed(data_communist, "the proletariat are equally inevitable.")[1]
bourgeios_corpus <- char_tolower(stri_sub(data_communist, start_f, end_f))

start_g <- stri_locate_first_fixed(data_communist, "In what relation do the Communists stand to the proletarians as a")[1]
end_g <- stri_locate_last_fixed(data_communist, "development of all.")[1]
proletarian_corpus <- char_tolower(stri_sub(data_communist, start_g, end_g))

start_h <- stri_locate_first_fixed(data_communist, "Owing to their historical position, it became the vocation of the")[1]
end_h <- stri_locate_last_fixed(data_communist, "oppose the Chartists and the Reformistes.")[1]
socialism_corpus <- char_tolower(stri_sub(data_communist, start_h, end_h))

start_i <- stri_locate_first_fixed(data_communist, "Section II has made clear the relations of")[1]
end_i <- stri_locate_last_fixed(data_communist, "WORKING MEN OF ALL COUNTRIES, UNITE!")[1]
party_corpus <- char_tolower(stri_sub(data_communist, start_i, end_i))

#combine karl_marx corpus
combined_karl <- corpus(c(bourgeios_corpus, proletarian_corpus, socialism_corpus, party_corpus))
rownames(combined_karl$documents) <- c("Bourgeios & Proletarians", "Proletarians & Communists","Socialist/Communist Literature", "Communists on Opposing Parties")
View(combined_karl$documents)

data_all <- corpus(c(data_des_cut, data_kant_cut, data_com_cut, data_ralph_cut, data_gene_cut, data_fredrich_cut))
rownames(data_all$documents) <- c("Principles of Philosophy", "The Critique of Pure Reason", "The Communist Manifesto", "Essays-The First Series",
                                  "Genealogy of Morals", "Beyond Good and Evil")

allfourbooks <- corpus(c(freeSpirit_corpus, religious_corpus, moralshistory_corpus, virtue_corpus, 
                          goodevil_corpus, guilt_corpus, ascetic_corpus,
                          spiritual_corpus, compensation_corpus, selfReliance_corpus, love_corpus,
                          bourgeios_corpus, proletarian_corpus, socialism_corpus, party_corpus))
rownames(allfourbooks$documents) <- c("Free Spirit", "Religious Mood","History of Morals", "Virtue",
                                       "Good and Evil", "Guilt","Ascetic Ideals",
                                       "Sprituality","Compensation","Self Reliance","Love",
                                       "Bourgeios & Proletarians", "Proletarians & Communists","Socialist/Communist Literature", "Communists on Opposing Parties")
View(allfourbooks$documents)

rm(data_ralph, data_gene, data_communist,data_fredrich, data_descartes)
rm(start_a, end_a, start_b, end_c, start_c, end_c, start_d, end_d, start_e, end_e, start_f, end_f, start_g, end_g, start_h,
   end_h, start_i, end_h, start_aa, end_aa,  start_bb, end_bb, start_cc, end_cc)

####################################################################################
####### The Functions Section #######
#this function prints top features
printTopFeatures <- function(corpus_dfm){
  topfeatures(corpus_dfm, n=10, scheme = c("count", "docfreq"), decreasing = TRUE)
}

extractCorpusDFM <- function(corpusname){
  corpusname <- iconv(corpusname, "latin1", "ASCII", sub="") 
  corp.tokens <- tokens(corpusname, what = "word", 
                        remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
  corp.tokens.st <- tokens_select(corp.tokens, stopwords(source='smart'), selection = 'remove')
  corp_dfm <- tokens(corpusname, remove_punct = TRUE) %>% 
    dfm()
  corp_dfm_st<-dfm(corp.tokens.st)
  
  print(nfeat(corp_dfm)) #number of words
  print(head(corp_dfm))
  
  return (corp_dfm_st)
}

#this function will create wordcloud of the corpus without the stop words
createWordCloud <- function(corpusname) {
  corpusname <- iconv(corpusname, "latin1", "ASCII", sub="") 
  corp.tokens <- tokens(corpusname, what = "word", 
                        remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
  corp.tokens.st <- tokens_select(corp.tokens, stopwords(source='smart'), selection = 'remove')
  corp_dfm <- tokens(corpusname, remove_punct = TRUE) %>% 
    dfm()
  corp_dfm_st<-dfm(corp.tokens.st)
  #get some information about DFM
  print(nfeat(corp_dfm)) #number of words
  print(head(corp_dfm))
  print(printTopFeatures(corp_dfm_st))
  
  #work cloud for spam using the word cloud function in quanteda
  corp.tokens.dfm <- dfm(corp.tokens.st, tolower = FALSE)
  #textplot_wordcloud(corp.tokens.dfm, max_words = 75)
  textplot_wordcloud(corp.tokens.dfm, min.freq = 50, max_words= 75,random.order=FALSE, rot.per=0.35,
                     colors = brewer.pal(8, "Dark2"))
}

getcorpustokens <- function(corpusname){
  corpusname <- iconv(corpusname, "latin1", "ASCII", sub="") 
  corp.tokens <- tokens(corpusname, what = "word", 
                        remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
  corp.tokens.st <- tokens_select(corp.tokens, stopwords(source='smart'), selection = 'remove')
  return (corp.tokens.st)
}

# Functions copied from D Robinson's libr

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}



scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# Create Word Clouds of Beyond Good and Evil lectures
createWordCloud(freeSpirit_corpus) 
createWordCloud(religious_corpus)
createWordCloud(moralshistory_corpus)
createWordCloud(virtue_corpus)

createWordCloud(goodevil_corpus)
createWordCloud(guilt_corpus)
createWordCloud(ascetic_corpus)

## This part of the code is dedicated to another philosophy book 

#Word Clouds for Waldo's Essays
createWordCloud(spiritual_corpus)
createWordCloud(compensation_corpus)
createWordCloud(selfReliance_corpus)
createWordCloud(love_corpus)

#Create Word Cloud of each Manifesto Topics:
createWordCloud(bourgeios_corpus)
createWordCloud(proletarian_corpus)
createWordCloud(socialism_corpus)
createWordCloud(party_corpus)



createWordCloud(data_ralph_cut)
createWordCloud(data_gene_cut)
createWordCloud(data_fredrich_cut)
createWordCloud(data_des_cut)
createWordCloud(data_kant_cut)
createWordCloud(combined_karl)



createWordCloud(allfourbooks)

#ngrams
chapter_similarity_total <- textstat_simil(dfm(data_all), rownames(data_all$documents), margin = "documents", method = "ejaccard")
chapter_similarity_total


###### Chapter Similarties
chapter_similarity <- textstat_simil(dfm(beyond_good_evil), c("Free Spirit", "Religious Mood","History of Morals", "Virtue"), margin = "documents", method = "ejaccard")
chapter_similarity #shows that the lecturer has used similar wording around various topics

chapter_simi_gene <- textstat_simil(dfm(morals_combined), c("Good and Evil", "Guilt","Ascetic Ideals"), margin = "documents", method = "ejaccard")
chapter_simi_gene

chapter_similarity_a <- textstat_simil(dfm(combined_ralph$documents), c("Spritual Law", "Compensation", "Self Reliance","Love"), margin = "documents", method = "ejaccard")
chapter_similarity_a #shows that the lecturer has used similar wording around various topics

chapter_similarity_b <- textstat_simil(dfm(combined_karl), rownames(combined_karl$documents), margin = "documents", method = "ejaccard")
chapter_similarity_b #shows that the lecturer has used similar wording around various topics


combined_similarity <- textstat_simil(dfm(allfourbooks), rownames(allfourbooks$documents), margin = "documents", method = "ejaccard")
combined_similarity

combine_four <- corpus(c())
# Create Graphs of TF-IDF against the words discovered

# Our function for calculating relative term frequency (TF)
words <- names(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined))))
idf_vals <- unname(as.numeric(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined)))))
#Plot
plot_a <- ggplot(data=c$tokens, aes(reorder(words, +idf_vals), idf_vals)) +
  geom_col(show.legend = FALSE, fill="red") + labs(x="Words Based on TF-IDF",y="TF-IDF Score")  + ggtitle("TD-IDF Value Chart of Four Phases of Morals") +
  coord_flip()+theme(plot.title = element_text(color="#666666", face="bold", size=10, hjust=0),
                     axis.title.x = element_text(color="#B20000", size=10, face="bold"),
                     axis.title.y = element_text(color="#B20000", size=10, face="bold"))

words_a <- names(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined_ralph))))
idf_vals_a <- unname(as.numeric(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined_ralph)))))
#Plot
plot_b <- ggplot(data=combined_ralph$tokens, aes(reorder(words_a, +idf_vals_a), idf_vals_a)) +
  geom_col(show.legend = FALSE, fill="green") + labs(x="Words Based on TF-IDF",y="TF-IDF Score")  + ggtitle("TD-IDF value Chart of Ralph Waldo's Essays") +
  coord_flip()+theme(plot.title = element_text(color="#666666", face="bold", size=10, hjust=0),
                     axis.title.x = element_text(color="#4ca64c", size=10, face="bold"),
                     axis.title.y = element_text(color="#4ca64c", size=10, face="bold"))

words_b <- names(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined_karl))))
idf_vals_b <- unname(as.numeric(printTopFeatures(dfm_tfidf(extractCorpusDFM(combined_karl)))))
#Plot
plot_c <- ggplot(data=combined_karl$tokens, aes(reorder(words_b, +idf_vals_b), idf_vals_b)) +
  geom_col(show.legend = FALSE, fill="blue") + labs(x="Words Based on TF-IDF",y="TF-IDF Score")  + ggtitle("TD-IDF value Chart of Karl Marx's Manifesto") +
  coord_flip()+theme(plot.title = element_text(color="#666666", face="bold", size=10, hjust=0),
                     axis.title.x = element_text(color="#3232ff", size=10, face="bold"),
                     axis.title.y = element_text(color="#3232ff", size=10, face="bold"))

ggarrange(plotd, plot_a, plot_b, plot_c, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


#### Reference: Code taken from url: http://uc-r.github.io/sentiment_analysis
#### This code allows us to create sentiment analysis graphs which can be based on different sentiments

titles <- c("Principles Of Philosophy By Descartes","The Critique of Pure Reason","Essays By Ralph Waldo", "Karl Marx's Manifesto", "Geneology of Morals By Nietzsche","Beyond Good and Evil By Nietzsche")
textbooks <- list(data_des_cut,data_kant_cut,data_ralph_cut, data_com_cut,  data_gene_cut, data_fredrich_cut)

titles <- c("The Critique of Pure Reason")
textbooks <- list(data_kant_cut)


series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(textbooks[[i]]),
                  text = textbooks[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(textbook = titles[i]) %>%
    select(textbook, everything())
  
  series <- rbind(series, clean)
}

series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)


series %>%
  group_by(textbook) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 300 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(textbook, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         textbook = factor(textbook, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = "cyan")) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE, color="#7a422d") +
  facet_wrap(~ textbook, ncol = 2, scales = "free_x")



#ngrams analysis
for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(textbooks[[i]]),
                  text = textbooks[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))


series %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(book, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(reorder_within(bigram, n, book), n, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip() + labs(x = "Bigrams", y="n")


# TF IDF CHART

(bigram_tf_idf <- series %>%
    count(book, bigram, sort = TRUE) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))
)

bigram_tf_idf %>%
  group_by(book) %>%
  top_n(15, wt = tf_idf) %>%
  ungroup() %>%
  mutate(book = factor(book) %>% forcats::fct_rev()) %>%
  ggplot(aes(reorder_within(bigram, tf_idf, book), tf_idf, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest TF-IDF bi-grams in selected Philosophical texts",
       x = NULL, y = "tf-idf") +
  scale_x_reordered() +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


#network graphs
(bigram_graph <- series %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    unite("bigram", c(word1, word2), sep = " ") %>%
    filter(n > 15) %>%
    graph_from_data_frame()
)

library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.55, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "red", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



#number of words
sapply(gregexpr("[[:alpha:]]+", data_gene_cut), function(x) sum(x > 0))
sapply(gregexpr("[[:alpha:]]+", data_fredrich_cut), function(x) sum(x > 0))
sapply(gregexpr("[[:alpha:]]+", data_ralph_cut), function(x) sum(x > 0))
sapply(gregexpr("[[:alpha:]]+", data_des_cut), function(x) sum(x > 0))
sapply(gregexpr("[[:alpha:]]+", data_com_cut), function(x) sum(x > 0))
sapply(gregexpr("[[:alpha:]]+", data_kant_cut), function(x) sum(x > 0))

extractCorpusDFM(data_gene_cut)
extractCorpusDFM(data_fredrich_cut)
extractCorpusDFM(data_ralph_cut)
extractCorpusDFM(data_des_cut)
extractCorpusDFM(data_com_cut)
extractCorpusDFM(data_kant_cut)
