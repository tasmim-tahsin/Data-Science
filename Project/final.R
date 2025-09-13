# Install all needed libraries (run this only once)
install.packages("dplyr")
install.packages("tm")
install.packages("SnowballC")
install.packages("textclean")
install.packages("cluster")
install.packages("factoextra")
install.packages("dbscan")
install.packages("tokenizers")
install.packages(c("hunspell", "textstem"))
install.packages("tidyverse")
install.packages("tidytext")

library(tidyverse)
library(tidytext)
library(tm)         # corpus and text processing
library(SnowballC)  # stemming
library(textclean)  # contractions, emojis
library(hunspell)   # spell checking
library(textstem)   # lemmatization
library(dplyr)
library(cluster)
library(factoextra)
library(dbscan)
library(syuzhet)
library(NLP)
library(tokenizers)
library(RColorBrewer)
library(stringi)
library(reshape2)

data <- read.csv("E:\\Data Science\\Dataset\\booking_reviews.csv", stringsAsFactors = FALSE)
data[data == "NULL"] <- NA
data[data == ""] <- NA
clean_data <- data[!is.na(data$review_text) & !is.na(data$review_title) & 
                     !is.na(data$rating) & !is.na(data$hotel_name), ]
set.seed(123)
data_sample <- clean_data[sample(nrow(clean_data), 2000), ]

reviews_raw <- as.character(data_sample$review_text)

# -----------------------------
# 2) Cleaning helpers
# -----------------------------
replace_emoticon_emoji_contraction <- function(x){
  if(requireNamespace("textclean", quietly = TRUE)){
    x <- textclean::replace_contraction(x)
    if("replace_emoticon" %in% ls("package:textclean")){
      x <- textclean::replace_emoticon(x)
    }
    if("replace_emoji" %in% ls("package:textclean")){
      x <- textclean::replace_emoji(x)
    } else {
      x <- stringi::stri_replace_all_regex(x, 
                                           "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U00002600-\U000027BF]", " ")
    }
  } else {
    contr <- c("won't"="will not","can't"="can not","n't"=" not","'re"=" are","'s"=" is","'d"=" would",
               "'ll"=" will","'ve"=" have","'m"=" am")
    x <- tolower(x)
    for(pat in names(contr)) x <- gsub(pat, contr[pat], x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("(:\\s?\\)|:-\\)|:\\)|:D|=\\))", " smile ", x)
    x <- gsub("(:\\s?\\(|:-\\(|:\\()", " sad ", x)
    x <- stringi::stri_replace_all_regex(x, 
                                         "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF]", " ")
  }
  return(x)
}

# -----------------------------
# 3) Build corpus + cleaning
# -----------------------------
corpus <- VCorpus(VectorSource(reviews_raw))
corpus <- tm_map(corpus, content_transformer(replace_emoticon_emoji_contraction))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[\n]+", " ", x)))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

clean_text <- sapply(corpus, function(x) x$content)

# -----------------------------
# 4) Tokenization + stemming + lemmatization
# -----------------------------
tokens_list <- tokenize_words(clean_text, lowercase = FALSE)

stemmed_text <- sapply(tokens_list, function(tok_vec){
  if(length(tok_vec)==0) return("")
  paste(wordStem(tok_vec, language = "en"), collapse = " ")
}, USE.NAMES = FALSE)

lemmatized_text <- textstem::lemmatize_strings(clean_text)

reviews_df <- data.frame(
  doc_id = seq_along(clean_text),
  raw = reviews_raw,
  cleaned = clean_text,
  stemmed = stemmed_text,
  lemmatized = lemmatized_text,
  stringsAsFactors = FALSE
)

# -----------------------------
# 5) TF-IDF matrix
# -----------------------------
corpus_lemma <- VCorpus(VectorSource(reviews_df$lemmatized))
dtm <- DocumentTermMatrix(corpus_lemma,
                          control = list(wordLengths = c(2, Inf)))
dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf_trim <- removeSparseTerms(dtm_tfidf, 0.99)

tfidf_mat <- as.matrix(dtm_tfidf_trim)
tfidf_mat <- tfidf_mat[, apply(tfidf_mat, 2, sd, na.rm = TRUE) > 0, drop = FALSE]
tfidf_scaled <- scale(tfidf_mat)

# -----------------------------
# 6) PCA
# -----------------------------
pca_res <- prcomp(tfidf_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(doc_id = reviews_df$doc_id,
                     PC1 = pca_res$x[,1],
                     PC2 = pca_res$x[,2])

# -----------------------------
# 7) Sentiment (syuzhet)
# -----------------------------
syuzhet_scores <- get_sentiment(reviews_df$lemmatized, method = "syuzhet")

reviews_df$sentiment_score <- syuzhet_scores
reviews_df$sentiment_label <- ifelse(reviews_df$sentiment_score > 0, "positive",
                                     ifelse(reviews_df$sentiment_score < 0, "negative", "neutral"))

table(reviews_df$sentiment_label)

# -----------------------------
# 8) Clustering
# -----------------------------
# K-means
set.seed(42)
k <- 3
kmeans_res <- kmeans(tfidf_scaled, centers = k, nstart = 25)
pca_df$kmeans_cluster <- factor(kmeans_res$cluster)

# Hierarchical
dist_mat <- dist(tfidf_scaled, method = "euclidean")
hclust_res <- hclust(dist_mat, method = "ward.D2")
hclust_clusters <- cutree(hclust_res, k = k)
pca_df$hclust_cluster <- factor(hclust_clusters)

# DBSCAN
pc_for_db <- pca_res$x[,1:5]
k_nn <- 4
kNNd <- kNNdist(as.matrix(pc_for_db), k = k_nn)
eps_guess <- as.numeric(quantile(kNNd, 0.90))
dbscan_res <- dbscan(as.matrix(pc_for_db), eps = eps_guess, minPts = 5)
pca_df$dbscan_cluster <- factor(ifelse(dbscan_res$cluster == 0, NA, dbscan_res$cluster))

# -----------------------------
# 9) Visualizations
# -----------------------------
p_kmeans <- ggplot(pca_df, aes(PC1, PC2, color = kmeans_cluster)) +
  geom_point(size=2, alpha=0.8) + ggtitle("K-means clusters (k=3)") + theme_minimal()

p_hclust <- ggplot(pca_df, aes(PC1, PC2, color = hclust_cluster)) +
  geom_point(size=2, alpha=0.8) + ggtitle("Hierarchical clusters (k=3)") + theme_minimal()

p_dbscan <- ggplot(pca_df, aes(PC1, PC2, color = dbscan_cluster)) +
  geom_point(size=2, alpha=0.8, na.rm=TRUE) + 
  ggtitle(paste0("DBSCAN clusters (eps=", round(eps_guess,3), ")")) + theme_minimal()

print(p_kmeans)
print(p_hclust)
print(p_dbscan)

# -----------------------------
# 10) Evaluation
# -----------------------------
combined <- left_join(pca_df, reviews_df %>% 
                        select(doc_id, sentiment_label, sentiment_score), by = "doc_id")

cat("\nK-means vs Sentiment:\n")
print(table(combined$kmeans_cluster, combined$sentiment_label))

cat("\nHierarchical vs Sentiment:\n")
print(table(combined$hclust_cluster, combined$sentiment_label))

cat("\nDBSCAN vs Sentiment:\n")
print(table(combined$dbscan_cluster, combined$sentiment_label, useNA = "ifany"))

sil_km <- silhouette(kmeans_res$cluster, dist_mat)
cat("\nK-means average silhouette:", mean(sil_km[, "sil_width"]), "\n")

sil_hc <- silhouette(hclust_clusters, dist_mat)
cat("Hierarchical average silhouette:", mean(sil_hc[, "sil_width"]), "\n")

# -----------------------------
# 11) Save cluster results
# -----------------------------
reviews_df$kmeans_cluster <- kmeans_res$cluster
reviews_df$hclust_cluster <- hclust_clusters
reviews_df$dbscan_cluster <- dbscan_res$cluster

head(reviews_df %>% 
       select(doc_id, sentiment_label, sentiment_score, 
              kmeans_cluster, hclust_cluster, dbscan_cluster, raw), 8)

















#viz

# Count of sentiments
sentiment_count <- table(reviews_df$sentiment_label)
sentiment_count

# Plot
ggplot(reviews_df, aes(x = sentiment_label, fill = sentiment_label)) +
  geom_bar() +
  labs(title = "Distribution of Sentiment in Reviews", x = "Sentiment", y = "Count") +
  scale_fill_manual(values = c("positive"="green", "neutral"="blue", "negative"="red")) +
  theme_minimal()


#review length
reviews_df$review_length <- sapply(strsplit(reviews_df$raw, "\\s+"), length)

ggplot(reviews_df, aes(x = review_length)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Review Length", x = "Number of Words", y = "Count") +
  theme_minimal()



# Create combined dataframe with sentiment labels
combined <- left_join(
  pca_df, 
  reviews_df %>% select(doc_id, sentiment_label), 
  by = "doc_id"
)

# Now plot K-means clusters vs sentiment
ggplot(combined, aes(x = kmeans_cluster, fill = sentiment_label)) +
  geom_bar(position = "fill") +
  labs(title = "K-means Clusters vs Sentiment", x = "Cluster", y = "Proportion") +
  scale_fill_manual(values = c("positive"="green", "neutral"="blue", "negative"="red")) +
  theme_minimal()








# Cleaned text for word clouds
cleaned_reviews <- reviews_df %>% select(lemmatized, sentiment_label)

# Function to generate word cloud for a sentiment
generate_wordcloud <- function(sentiment){
  words <- cleaned_reviews %>%
    filter(sentiment_label == sentiment) %>%
    pull(lemmatized)
  
  words_corpus <- Corpus(VectorSource(words))
  words_corpus <- tm_map(words_corpus, removeWords, stopwords("en"))
  
  dtm <- TermDocumentMatrix(words_corpus)
  matrix <- as.matrix(dtm)
  word_freq <- sort(rowSums(matrix), decreasing = TRUE)
  wordcloud(names(word_freq), freq = word_freq, min.freq = 5,
            max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
            scale = c(3,0.5), main = paste("Word Cloud:", sentiment))
}

# Generate word clouds
generate_wordcloud("positive")
generate_wordcloud("neutral")
generate_wordcloud("negative")





library(ggplot2)

# Ensure rating is numeric
reviews_df$rating <- as.numeric(data_sample$rating)

# Plot histogram
ggplot(reviews_df, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count of Reviews") +
  scale_x_continuous(breaks = seq(min(reviews_df$rating, na.rm = TRUE),
                                  max(reviews_df$rating, na.rm = TRUE), 0.5)) +
  theme_minimal()


# Ensure rating is numeric
reviews_df$rating <- as.numeric(data_sample$rating)

# Line histogram with density overlay
ggplot(reviews_df, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, 
                 color = "blue", fill = NA, size = 1) +  # Line histogram
  geom_density(color = "orange", size = 1) +     # Smooth density curve
  labs(title = "Rating Distribution with Density Curve",
       x = "Rating", y = "Density") +
  scale_x_continuous(breaks = seq(min(reviews_df$rating, na.rm = TRUE),
                                  max(reviews_df$rating, na.rm = TRUE), 0.5)) +
  theme_minimal()









# Select numeric columns only
numeric_data <- clean_data %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt correlation matrix into long format
cor_melt <- melt(cor_matrix)

# Plot heatmap
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
str(clean_data)
