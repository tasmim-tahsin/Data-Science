# Dataset [Final]
https://www.kaggle.com/datasets/thedevastator/booking-com-hotel-reviews

# Hotel Reviews Sentiment Analysis and Clustering Project

## 📋 Project Overview
This project performs comprehensive sentiment analysis and clustering on hotel review data using natural language processing (NLP) techniques. The code processes textual reviews, extracts features using TF-IDF, performs sentiment analysis, and applies three clustering algorithms with visualization.

## 🎯 Objectives
- Clean and preprocess hotel review text data
- Perform sentiment analysis using multiple methods
- Apply dimensionality reduction using PCA
- Implement three clustering algorithms (K-means, Hierarchical, DBSCAN)
- Visualize results and evaluate clustering performance
- Generate insights from hotel review data

## 📊 Dataset
- **Source**: `booking_reviews.csv` `https://www.kaggle.com/datasets/thedevastator/booking-com-hotel-reviews`
- **Features**: review_text, review_title, rating, hotel_name
- **Sample Size**: 2,000 reviews (randomly sampled from cleaned data)

## 🛠️ Installation & Setup

### Required Libraries
```r
# Install packages (run once)
install.packages(c(
  "dplyr", "tm", "SnowballC", "textclean", "cluster", 
  "factoextra", "dbscan", "tokenizers", "hunspell", 
  "textstem", "tidyverse", "tidytext", "syuzhet", 
  "NLP", "RColorBrewer", "stringi", "reshape2"
))

# Load libraries
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(textclean)
library(hunspell)
library(textstem)
library(cluster)
library(factoextra)
library(dbscan)
library(syuzhet)
library(NLP)
library(tokenizers)
library(RColorBrewer)
library(stringi)
library(reshape2)
```

## 🔧 Code Structure

### 1. Data Loading and Cleaning
```r
# Load and clean data
data <- read.csv("E:\\Data Science\\Dataset\\booking_reviews.csv", stringsAsFactors = FALSE)
data[data == "NULL"] <- NA
data[data == ""] <- NA
clean_data <- data[!is.na(data$review_text) & !is.na(data$review_title) & 
                     !is.na(data$rating) & !is.na(data$hotel_name), ]
set.seed(123)
data_sample <- clean_data[sample(nrow(clean_data), 2000), ]
```

### 2. Text Preprocessing
The code includes comprehensive text cleaning:
- **Emoticon/Emoji Replacement**: Converts emoticons and emojis to text
- **Contraction Expansion**: Expands contractions (e.g., "won't" → "will not")
- **Case Normalization**: Converts to lowercase
- **Special Character Removal**: Removes numbers, punctuation, extra whitespace
- **Tokenization**: Splits text into individual words

### 3. Stemming and Lemmatization
- **Stemming**: Reduces words to their root form using Porter's algorithm
- **Lemmatization**: Converts words to their dictionary form using `textstem`

### 4. TF-IDF Feature Extraction
```r
# Create TF-IDF matrix
dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf_trim <- removeSparseTerms(dtm_tfidf, 0.99)
tfidf_mat <- as.matrix(dtm_tfidf_trim)
tfidf_scaled <- scale(tfidf_mat)
```

### 5. Dimensionality Reduction (PCA)
```r
# Perform PCA on TF-IDF features
pca_res <- prcomp(tfidf_scaled, center = TRUE, scale. = TRUE)
pca_df <- data.frame(doc_id = reviews_df$doc_id,
                     PC1 = pca_res$x[,1],
                     PC2 = pca_res$x[,2])
```

### 6. Sentiment Analysis
Uses the `syuzhet` package for sentiment scoring:
- **Method**: "syuzhet" dictionary-based approach
- **Output**: Sentiment scores and labels (positive/negative/neutral)
- **Visualization**: Bar charts of sentiment distribution

### 7. Clustering Algorithms

#### a) K-means Clustering
```r
set.seed(42)
k <- 3
kmeans_res <- kmeans(tfidf_scaled, centers = k, nstart = 25)
```

#### b) Hierarchical Clustering
```r
dist_mat <- dist(tfidf_scaled, method = "euclidean")
hclust_res <- hclust(dist_mat, method = "ward.D2")
hclust_clusters <- cutree(hclust_res, k = k)
```

#### c) DBSCAN Clustering
```r
# Automatic epsilon detection
kNNd <- kNNdist(as.matrix(pc_for_db), k = k_nn)
eps_guess <- as.numeric(quantile(kNNd, 0.90))
dbscan_res <- dbscan(as.matrix(pc_for_db), eps = eps_guess, minPts = 5)
```

### 8. Visualization
The code generates multiple visualizations:
- **PCA Plots**: 2D scatter plots of clustering results
- **Sentiment Distribution**: Bar charts of sentiment labels
- **Review Length Analysis**: Histogram of word counts
- **Word Clouds**: Visual representation of frequent words by sentiment
- **Rating Distribution**: Histogram and density plots of ratings
- **Correlation Heatmap**: Relationships between numeric variables

### 9. Evaluation Metrics
- **Silhouette Scores**: Measures clustering quality
- **Cluster-Sentiment Comparison**: Cross-tabulation of clusters vs sentiment labels
- **Agreement Analysis**: Comparison between different clustering methods

## 📈 Output Files
1. **Clustering Results**: Dataframe with cluster assignments and sentiment scores
2. **Visualizations**: Multiple plots showing different aspects of the analysis
3. **Evaluation Metrics**: Silhouette scores and cluster agreement statistics

## 🎨 Key Visualizations
- K-means, Hierarchical, and DBSCAN cluster plots on PCA-reduced space
- Sentiment distribution across different clustering methods
- Word clouds for positive, negative, and neutral sentiments
- Rating distribution and correlation heatmaps

## 📋 Usage
1. Run the installation code to install required packages
2. Update the file path to your `booking_reviews.csv` dataset
3. Execute the code sequentially from top to bottom
4. Review the generated visualizations and output files
5. Modify parameters (like sample size, number of clusters) as needed

## ⚙️ Customization
- Adjust sample size by changing the number in `sample(nrow(clean_data), 2000)`
- Modify number of clusters by changing `k <- 3`
- Adjust DBSCAN parameters (`eps_guess` and `minPts`) for different density requirements
- Change TF-IDF sparsity threshold in `removeSparseTerms()`

## 📊 Results Interpretation
- **Positive Clusters**: Groups of reviews with predominantly positive sentiment
- **Negative Clusters**: Groups with negative feedback patterns
- **Neutral Clusters**: Reviews with mixed or neutral sentiment
- **Outliers**: Points that don't fit well into any cluster (particularly in DBSCAN)

This comprehensive analysis provides insights into customer sentiment patterns, helps identify common themes in reviews, and can inform hotel management decisions based on customer feedback analysis.
