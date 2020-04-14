library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(RCurl)
library(XML)
library(purrr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plotly)

# NOTE: My IP address was temporarly blocked by amazon for web crawling. 
# Becareful and only run the 'scrape_amazon' function when necessary to avoid being blocked.


######################################################################################
############################### Scraping Amazon Reviews############################### 
######################################################################################
# A couple of example products to use
# PRODUCT NAME        PROD CODE       REVIEW COUNT

# Iphone case         B07HRJL27Z      (6000+ reviews) 
# Iphone Charger      B01F9RH5M4      (2000 reviews)

# change product code to item you want to analyze
prod_code = 'B01F9RH5M4'

url <- paste0("https://www.amazon.ca/product-reviews/", prod_code)

doc <- read_html(url)

# Get product name
productName <- html_nodes(doc, '.a-text-ellipsis .a-link-normal')%>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  trimws()

# Get number of reviews for product
reviewCount <- html_nodes(doc, '#filter-info-section .a-size-base')%>% 
  html_text()
reviewCount <- substr(reviewCount, 17, 30)
reviewCount <- gsub(" reviews", "", reviewCount)
reviewCount <- gsub(",", "", reviewCount)


# Number of iterations of for loop
pages <- floor(as.numeric(reviewCount) / 10)
# Function to gather text from reviews 
scrape_amazon <- function(url, throttle = 0){
  
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  doc <- read_html(url)
  
  # Parse relevant elements from HTML
  title <- html_nodes(doc, '.a-text-bold span')%>% 
    html_text()
  
  author <- html_nodes(doc, '.a-profile-name')%>% 
    html_text()
  author <- author[-c(1,2)]
  
  date <- html_nodes(doc, '.review-date')%>% 
    html_text() 
  date <- date[-c(1,2)]
  
  stars <- html_nodes(doc, '.a-icon-alt')%>% 
    html_text()
  stars <- stars[-c(1,2,3,14,15,16,17,18)]
  
  review <- html_nodes(doc, '.review-text-content span')%>% 
    html_text() 

  # Combine attributes into a single data frame
  df <- data.frame(title, author, date, stars, review, stringsAsFactors = F)
}

#create master df for review data 
reviews_all <- data.frame()

# Loop through pages and fill master df
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.ca/product-reviews/",prod_code,"/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(productName, reviews))
}

#Clean up columns 
reviews_all$stars <- substr(reviews_all$stars,1,nchar(reviews_all$stars) -15)
reviews_all$stars <- as.numeric(reviews_all$stars)
reviews_all$date <- as.Date(reviews_all$date, format = "%B %d, %Y")

# REMOVE ALL NON ASCII CHARACTERS ********
reviews_all$title <- gsub("[^\x01-\x7F]", "", reviews_all$title)
reviews_all$author <- gsub("[^\x01-\x7F]", "", reviews_all$author)
reviews_all$review <- gsub("[^\x01-\x7F]", "", reviews_all$review)


############################################################################################
######################################## Write to CSV ######################################
############################################################################################

write.csv(reviews_all, file = "AmazonReviews.csv")


###############################################################################################
################################### End of scraping process ###################################
###############################################################################################



###############################################################################################
##################################### Refining Review Text ####################################
###############################################################################################

# Refine reviews into text corpus
corpusReviews <- Corpus(VectorSource(reviews_all$review)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

# Create term-document matrices & remove sparse terms
tdmReviews <- DocumentTermMatrix(corpusReviews) %>%
  removeSparseTerms(1 - (5/length(corpusReviews)))

# Calculate and sort by word frequencies
word.freqReviews <- sort(colSums(as.matrix(tdmReviews)), 
                    decreasing = T)

# Create word frequency table
tableReviews <- data.frame(word = names(word.freqReviews), 
                      absolute.frequency = word.freqReviews, 
                      relative.frequency = 
                        word.freqReviews/length(word.freqReviews))
rownames(tableReviews) <- NULL

###################################################################################################
####################################### Refining Title Text #######################################
###################################################################################################

# Refine titles into text corpus
corpusTitles <- Corpus(VectorSource(reviews_all$title)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

# Create term-document matrices & remove sparse terms
tdmTitles <- DocumentTermMatrix(corpusTitles) %>%
  removeSparseTerms(1 - (5/length(corpusTitles)))

# Calculate and sort by word frequencies
word.freqTitles <- sort(colSums(as.matrix(tdmTitles)), 
                         decreasing = T)

# Create word frequency table
tableTitles <- data.frame(word = names(word.freqTitles), 
                           absolute.frequency = word.freqTitles, 
                           relative.frequency = 
                             word.freqTitles/length(word.freqTitles))
rownames(tableTitles) <- NULL


############################################################################################################
############################## Remove words that also appear in product name ###############################
############################################################################################################

productName <- gsub('[[:punct:] ]+',' ',productName)

parseProdName <- Corpus(VectorSource(productName)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

tdmProdName <- DocumentTermMatrix(parseProdName)

prodName.words <- sort(colSums(as.matrix(tdmProdName)), 
                       decreasing = T)

tableProdName <- data.frame(word = names(prodName.words))

tableReviews <- tableReviews[!(tableReviews$word %in% tableProdName$word),]
tableTitles <- tableTitles[!(tableTitles$word %in% tableProdName$word),]


############################################################################################################
############################################ BASIC Visuals #################################################
############################################################################################################

#word cloud to display word freq for titles
wordcloud(words = tableTitles$word, freq = tableTitles$absolute.frequency, scale= c(3,0.8), min.freq = 1,
          max.words = 50, random.order = FALSE, rot.per = 0.2, colors = brewer.pal(8, "Dark2"))

#word cloud to display word freq for reviews
wordcloud(words = tableReviews$word, freq = tableReviews$absolute.frequency, scale= c(3,0.5), min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.25, colors = brewer.pal(8, "Dark2"))

# Basic bar plot for product rating distribution
counts <- table(reviews_all$stars)
barplot(counts, main = "Review Rating Distribution",
        xlab = "Count", ylab = "Stars", col= "yellow", horiz = TRUE)


# Bar plot for top 20 words and their freq
top20words <- tableReviews %>% top_n(20)
ggplot(data = top20words, aes(x= reorder(word, -absolute.frequency), y= absolute.frequency)) + 
  geom_bar(stat="identity", color= "black", fill= "darkgreen") + 
  labs(title = "Top 20 Most Freq Words", x= "",  y= "") +
  theme(axis.text.x = element_text(size = 12, color = "black"), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, color = "black"), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, color = "black", hjust= 0.5))

# Histogram of review length (# of words)
reviewLength <- sapply(strsplit(reviews_all$review, " "), length)
ggplot(reviews_all, aes(x=sapply(strsplit(reviews_all$review, " "), length))) + 
  geom_histogram(color= "black", fill= "darkgreen", binwidth = 5) + 
  labs(title = "Review Text Length Distibution", x= "Word Count",  y= "") +
  theme(axis.text.x = element_text(size = 12, color = "black"), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, color = "black"), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, color = "black", hjust= 0.5)) 

# Cluster dendrogram (doesnt really work)
d <- dist(top20words$word, method = "euclidean")
fit <- hclust(d, method="complete")
plot(fit)








