---
title: "Lab exercise: Text analytics in R"
knit: (function(input_file, encoding) {
  out_dir <- 'docs';
  rmarkdown::render(input_file,
 encoding=encoding,
 output_file=file.path(dirname(input_file), out_dir, 'index.html'))})
output: 
  html_document: 
    keep_md: yes
---


## Objectives
1. Create tidy text in R
2. Explore word frequency and generate wordcloud in R
3. Perform sentiment analysis 
4. Analyze relationship between words

## Overview 

Please make sure to install and load the following packages. 


```r
# Basic data manipulation
library(tidyverse)
# Tidy text package
library(tidytext)
```


In this lab we will check out the tidy text and sentiment analysis of the covid Tweets in R. Load the dataset in R. 


```r
covid <- read_csv("data/covid19_tweets.csv")
```

The tweets contains #covid19 hashtag. Check out the column descriptions below 

Column | Description
------------- | -------------
User_name  | User name
User_location  | user location
User_description|user description
User_created|when the user was created
User_followers|number of followers of this user
User_friends|number of friends of this user
User_favourites|number of favourites for this user
User_verified|is the user verified or not
date|tweet date
text|text of the tweet
hashtags|hastages
source|device posting this tweet
is_retweet| is a retweet or not



```r
glimpse(covid)
```

All the columns are already in a proper format. We will focus on the contents of these tweets.  However, there is no key or id column for this dataset for rapid random lookups and efficient access for each tweet. So we can simply create a id column. The id can uniquely identify a certain post of tweet in the dataset. 


```r
covid$ID <- seq.int(nrow(covid))
```


## 1. Tidy text format

Check out the text column in the covid dataset. The content is still in the raw text format, which include punctuations, user names, links etc. Let's first remove these unnecessary characters. 

The `gsub` function will perform replacement of certain characters and patterns in a string. We will directly overwrite the output to the `text` colum. In the gsub function, the first argument is the pattern we are looking for, and the second argument is the replacement for matched pattern. We leave the second argument as blank strings to remove the unnecessary patterns. The third argument is the text column we are working on. 


```r
# Remove &amp
covid$text <- gsub("&amp", "", covid$text)
# Remove numbers in text
covid$text <- gsub("[[:digit:]]+", "", covid$text)
# Remove punctuation
covid$text <- gsub("[[:punct:]]", "", covid$text)
# Remove @user names
covid$text <- gsub("@\\w+", "", covid$text)
# Remove hashtags
covid$text <- gsub("#\\w+", "", covid$text)
# Remove links in text. 
covid$text <- gsub("http\\w+", "", covid$text)
# Remove blank spaces at the beginning
covid$text  <- gsub("^ ", "", covid$text )
# Remove blank spaces at the end
covid$text  <- gsub(" $", "", covid$text )
```

Currently the data is one-tweet per row. We need to convert and transform the table into a tidy text format first. Recall a tidy text format is a table with one-token-per-row. We will use the basic tokenization process to break down the text into individual words. Under the `unnest_tokens` function, we name the output column as "term", and the input column is `text` in the covid dataset. We set the unit of token at words level. The tokens are stored into a new dataset calle covid_token. 



```r
covid_token <- covid %>%
    unnest_tokens(term, text,token = "words")
```

Now that the covid_token data is in one-word-per-row format. Examine the covid_token dataset. All the other attributes e.g. user name are preserved, and each tweets are split into multiple terms, so we get a longer table in this tidy text format. The column called "term" are the tokens. 

Next let's remove the stop words, which does not add much meaning to a sentence (e.g. to, from, at). The `stop_words` dataset in the `tidytext` package contains stop words from three dictionaries:"onix", "SMART", or "snowball". Once we load the stop words, we can use `anti_join` function to exclude these words in the covid_token dataset. We call the output `tidy_covid` . 


```r
data(stop_words)

tidy_covid <- covid_token %>%
    anti_join(stop_words, by=c("term"="word"))
```

Examine the tidy_covid dataset, which is under the tidy text format. 

## 2. Word frequency

We can also use `count()` to find the most common words for these tweets.


```r
word_covid <- tidy_covid %>%
    count(term, sort = TRUE)

# Just plot out the terms with frequency above 2000. 
word_covid %>%
    filter(n > 2000) %>%
    mutate(word = reorder(term, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
```

Again the term covid is not informative. We can manually exclude terms e.g. covid as the stop words. Let's check out the bar chart again to see what are people talking about covid: 


```r
my_stopwords <- tibble(word = c("covid","coronavirus"))

tidy_covid <- tidy_covid %>% 
    anti_join(my_stopwords, by=c("term"="word"))
word_covid <- tidy_covid %>%
    count(term, sort = TRUE)

# Just plot out the terms with frequency above 2000. 
word_covid %>%
    filter(n > 2000) %>%
    mutate(word = reorder(term, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
```


## 3. Sentiment analysis

Let’s move to sentiment analysis to understand their opinion toward the covid. Recall the sentiment analysis is a dictionary-based method. Let's load the three dataset:these sentiment label codings are made by people, through crowdsorcing, etc.

- Afinn: score each term between [-5, 5], 

- Bing: score each term as positive/negative sentiment

- NRC: provides primary emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and 2 sentiments (positive and negative)


```r
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
```

Let's first evaluate the sentiment for tweets based on afinn dictionary. Note this is a evaluation in numbers from -5 to 5. 


```r
covid_affin <- tidy_covid %>%
    inner_join(get_sentiments("afinn"), by=c("term"="word"))
```

Each term now is assigned a sentiment score (column `value`). We can further explore the pattern of the sentiment score in the dataset. 

For example, how is the sentiment scores change over time? Let's check out the pattern by each day. We assume that the sentiment content of tweet per day is the sum of the sentiment content of the individual words.


```r
# Get date as a new column
library(lubridate)

covid_affin$dday <- date(covid_affin$date)

# summary sentiment score for each day

covid_affin %>% group_by(dday) %>%
    summarise(sentiment = sum(value)) %>% ggplot()+geom_line(aes(dday, sentiment))+theme_minimal()
```

Check out the plot you generated above. In general, the sentiment score per day is negative. Note this summary is based on the sentiment content of the individual words for all the tweets for a certain day, so the score can be biased by total number of available tweets. In this dataset, July 25th has the most amount of tweets about covid, at the same time, has the lowest sentiment score. 

It is also interesting to see that on August 11th, the overall sentiment is positive. Let's further examine what are the positive terms mentioned in that day. 

Here we switch to another dictionary, `bing` lexicon to examine the positive and negative terms. The sentiment labels are stored in the new column called `sentiment`. 


```r
# Gete date for the tidy text format data
tidy_covid$dday <- date(tidy_covid$date)

# inner join with the bing dictionary
covid_bing <- tidy_covid %>%
    inner_join(get_sentiments("bing"), by=c("term"="word"))
```

Let's dive into Aug 11th to see what are the most frequently mentioned positive and negative terms. We summarize the word counts by positive and negative. 


```r
Aug11 <- covid_bing %>% filter(dday=="2020-08-11")

Aug_count <- Aug11 %>% count(term, sentiment, sort = TRUE) 
```


Let's generate two bar chart side by side, to see the positive and negative terms on Aug 11th. 


```r
Aug_sentiment <- Aug_count%>%
    group_by(sentiment)  %>% # Group by two sentiment groups
    top_n(10) %>% # Pick top 10 each group
    mutate(word = reorder(term, n)) 
    
# Plot out the bar chart from highest to lowest values
ggplot(data=Aug_sentiment,aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    # terms vary across columns, set scales as free_y
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Word count by sentiment",x = NULL) +
    coord_flip()
```

For the top 10 positive terms, you will soon realize that the term "positive" by default is positive, but under this covid scenario, it should indicate something negative since it might actually record they got covid test positive, or how many positive cases are there. In addition, the term "trump" should not be a positive nor negative term. We need to further refine the sentiment categories. 

Let's change term "positive" as a negative term (same for affin dictionary, it is originally lables as score 2. You can change it into -2 too) for this project, and remove term "trump" in the list since it should be a netrual term. Then let's generate a new bar graph for positive and negative terms on Aug 11th. 


```r
# assign sentiment as negative for term positive
covid_bing[covid_bing$term== "positive",]$sentiment <- "negative"

# exclude term "trump" for each row
covid_bing2 <- covid_bing[!covid_bing$term=="trump",]

# Create a new plot
Aug11_2 <- covid_bing2 %>% filter(dday=="2020-08-11")

Aug_count2 <- Aug11_2 %>% count(term, sentiment, sort = TRUE)

Aug_count2%>%
    group_by(sentiment)  %>% # Group by two sentiment groups
    top_n(10) %>% # Pick top 10 each group
    mutate(word = reorder(term, n)) %>% # Plot out the bar chart from highest to lowest values
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Word count by sentiment",x = NULL) +
    coord_flip()
```

Now the bar chart makes more sense. We can generate a wordcloud to visualize the content. 

For example, instead of using pure word frequency, we can generate a wordcloud for the most positive or negative terms overall for the entire dataset. 

In the wordcloud function, we specify the word to be plotted is from `term`. Since all the terms are ordered my number of occurrence in the dataset (in count, sort=T), we use `n` as the frequencies. In this case, the most common word will be displayed in this wordcloud. We set 100 number of words to be plotted in this figure. 


```r
library(wordcloud)
covid_bing2 %>% count(term, sentiment, sort = TRUE) %>% 
    with(wordcloud(term, n, max.words = 100))
```

However, it is hard to tell if it is a positive or negative term. We can generate a comparison wordcloud to color terms differently based on their sentiment.The `comparison.cloud` function will plot a cloud comparing the frequencies (here is count) of words across documents. Here we color positive term in blue, and negative term in red. 

 

```r
library(reshape2)
library(wordcloud)
covid_bing2 %>% count(term, sentiment, sort = TRUE) %>%
    acast(term ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("red", "blue"),
                     max.words = 100)
```

## 4. Sentiment pattern over time

First, We can further improve the sentiment trend over time based on the affin dictionary. Below we changed term "positive" as score -2 instead of 2. 
Now the sentiment trend is more reliable. 


```r
# change score for term positive
covid_affin[covid_affin$term== "positive",]$value <- -2

covid_affin %>% group_by(dday) %>%
    summarise(sentiment = sum(value)) %>% ggplot()+geom_line(aes(dday, sentiment))+
  theme_minimal()+labs(title="Sentiment based on afinn")
```

We can also generate sentiment trend based on the bing dictionary. Originally each term is labeled in positive or negative, we can first count how many positive and negative terms per day, and calculate a new column as sentiment score which is the difference between the number of positive and negative term. Check out the plot below. The overall pattern is similar to the trend we get using afinn dictionary. 


```r
covid_sentiment_bing <- covid_bing2 %>%
    count(dday,sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

ggplot(covid_sentiment_bing, aes(dday, sentiment)) +
    geom_line()+theme_minimal()+labs(title="Sentiment based on bing")
```

Similarly, we can use the `nrc` dictionary to plot out sentiment trend by groups too. Note nrc dictionary will label terms into 10 different emotional groups. Let's check out how many terms are mentioned each day per group. For the nrc dictionary, there are lots of terms are labeled as positive. It could potentially influence our analysis. 


```r
# label each term using nrc dictionary
covid_nrc<- tidy_covid %>% 
    inner_join(get_sentiments("nrc"), by=c("term"="word"))

# change term positive as negative sentiment

covid_nrc[covid_nrc$term== "positive",]$sentiment <- "negative"

# summary positive and negative terms for plot.
# Here we plot total terms per group per day
nrc_count <- covid_nrc%>% count(dday,sentiment) 

ggplot(nrc_count)+geom_line(aes(x=dday, y=n, color=sentiment),
            size=2)+scale_color_brewer(palette="Set3")+
    theme_minimal()+labs(title="Word frequency for emotional groups through time")
```

## 5. Relationship between words: n-grams

So far we’ve just considered single words as individual units to understand these tweets. However, many interesting text analyses are based on the relationships between different combination of consecutive words. For example we can explore which words tend to follow others immediately, or any terms usually tend to co-occur within the same tweet. 


Let's go back to our original tweet dataset. This time we will create token as 2, which means we split document into pairs of adjacent words rather than by individual ones. The relationship between words can help to construct network plot. 

Let first set up tokens in pairs. We are extracting pairs of two consecutive words for each tweet, which is often called “bigrams”. We name the new column as `pair`.


```r
covid_bigram <- covid %>%
    unnest_tokens(pair, text,token = "ngrams", n = 2)
```

Examine the `covid_bigram` data. The last column `pair` represents the pair of words. 

Note we have not excluded the stop words. Below we exclude all the stop words for each term under column `pair`. First, we need to re-structure "pair" column to split two terms into two column: word1 and word2. Next we can anti join the stop words respectively. 



```r
# Separate two terms apart. 
bigrams_separated <- covid_bigram %>%
  separate(pair, c("word1", "word2"), sep = " ")


# Exclude stop words for each column: word1 and word2
bigrams_tidy <- bigrams_separated %>%
    anti_join(stop_words, by=c("word1"="word")) %>% 
    anti_join(stop_words, by=c("word2"="word"))
```

We can now explore sentiment using these bigrams. Note in section 3&4 we just consider each single word as the unit for sentiment analysis. However, it is problematic since that a word’s context can be totally opposite when consider the word next to it. For example, "I am not feeling well today" means something negative. However, the term "well" will conclude that it is positive.  

Now that we have the data re-structured into pairs, it’s easy to tell how many words follow terms like “not”. We could use this to ignore or even reverse their contribution toward the sentiment score.


```r
# You can not use bigrams_tidy here since term not is excluded based on stop words. 
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
```

So how can we evaluate the impact of these terms which follow "not" for sentiment analysis? Let's use the afinn dictionary again here.


```r
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)
```

In this `not_words`table, it summarizes the most sentiment-associated terms which follows 'not'. e.g. not safe, not alone. Again since these pairs of terms indicate inversed sentiment direction compared with the term itself. Let's evaluate its impact toward the “wrong” direction. In this `not_words`table, `value` means the score assigned from afinn dictionary, and n is the number of times they appear in the entire tweet dataset. 

Let's generate a new score column, which multiplies sentiment value by the number of times they appear. It incidates the "contribution" of sentiment toward the wrong direction. Here top ones are "not good", "not true", "not safe" etc. Apparently you can tell that people are feeling panic. 


```r
not_words %>%
  mutate(score = n * value) %>%
  arrange(desc(abs(score))) %>%
  head(20) %>%
    # reorder the levels in word2. 
  mutate(word2 = reorder(word2, score)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment contribution",y = "Words follows \"not\"")
```

Now you can go back to the sentiment analysis using afinn dictionary, to reverse the AFINN values of each word that follows a negation for a more reliable results. For example, originally term `safe` is labled as positive 1 based on afinn dictionary. Actually in the raw text, it means `not safe`. We can change it into -1 and check out the sentiment trend over time again. 


```r
# change score for term positive
covid_affin[covid_affin$term== "safe",]$value <- -1

covid_affin %>% group_by(dday) %>%
    summarise(sentiment = sum(value)) %>% ggplot()+geom_line(aes(dday, sentiment))+
  theme_minimal()+labs(title="Sentiment based on afinn")
```


## 6. Visualizing a network plot

Let's find out ways to visualize relationship between words. We can arrange the words into a network instead of a simple wordcloud. 

This time the figure, or graph is a combination of connected nodes. Note a network includes: 

- from: the node an edge is coming from

- to: the node an edge is going towards

- weight: A numeric value associated with each edge


Text From [here](https://www.tidytextmining.com/ngrams.html)

We will use the `igraph` package to generate networks for pairs of words, and color the network by positive relationship and negative relationship.

Here we assume that the relationship is determined by the sentiment of the first term in a pair of words. We join the `bing` dictionary by the first word. 

Then we summarize the count for these words in pairs. Since we need to label the network by positive and negative terms, we need to count the word frequency by sentiment group too. The bigram_counts data summarizes how many times each pair of words appear in these tweets. 


```r
bigram_counts <- bigrams_tidy %>%
   inner_join(get_sentiments("bing"), by = c(word1 = "word")) %>%
    count(word1, word2, sentiment, sort = TRUE)
```

Next, we need to generate a igraph object from this tidy data. The graph_from_data_frame() function takes a clean data frame of edges with columns for “from” (word1), “to” (word2), and edge attributes (in this case n, how many time they appear in the dataset):


```r
library(igraph)
bigram_graph <- bigram_counts %>%
    filter(n > 30) %>% # customize frequency 
    graph_from_data_frame()
```


The bigram_graph object includes the relationship between each terms in pairs. We also assign different colors for positive and negative relationship.  Now we can plot out the graph:


```r
library(ggraph)
ggraph(bigram_graph, layout = 'fr') +
    geom_edge_link(aes(colour = sentiment)) +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
```


Based on this figure, it is obvious to see that people mentioned a lot about "challenging times", "fake news", and "rapid testing". 



## 7. Summary

In this lab, we explore the tweets with hashtag covid from July 2020 to August 2020. Here are a few tips for text analytics:

1) Get your dataset clean. Note the raw text data could be very messy: special characters, typos,spaces, tabs, abbreviations etc. 

2) Then you need to determine what level do you want to explore to crete your tokens: single word? pair of words? or sentences?

3) After generate tokens in a proper unit, you need to exclude stop words. Note based on different scenarios, you might need to manually exclude some words which are not provided by the stop words dataset

4) We have introduced three different lexicons for sentiment analysis. You can use any of these in the future, just depend on how you want to score your terms: in numeric format? or binary classes? 

5) You need to take care of terms which follow a negation. Usually you can check out bigrams. 

6) There is no standard workflows for text analysis. For example, in this lab, you can continue exploring different emotions of tweets between different user groups (personal account or institutional account?) or different countries. The ultimate goal here is using text to understand what people are thinking about. 







