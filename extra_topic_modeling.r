library(topicmodels)
word_covid2 <- tidy_covid %>%
    count(term, ID, sort = TRUE)
covid_dtm <- word_covid2 %>%
    cast_dtm(ID, term, n)
covid_lda <- LDA(covid_dtm, k = 4, control = list(seed = 1234))

covid_topics <- tidy(covid_lda, matrix = "beta")
top_terms <- covid_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 5) %>% 
    ungroup() %>%
    arrange(topic, -beta)
library(ggplot2)

top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()

covid_gamma <- tidy(covid_lda, matrix = "gamma")
covid_gamma <- covid_gamma %>%
    separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
covid_gamma %>%
    mutate(title = reorder(title, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ title) +
    labs(x = "topic", y = expression(gamma))
