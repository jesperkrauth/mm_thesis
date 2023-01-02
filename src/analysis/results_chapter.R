### LOAD LIBRARIES ###
library(tidyverse) # essential Y
library(readr) # used to load in datasets Y
library(fixest) # feols Y
library(broom) # tidy model Y



### TABLE 5 ###
final_data <- read_rds('../../gen/data-preparation/output/final_diff_in_diff_data.rds')

# No fixed effects model
model_1 <- feols(rating ~ amazon_dummy*acquisition,
                 data = final_data,
                 cluster = ~ book_id
)

tidy_model_1 <- tidy(model_1, conf.int = TRUE)
tidy_model_1
summary(model_1)

# Time fixed effects only model
model_2 <- feols(rating ~ amazon_dummy*acquisition + 
                   t:amazon_dummy
                 |
                   t,
                 data = final_data,
                 cluster = ~ book_id
)

tidy_model_2 <- tidy(model_2, conf.int = TRUE)
tidy_model_2
summary(model_2)

# Both time fixed effects and book fixed effects model
model_3 <- feols(rating ~ amazon_dummy*acquisition + 
                   t:amazon_dummy
                 |
                   t + 
                   book_id,
                 data = final_data,
                 cluster = ~ book_id
)

tidy_model_3 <- tidy(model_3, conf.int = TRUE)
tidy_model_3
summary(model_3)


### TABLE 6 ###
# List of most popular genres
final_data %>% group_by(dominant_genre) %>% summarize(n = n()) %>% arrange(desc(n))

# 1 fiction model
genre1 <- final_data %>% filter(dominant_genre == 'fiction')
model_genre1 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre1,
                      cluster = ~ book_id
)

tidy_model_genre1 <- tidy(model_genre1, conf.int = TRUE)
tidy_model_genre1
summary(model_genre1)

# 2 non-fiction model
genre2 <- final_data %>% filter(dominant_genre == 'non-fiction')
model_genre2 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre2,
                      cluster = ~ book_id
)

tidy_model_genre2 <- tidy(model_genre2, conf.int = TRUE)
tidy_model_genre2
summary(model_genre2)

# 3 mystery, thriller, crime model
genre3 <- final_data %>% filter(dominant_genre == 'mystery, thriller, crime')
model_genre3 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre3,
                      cluster = ~ book_id
)

tidy_model_genre3 <- tidy(model_genre3, conf.int = TRUE)
tidy_model_genre3
summary(model_genre3)

# 4 romance model
genre4 <- final_data %>% filter(dominant_genre == 'romance')
model_genre4 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre4,
                      cluster = ~ book_id
)

tidy_model_genre4 <- tidy(model_genre4, conf.int = TRUE)
tidy_model_genre4
summary(model_genre4)

# 5 children model
genre5 <- final_data %>% filter(dominant_genre == 'children')
model_genre5 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre5,
                      cluster = ~ book_id
)

tidy_model_genre5 <- tidy(model_genre5, conf.int = TRUE)
tidy_model_genre5
summary(model_genre5)

# 6 history, historical fiction, biography model
genre6 <- final_data %>% filter(dominant_genre == 'history, historical fiction, biography')
model_genre6 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre6,
                      cluster = ~ book_id
)

tidy_model_genre6 <- tidy(model_genre6, conf.int = TRUE)
tidy_model_genre6
summary(model_genre6)

# 7 fantasy, paranormal model
genre7 <- final_data %>% filter(dominant_genre == 'fantasy, paranormal')
model_genre7 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre7,
                      cluster = ~ book_id
)

tidy_model_genre7 <- tidy(model_genre7, conf.int = TRUE)
tidy_model_genre7
summary(model_genre7)

# 8 young-adult model
genre8 <- final_data %>% filter(dominant_genre == 'young-adult')
model_genre8 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre8,
                      cluster = ~ book_id
)

tidy_model_genre8 <- tidy(model_genre8, conf.int = TRUE)
tidy_model_genre8
summary(model_genre8)

# 9 comics, graphic model
genre9 <- final_data %>% filter(dominant_genre == 'comics, graphic')
model_genre9 <- feols(rating ~ amazon_dummy*acquisition + 
                        t:amazon_dummy
                      |
                        t + 
                        book_id,
                      data = genre9,
                      cluster = ~ book_id
)

tidy_model_genre9 <- tidy(model_genre9, conf.int = TRUE)
tidy_model_genre9
summary(model_genre9)

# 10 poetry model
genre10 <- final_data %>% filter(dominant_genre == 'poetry')
model_genre10 <- feols(rating ~ amazon_dummy*acquisition + 
                         t:amazon_dummy
                       |
                         t + 
                         book_id,
                       data = genre10,
                       cluster = ~ book_id
)

tidy_model_genre10 <- tidy(model_genre10, conf.int = TRUE)
tidy_model_genre10
summary(model_genre10)




### FIGURE 4 ###
plot_data <- rbind(tidy_model_genre1, tidy_model_genre2, tidy_model_genre3, tidy_model_genre4, tidy_model_genre5, tidy_model_genre6, tidy_model_genre7, tidy_model_genre8, tidy_model_genre9, tidy_model_genre10)
plot_data <- plot_data %>% filter(term == "amazon_dummy:acquisition") %>% rowid_to_column("genre")
plot_data$genre <- as.factor(plot_data$genre)

theme_set(
  theme_classic() +
    theme(legend.position = "top")
)
error_plot_2 <- ggplot(plot_data, aes(x = genre, y = estimate, ymin = estimate-1.96*std.error, ymax = estimate+1.96*std.error)) + 
  geom_errorbar(width = 0.2) + 
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linetype = "dashed") + 
  ylab("Estimate") + 
  xlab("Book Genre")
error_plot_2
ggsave("../../gen/analysis/output/error_plot_2.png", error_plot_2)



### TABLE 7: FINDING 'DOMINANT' GENRES ###
# Mean for all genres on Goodreads before the merger
final_data %>% filter(acquisition==0 & amazon_dummy==0) %>% group_by(dominant_genre) %>% summarize(mean_genre = mean(rating))
# Total mean Goodreads before the merger
final_data %>% filter(acquisition==0 & amazon_dummy==0) %>% summarize(mean_total = mean(rating))

# Mean for all genres on Amazon before the merger
final_data %>% filter(acquisition==0 & amazon_dummy==1) %>% group_by(dominant_genre) %>% summarize(mean_genre = mean(rating))
# Total mean Amazon before the merger
final_data %>% filter(acquisition==0 & amazon_dummy==1) %>% summarize(mean_total = mean(rating))



### FIGURES A1 & 5 ###
vader_sample2 <- read_rds('../../gen/data-preparation/output/vader_sample2.rds')

# Positive-negative ratio/month for Amazon
pnratio_m_amazon <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon %>% select(t, pos_neg_ratio)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon = pos_neg_ratio)

# Positive-negative ratio/month for Goodreads
pnratio_m_goodreads <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads = pos_neg_ratio)

# Merge both temps together as plot data
plot_data <- temp %>% left_join(temp2, by = "t")
plot_data <- drop_na(plot_data)

# Create plot over time (APPENDIX 1)
pos_neg_full_plot <- ggplot(plot_data, (aes(x = t))) + 
  geom_line(aes(y = pos_neg_ratio_goodreads), color = "red") +
  geom_line(aes(y = pos_neg_ratio_amazon), color = "blue") + 
  geom_vline(xintercept = 186, linetype = "longdash") + 
  ylab("Positive-Negative Ratio") + 
  xlab("Number of months since the first review") + 
  #  ggtitle("Average book rating on Amazon and Goodreads") +
  theme_bw() + 
  annotate("text", x=200, y=8.5, label="Amazon", size = 5.5, color = "blue") + 
  annotate("text", x=205, y = 4.3, label = "Goodreads", size = 5.5, color = "red") + 
  annotate("text", x = 170, y = 3, label = "Acquisition", size = 5) +
  theme(plot.title = element_text(hjust = 0.5))
pos_neg_full_plot
ggsave('../../gen/analysis/output/pos_neg_full_plot.png', pos_neg_full_plot)

# Zooming in on periods 30 months before and after acquisition, i.e. t = 150 til t = 210 (FIGURE 5)
plot_data2 <- plot_data %>% filter(t >= 156 & t <= 216)
pos_neg_30_day_plot <- ggplot(plot_data2, (aes(x = t))) + 
  geom_line(aes(y = pos_neg_ratio_goodreads), color = "red") +
  geom_line(aes(y = pos_neg_ratio_amazon), color = "blue") + 
  geom_vline(xintercept = 186, linetype = "longdash") + 
  ylab("Positive-Negative Ratio") + 
  xlab("Number of months since the first review") + 
  theme_bw() + 
  annotate("text", x=200, y=8.5, label="Amazon", size = 5.5, color = "blue") + 
  annotate("text", x=205, y = 4.3, label = "Goodreads", size = 5.5, color = "red") + 
  annotate("text", x = 178, y = 4.07, label = "Acquisition", size = 5) +
  theme(plot.title = element_text(hjust = 0.5))
pos_neg_30_day_plot
ggsave('../../gen/analysis/output/pos_neg_30_day_plot.png', pos_neg_30_day_plot)




### FIGURES 6 & A2 ###
vader_sample2 %>% group_by(dominant_genre) %>% summarise(n = n()) %>% arrange(desc(n))

genre_data <- vader_sample2 %>%
  filter(vader_sample != 'neutral') %>%
  group_by(t, dominant_genre, Label, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
genre_data <- genre_data %>% na.omit()  
genre_data <- genre_data %>% select(t, dominant_genre, Label, pos_neg_ratio)
genre_data <- distinct(genre_data)
saveRDS(genre_data, '../../gen/analysis/output/genre_data.rds')

# Let's only use data from the periods 150-210, i.e., 30 months before and after the acquisition
genre_data2 <- genre_data %>% filter(t >= 156 & t <= 216)
# Furthermore let's only use data from the top 4 dominant genres
genre_data2 <- genre_data2 %>% filter(dominant_genre == 'fiction' | dominant_genre == 'non-fiction' | dominant_genre == 'mystery, thriller, crime' | dominant_genre == 'history, historical fiction, biography')

# Pos-neg ratios for 4 biggest book genres (FIGURE 6)
pos_neg_genres_30_days <- ggplot(genre_data2, aes(x = t, y = pos_neg_ratio, colour = Label)) + 
  geom_line() + 
  facet_wrap(~ dominant_genre, ncol = 2) +
  geom_vline(xintercept=186, linetype = "dashed") +
  xlab("Number of months since the first review") + 
  ylab("Positive-Negative Ratio")
pos_neg_genres_30_days
ggsave('../../gen/analysis/output/pos_neg_genres_30_days.png', pos_neg_genres_30_days)

# Make plot with remaining 6 genres (FIGURE A2)
genre_data2 <- genre_data %>% filter(t >= 156 & t <= 216)
genre_data2 <- genre_data2 %>% filter(dominant_genre == 'young-adult' | dominant_genre == 'romance' | dominant_genre == 'fantasy, paranormal' | dominant_genre == 'children' | dominant_genre == 'comics, graphic' | dominant_genre == 'poetry')
pos_neg_genres_30_days_smaller_6 <- ggplot(genre_data2, aes(x = t, y = pos_neg_ratio, colour = Label)) + 
  geom_line() + 
  facet_wrap(~ dominant_genre, ncol = 2) +
  geom_vline(xintercept=186, linetype = "dashed") +
  xlab("Number of months since the first review") + 
  ylab("Positive-Negative Ratio")
pos_neg_genres_30_days_smaller_6
ggsave('../../gen/analysis/output/pos_neg_genres_30_days_smaller_6.png', pos_neg_genres_30_days_smaller_6)
