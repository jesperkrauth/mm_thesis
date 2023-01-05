#########################
### RESULTS CHAPTER #####
#########################

### LOAD LIBRARIES ###
library(tidyverse)
library(readr)
library(fixest)
library(broom)


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




### TABLE 7 & A2 ###
vader_sample2 %>% group_by(dominant_genre) %>% summarise(n = n()) %>% arrange(desc(n))
# Fiction
pnratio_m_amazon_fiction <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'fiction') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_fiction %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_fiction = pos_neg_ratio)

pnratio_m_goodreads_fiction <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'fiction') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_fiction %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_fiction = pos_neg_ratio)

overlap_data_fiction <- temp %>% left_join(temp2, by = "t")
overlap_data_fiction <- drop_na(overlap_data_fiction)
overlap_data_fiction <- overlap_data_fiction %>% filter(t >= 156 & t <= 216)

mean(overlap_data_fiction$pos_neg_ratio_amazon_fiction)
min(overlap_data_fiction$pos_neg_ratio_amazon_fiction)
max(overlap_data_fiction$pos_neg_ratio_amazon_fiction)

mean(overlap_data_fiction$pos_neg_ratio_goodreads_fiction)
min(overlap_data_fiction$pos_neg_ratio_goodreads_fiction)
max(overlap_data_fiction$pos_neg_ratio_goodreads_fiction)

# Non-fiction
pnratio_m_amazon_non_fiction <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'non-fiction') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_non_fiction %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_non_fiction = pos_neg_ratio)

pnratio_m_goodreads_non_fiction <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'non-fiction') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_non_fiction %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_non_fiction = pos_neg_ratio)

overlap_data_non_fiction <- temp %>% left_join(temp2, by = "t")
overlap_data_non_fiction <- drop_na(overlap_data_non_fiction)
overlap_data_non_fiction <- overlap_data_non_fiction %>% filter(t >= 156 & t <= 216)

mean(overlap_data_non_fiction$pos_neg_ratio_amazon_non_fiction)
min(overlap_data_non_fiction$pos_neg_ratio_amazon_non_fiction)
max(overlap_data_non_fiction$pos_neg_ratio_amazon_non_fiction)

mean(overlap_data_non_fiction$pos_neg_ratio_goodreads_non_fiction)
min(overlap_data_non_fiction$pos_neg_ratio_goodreads_non_fiction)
max(overlap_data_non_fiction$pos_neg_ratio_goodreads_non_fiction)

# Mystery, thriller, crime
pnratio_m_amazon_mtc <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'mystery, thriller, crime') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_mtc %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_mtc = pos_neg_ratio)

pnratio_m_goodreads_mtc <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'mystery, thriller, crime') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_mtc %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_mtc = pos_neg_ratio)

overlap_data_mtc <- temp %>% left_join(temp2, by = "t")
overlap_data_mtc <- drop_na(overlap_data_mtc)
overlap_data_mtc <- overlap_data_mtc %>% filter(t >= 156 & t <= 216)

mean(overlap_data_mtc$pos_neg_ratio_amazon_mtc)
min(overlap_data_mtc$pos_neg_ratio_amazon_mtc)
max(overlap_data_mtc$pos_neg_ratio_amazon_mtc)

mean(overlap_data_mtc$pos_neg_ratio_goodreads_mtc)
min(overlap_data_mtc$pos_neg_ratio_goodreads_mtc)
max(overlap_data_mtc$pos_neg_ratio_goodreads_mtc)

# History, historical fiction, biography
pnratio_m_amazon_hist <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'history, historical fiction, biography') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_hist %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_hist = pos_neg_ratio)

pnratio_m_goodreads_hist <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'history, historical fiction, biography') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_hist %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_hist = pos_neg_ratio)

overlap_data_hist <- temp %>% left_join(temp2, by = "t")
overlap_data_hist <- drop_na(overlap_data_hist)
overlap_data_hist <- overlap_data_hist %>% filter(t >= 156 & t <= 216)

mean(overlap_data_hist$pos_neg_ratio_amazon_hist)
min(overlap_data_hist$pos_neg_ratio_amazon_hist)
max(overlap_data_hist$pos_neg_ratio_amazon_hist)

mean(overlap_data_hist$pos_neg_ratio_goodreads_hist)
min(overlap_data_hist$pos_neg_ratio_goodreads_hist)
max(overlap_data_hist$pos_neg_ratio_goodreads_hist)

# Young-adult
pnratio_m_amazon_yadult <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'young-adult') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_yadult %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_yadult = pos_neg_ratio)

pnratio_m_goodreads_yadult <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'young-adult') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_yadult %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_yadult = pos_neg_ratio)

overlap_data_yadult <- temp %>% left_join(temp2, by = "t")
overlap_data_yadult <- drop_na(overlap_data_yadult)
overlap_data_yadult <- overlap_data_yadult %>% filter(t >= 156 & t <= 216)

mean(overlap_data_yadult$pos_neg_ratio_amazon_yadult)
min(overlap_data_yadult$pos_neg_ratio_amazon_yadult)
max(overlap_data_yadult$pos_neg_ratio_amazon_yadult)

mean(overlap_data_yadult$pos_neg_ratio_goodreads_yadult)
min(overlap_data_yadult$pos_neg_ratio_goodreads_yadult)
max(overlap_data_yadult$pos_neg_ratio_goodreads_yadult)

# Romance
pnratio_m_amazon_romance <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'romance') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_romance %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_romance = pos_neg_ratio)

pnratio_m_goodreads_romance <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'romance') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_romance %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_romance = pos_neg_ratio)

overlap_data_romance <- temp %>% left_join(temp2, by = "t")
overlap_data_romance <- drop_na(overlap_data_romance)
overlap_data_romance <- overlap_data_romance %>% filter(t >= 156 & t <= 216)

mean(overlap_data_romance$pos_neg_ratio_amazon_romance)
min(overlap_data_romance$pos_neg_ratio_amazon_romance)
max(overlap_data_romance$pos_neg_ratio_amazon_romance)

mean(overlap_data_romance$pos_neg_ratio_goodreads_romance)
min(overlap_data_romance$pos_neg_ratio_goodreads_romance)
max(overlap_data_romance$pos_neg_ratio_goodreads_romance)

# Fantasy, paranormal
pnratio_m_amazon_fanpar <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'fantasy, paranormal') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_fanpar %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_fanpar = pos_neg_ratio)

pnratio_m_goodreads_fanpar <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'fantasy, paranormal') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_fanpar %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_fanpar = pos_neg_ratio)

overlap_data_fanpar <- temp %>% left_join(temp2, by = "t")
overlap_data_fanpar <- drop_na(overlap_data_fanpar)
overlap_data_fanpar <- overlap_data_fanpar %>% filter(t >= 156 & t <= 216)

mean(overlap_data_fanpar$pos_neg_ratio_amazon_fanpar)
min(overlap_data_fanpar$pos_neg_ratio_amazon_fanpar)
max(overlap_data_fanpar$pos_neg_ratio_amazon_fanpar)

mean(overlap_data_fanpar$pos_neg_ratio_goodreads_fanpar)
min(overlap_data_fanpar$pos_neg_ratio_goodreads_fanpar)
max(overlap_data_fanpar$pos_neg_ratio_goodreads_fanpar)

# Children
pnratio_m_amazon_children <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'children') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_children %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_children = pos_neg_ratio)

pnratio_m_goodreads_children <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'children') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_children %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_children = pos_neg_ratio)

overlap_data_children <- temp %>% left_join(temp2, by = "t")
overlap_data_children <- drop_na(overlap_data_children)
overlap_data_children <- overlap_data_children %>% filter(t >= 156 & t <= 216)

mean(overlap_data_children$pos_neg_ratio_amazon_children)
min(overlap_data_children$pos_neg_ratio_amazon_children)
max(overlap_data_children$pos_neg_ratio_amazon_children)

mean(overlap_data_children$pos_neg_ratio_goodreads_children)
min(overlap_data_children$pos_neg_ratio_goodreads_children)
max(overlap_data_children$pos_neg_ratio_goodreads_children)

# Comics, graphic
pnratio_m_amazon_comgra <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'comics, graphic') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_comgra %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_comgra = pos_neg_ratio)

pnratio_m_goodreads_comgra <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'comics, graphic') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_comgra %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_comgra = pos_neg_ratio)

overlap_data_comgra <- temp %>% left_join(temp2, by = "t")
overlap_data_comgra <- drop_na(overlap_data_comgra)
overlap_data_comgra <- overlap_data_comgra %>% filter(t >= 156 & t <= 216)

mean(overlap_data_comgra$pos_neg_ratio_amazon_comgra)
min(overlap_data_comgra$pos_neg_ratio_amazon_comgra)
max(overlap_data_comgra$pos_neg_ratio_amazon_comgra)

mean(overlap_data_comgra$pos_neg_ratio_goodreads_comgra)
min(overlap_data_comgra$pos_neg_ratio_goodreads_comgra)
max(overlap_data_comgra$pos_neg_ratio_goodreads_comgra)

# Poetry
pnratio_m_amazon_poetry <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon' & dominant_genre == 'poetry') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp <- pnratio_m_amazon_poetry %>% select(t, pos_neg_ratio)
temp <- drop_na(temp)
temp <- distinct(temp)
temp <- rename(temp, pos_neg_ratio_amazon_poetry = pos_neg_ratio)

pnratio_m_goodreads_poetry <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads' & dominant_genre == 'poetry') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
temp2 <- pnratio_m_goodreads_poetry %>% select(t, pos_neg_ratio)
temp2 <- drop_na(temp2)
temp2 <- distinct(temp2)
temp2 <- rename(temp2, pos_neg_ratio_goodreads_poetry = pos_neg_ratio)

overlap_data_poetry <- temp %>% left_join(temp2, by = "t")
overlap_data_poetry <- drop_na(overlap_data_poetry)
overlap_data_poetry <- overlap_data_poetry %>% filter(t >= 156 & t <= 216)

mean(overlap_data_poetry$pos_neg_ratio_amazon_poetry)
min(overlap_data_poetry$pos_neg_ratio_amazon_poetry)
max(overlap_data_poetry$pos_neg_ratio_amazon_poetry)

mean(overlap_data_poetry$pos_neg_ratio_goodreads_poetry)
min(overlap_data_poetry$pos_neg_ratio_goodreads_poetry)
max(overlap_data_poetry$pos_neg_ratio_goodreads_poetry)



### FIGURES 6 & A3 ###
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
pos_neg_genres_30_months <- ggplot(genre_data2, aes(x = t, y = pos_neg_ratio, colour = Label)) + 
  geom_line() + 
  facet_wrap(~ dominant_genre, ncol = 2) +
  geom_vline(xintercept=186, linetype = "dashed") +
  xlab("Number of months since the first review") + 
  ylab("Positive-Negative Ratio")
pos_neg_genres_30_months
ggsave('../../gen/analysis/output/pos_neg_genres_30_months.png', pos_neg_genres_30_months)

# Make plot with remaining 6 genres (FIGURE A2)
genre_data2 <- genre_data %>% filter(t >= 156 & t <= 216)
genre_data2 <- genre_data2 %>% filter(dominant_genre == 'young-adult' | dominant_genre == 'romance' | dominant_genre == 'fantasy, paranormal' | dominant_genre == 'children' | dominant_genre == 'comics, graphic' | dominant_genre == 'poetry')
pos_neg_genres_30_months_smaller_6 <- ggplot(genre_data2, aes(x = t, y = pos_neg_ratio, colour = Label)) + 
  geom_line() + 
  facet_wrap(~ dominant_genre, ncol = 2) +
  geom_vline(xintercept=186, linetype = "dashed") +
  xlab("Number of months since the first review") + 
  ylab("Positive-Negative Ratio")
pos_neg_genres_30_months_smaller_6
ggsave('../../gen/analysis/output/pos_neg_genres_30_months_smaller_6.png', pos_neg_genres_30_months_smaller_6)

