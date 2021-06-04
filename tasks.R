# Load libraries
library(dplyr)
library(readr)
library(tidytext)
library(sweary)

# Load datasets
sp_lines <- read_csv("datasets/sp_lines.csv")
sp_ratings <- read_csv("datasets/sp_ratings.csv")

# Take a look at the last six observations
tail(sp_lines, 6)
tail(sp_ratings, 6)

# Load english swear words
en_swear_words <- sweary::get_swearwords("en") %>%
  mutate(stem = SnowballC::wordStem(word))

# Load the AFINN lexicon
afinn  <- read_rds("datasets/afinn.rds")

# Join lines with episode ratings
sp <- sp_lines %>% left_join(sp_ratings)

# Unnest lines to words, leave out stop words and add a 
# swear_word logical column
sp_words <- sp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  left_join(afinn) %>%
  mutate(word_stem = SnowballC::wordStem(word),
         swear_word = word %in% en_swear_words$word | word_stem %in% en_swear_words$stem)

# View the last six observations
tail(sp_words)

# Group by and summarize data by episode
by_episode <- sp_words %>%
  group_by(episode_name, rating, episode_order) %>%
  summarize(
    swear_word_ratio = sum(swear_word) / n(),
    sentiment_score = mean(value, na.rm = TRUE)
  ) %>%
  arrange(episode_order)

# Examine the last few rows of by_episode
tail(by_episode)

# What is the naughtiest episode?
( naughtiest <- by_episode[which.max(by_episode$swear_word_ratio), ] )

# Load the ggplot2
library(ggplot2)

# Set a minimal theme for all future plots
theme_set(theme_minimal())

# Plot sentiment score for each episode
ggplot(by_episode, aes(episode_order, sentiment_score)) +
  geom_col() + 
  geom_smooth(method = "lm", se = FALSE)

# Plot episode ratings
ggplot(by_episode, aes(episode_order, rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 100, col = "red", lty = "dashed")

# Plot swear word ratio over episode rating
ggplot(by_episode, aes(rating , swear_word_ratio)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(6, 10, 0.5)) +
  labs(
    x = "IMDB rating",
    y = "Episode swear word ratio"
  )

# Create a function that compares profanity of two characters
compare_profanity <- function(char1, char2, words) {
  char_1 <- filter(words, character == char1)
  char_2 <- filter(words, character == char2)
  char_1_summary <- summarise(char_1, swear = sum(swear_word), total = n() - sum(swear_word))
  char_2_summary <- summarise(char_2, swear = sum(swear_word), total = n() - sum(swear_word))
  char_both_summary <- bind_rows(char_1_summary, char_2_summary)
  result <- prop.test(as.matrix(char_both_summary), correct = FALSE)
  return(broom::tidy(result) %>% bind_cols(character = char1))
}

# Vector of most speaking characters in the show
characters <- c("butters", "cartman", "kenny", "kyle", "randy", "stan", "gerald", "mr. garrison",
                "mr. mackey", "wendy", "chef", "jimbo", "jimmy", "sharon", "sheila", "stephen")

# Map compare_profanity to all characters against Cartman
prop_result <- purrr::map_df( characters, compare_profanity, "cartman", sp_words)

# Plot estimate1-estimate2 confidence intervals of all characters and color it by a p.value threshold
ggplot(prop_result, aes(x = reorder(character, -estimate1), y = estimate1 - estimate2, color = p.value < 0.05)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Are naughty episodes more popular? TRUE/FALSE
naughty_episodes_more_popular <- FALSE

# Is Eric Cartman the naughtiest character? TRUE/FALSE
eric_cartman_naughtiest <- FALSE

# If he is, assign an empty string, otherwise write his name
who_is_naughtiest <-"kenny"

