library(tidyverse)
library(bigchess)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chess = as_tibble(read.pgn("/Users/louisteitelbaum/Downloads/lichess_Kippah_2022-03-10.pgn",
                           add.tags = c("WhiteElo", "BlackElo", "WhiteRatingDiff", "BlackRatingDiff", "Termination", "UTCTime"),
                           extract.moves = 2,
                           last.move = F)) %>% select(!c(Site, Round, 20:38))

me_first = read_csv("/Users/louisteitelbaum/Downloads/mussar_october_november.csv") %>% select(c(date, bedtime, wakeup, prenoon_coffeecups, postnoon_coffeecups, 41, 42))

me = read_csv("/Users/louisteitelbaum/Downloads/mussar_december_march.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Wrangling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename and Standardize Variables
names(me) = c("timestamp", "date", "today", "weekday", "shabbossocial", "bedtime", "wakeup", "shaharit", "nap", "coffeecups", "sedarim_missed", "sedarim_madeup", "anxiety", "happiness", "notes")
me = me %>% select(date, bedtime, wakeup, coffeecups, anxiety, happiness, sleeptime)

      # Sanity check bedtimes
      me = me %>% mutate(bedtime = if_else(as.integer(str_remove_all(str_sub(me$bedtime, 1, 2), ":")) >= 12L, 
                                      paste(str_sub(me$bedtime, 1, -3), "AM", sep = ""), 
                                      bedtime))
      me = me %>% mutate(bedtime = if_else(as.integer(str_remove_all(str_sub(me$bedtime, 1, 2), ":")) < 12L, 
                                      paste(str_sub(me$bedtime, 1, -3), "PM", sep = ""), 
                                      bedtime))      

me = me %>% mutate(date = as.Date(date, "%m/%d/%Y")) %>%
            mutate(bedtime = if_else(str_sub(bedtime, -2, -1) == "PM", ymd_hms(paste(date-1, bedtime, sep = " ")), ymd_hms(paste(date, bedtime, sep = " "))),
                   wakeup = ymd_hms(paste(date, wakeup, sep = " "))) %>%
            mutate(sleeptime = wakeup-bedtime)


names(me_first)[6:7] = c("stress_subjective", "mood_subjective")
      # Sanity check bedtimes
      me_first = me_first %>% 
        mutate(bedtime = if_else(as.integer(str_remove_all(str_sub(bedtime, 1, 2), ":")) < 12L,
                                 paste(bedtime, "PM", sep = ""),
                                 bedtime))    
      me_first = me_first %>% 
        mutate(bedtime = if_else(as.integer(str_remove_all(str_sub(bedtime, 1, 2), ":")) >= 12L | as.integer(str_remove_all(str_sub(bedtime, 1, 2), ":")) <= 3L,
                                 paste(bedtime, "AM", sep = ""),
                                 bedtime))
  
me_first = me_first %>% 
  mutate(bedtime = if_else(str_sub(bedtime, -2, -1) == "PM", ymd_hm(paste(ymd(date)-1, bedtime, sep = " ")), ymd_hm(paste(date, bedtime, sep = " "))),
         wakeup = ymd_hm(paste(date, wakeup, "AM", sep = " "))) %>%
  mutate(sleeptime = wakeup-bedtime,
         coffeecups = prenoon_coffeecups + postnoon_coffeecups,
         anxiety = (stress_subjective/5)*4 - 1,
         happiness = (mood_subjective/5)*4 - 1,
         date = as.Date(date))

names(chess) = c("event", "date", "white", "black", "result", "white_elo", "black_elo", "white_ratingdiff", "black_ratingdiff", "termination", "utctime", "movetext", "nmoves", "w1", "b1", "w2", "b2")

chess = chess %>% mutate(date = as.Date(date, "%Y.%m.%d"),
                         white_ratingdiff = as.numeric(white_ratingdiff),
                         black_ratingdiff = as.numeric(black_ratingdiff))
# Calculate Variables
chess = chess %>%
  mutate(utctime = hms::as_hms(utctime),
         israeltime = hms::as_hms(hms::as_hms(utctime) + 60*60*2),
         my_color = if_else(white == "Kippah", "white", "black"),
         my_result = (substr(result, 1, 2) == "1-")*(white == "Kippah") + (substr(result, 2, 3) == "-1")*(black == "Kippah") + (substr(result, 1, 3) == "1/2")*0.5,
         my_win = if_else(white == "Kippah", if_else(substr(result, 1, 2) == "1-", T, F), if_else(substr(result, 2, 3) == "-1", T, F)),
         my_loss = if_else(white == "Kippah", if_else(substr(result, 2, 3) == "-1", T, F), if_else(substr(result, 1, 2) == "1-", T, F)),
         my_elo = if_else(white == "Kippah", white_elo, black_elo),
         my_ratingdiff = if_else(white == "Kippah", white_ratingdiff, black_ratingdiff),
         my_firstmove = if_else(white == "Kippah", as.character(w1), as.character(b1))) %>%
  mutate(my_lostontime = termination == "Time forfeit" & my_loss,
         my_wonontime = termination == "Time forfeit" & my_win)

# Big dataset
byday = me %>%
  bind_rows(me_first) %>%
  right_join(chess) %>%
  group_by(date, weekday, coffeecups, anxiety, happiness, sleeptime) %>%
  summarise(ngames = n(),
            wins = sum(my_result),
            my_ratingdiff_pergame = sum(my_ratingdiff)/n(),
            lostontime = sum(my_lostontime)/sum(my_loss),
            wonontime = sum(my_wonontime)/sum(my_win),
            sleeptime = max(as.numeric(sleeptime)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Models and Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(flexplot)

# How much chess do I play at different hours of the day?
chess %>%
  filter(date > "2021-09-14") %>%
  mutate(hour = hour(israeltime)) %>%
  filter(hour>1) %>%
  group_by(hour) %>%
  summarise(ngames = n()) %>%
  ggplot(aes(hour, ngames)) +
    geom_point() +
    geom_smooth()

# How does my chess performance vary by time of day?
chess %>%
  filter(date > "2021-09-14") %>%
  mutate(hour = hour(israeltime)) %>%
  filter(hour>1) %>%
  summarise(n = n())

chess %>%
  filter(date > "2021-09-14") %>%
  mutate(hour = hour(israeltime)) %>%
  filter(hour>1) %>%
  group_by(hour) %>%
  summarise(ngames = n(),
            my_ratingdiff_pergame = sum(my_ratingdiff)/n()) %>%
  ggplot(aes(hour, my_ratingdiff_pergame, weight = ngames)) +
    geom_point(aes(size = ngames)) +
    geom_smooth() +
    theme_minimal() +
    scale_x_continuous(breaks = c(4:23),
                       labels = c("4:00 AM", "", "", 
                                  "7:00 AM", "", "", 
                                  "10:00 AM", "", "", 
                                  "1:00 PM", "", "",
                                  "4:00 PM", "", "",
                                  "7:00 PM", "", "",
                                  "10:00 PM", "")) +
    scale_size(name = "N Games") +
    labs(title = "Average Chess Performance by Hour of the Day",
         subtitle = "N = 3,280 Games",
         caption = "Note: Loess curve is weighted by number of games.",
         x = "",
         y = "Average change in rating per game")

# How do my affect and anxiety affect the amount of chess I play?
byday %>%
  ggplot(aes(happiness, ngames)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(y = "Chess games played that day", x = "I feel cheerful",
         title = "I Play Less Chess When I'm Feeling Happy",
         caption = str_wrap("Note: From October 6 to November 21 I used a five-item cheerfulness scale. These values were rescaled to match the current four-item scale.", width = 74))

byday %>%
  ggplot(aes(anxiety, ngames)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Chess games played that day", x = "I feel tense or 'wound up'",
       title = "I Play More Chess When I'm Feeling Anxious",
       caption = str_wrap("Note: From October 6 to November 21 I used a five-item anxiety scale. These values were rescaled to match the current four-item scale.", width = 70))

# How does the amount of sleep I get at night affect my chess performance?
mean(as.numeric(c(me$sleeptime, me_first$sleeptime)), na.rm = T) #I get an average of 7.53 hours of sleep a night.

byday %>%
  ggplot(aes(sleeptime, my_ratingdiff_pergame, weight = ngames)) +
  geom_point(aes(size = ngames)) +
  geom_smooth() +
  labs(x = "Hours slept the previous night", y = "Average change in rating per chess game",
       title = "A Good Night's Sleep is Good for Chess Performance",
       caption = "Note: Loess curve is weighted by number of games.") +
  theme_minimal() +
  ylim(-7, 5)

# How do hour/affect/anxiety/sleep affect the way in which I lose/win my games?
  # hour
chess %>%
  filter(date > "2021-09-14") %>%
  mutate(hour = hour(israeltime)) %>%
  filter(hour > 4) %>%
  group_by(hour) %>%
  summarise(ngames = n(),
            my_lostontime = sum(my_lostontime)/sum(my_loss),
            my_wonontime = sum(my_wonontime)/sum(my_win)) %>%
  pivot_longer(c("my_lostontime", "my_wonontime"), names_to = "winlose", values_to = "ontime") %>%
  ggplot(aes(hour, 100*ontime, color = winlose)) +
    geom_point(aes(size = ngames)) +
    geom_smooth(se = F) +
    theme_minimal() +
    scale_x_continuous(breaks = c(4:23),
                       labels = c("4:00 AM", "", "", 
                                  "7:00 AM", "", "", 
                                  "10:00 AM", "", "", 
                                  "1:00 PM", "", "",
                                  "4:00 PM", "", "",
                                  "7:00 PM", "", "",
                                  "10:00 PM", "")) +
    scale_size(name = "N Games") +
    scale_color_discrete(name = NULL, labels = c("Lost by Timeout", "Won by Timeout")) +
    labs(title = "How I Win or Lose Chess Games by Hour of the Day",
         subtitle = "N = 3,280 Games",
         x = "",
         y = "Percent")

  # affect
byday %>%
  pivot_longer(c("lostontime", "wonontime"), names_to = "winlose", values_to = "ontime") %>%
  ggplot(aes(happiness, 100*ontime, color = winlose, weight = ngames)) +
  geom_point(aes(size = ngames)) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "I feel cheerful", y = "Percent", 
       title = "Affect and Game Outcome by Day",
       caption = "Note: Loess curve is weighted by number of games.") +
  theme_minimal() +
  scale_color_discrete(name = NULL, labels = c("Lost by Timeout", "Won by Timeout"))

  # anxiety
byday %>%
  pivot_longer(c("lostontime", "wonontime"), names_to = "winlose", values_to = "ontime") %>%
  ggplot(aes(anxiety, 100*ontime, color = winlose, weight = ngames)) +
  geom_point(aes(size = ngames)) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Anxiety and Game Outcome by Day", x = "I feel tense or 'wound up'", y = "Percent",
       caption = "Note: Loess curve is weighted by number of games.") +
  theme_minimal() +
  scale_color_discrete(name = NULL, labels = c("Lost by Timeout", "Won by Timeout"))

  # sleep
byday %>%
  pivot_longer(c("lostontime", "wonontime"), names_to = "winlose", values_to = "ontime") %>%
  ggplot(aes(sleeptime, 100*ontime, color = winlose, weight = ngames)) +
    geom_point(aes(size = ngames)) +
    geom_smooth(se = F) +
    labs(x = "Hours slept the previous night", y = "Percent",
         title = "Sleep and Game Outcome by Day",
         caption = "Note: Loess curve is weighted by number of games.") +
    theme_minimal() +
    scale_color_discrete(name = NULL, labels = c("Lost by Timeout", "Won by Timeout"))

# Formal modeling
byday_robust = byday %>% filter(ngames >= 20)

happinessmod1 <- lm(my_ratingdiff_pergame ~ happiness, byday_robust, weights = ngames)
  summary(happinessmod1)
happinessmod2 <- lm(my_ratingdiff_pergame ~ happiness + ngames, byday_robust, weights = ngames)
summary(happinessmod2)
compare.fits(my_ratingdiff_pergame ~ happiness | ngames, data = byday_robust, happinessmod1, happinessmod2)
model.comparison(happinessmod1, happinessmod2)

anxietymod1 <- lm(my_ratingdiff_pergame ~ anxiety, byday, weights = ngames)
  summary(anxietymod1)
anxietymod2 <- lm(my_ratingdiff_pergame ~ anxiety + ngames, byday_robust, weights = ngames)
  summary(anxietymod2)
compare.fits(my_ratingdiff_pergame ~ anxiety | ngames, data = byday_robust, anxietymod1, anxietymod2)
model.comparison(anxietymod1, anxietymod2)
  
sleepmod1 <- lm(my_ratingdiff_pergame ~ sleeptime, byday_robust, weights = ngames)
  summary(sleepmod1)
sleepmod2 <- lm(my_ratingdiff_pergame ~ sleeptime + ngames, byday_robust, weights = ngames)
  summary(sleepmod2)
compare.fits(my_ratingdiff_pergame ~ sleeptime | ngames, data = byday_robust, sleepmod1, sleepmod2)
model.comparison(sleepmod1, sleepmod2)
