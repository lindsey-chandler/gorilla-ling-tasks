#### Gorilla Data Analysis
#### LexTALE-Spanish

## What is the purpose of this script? 
  # For the Spanish version of the LexTALE which can be programmed as a lexical-decision task in Gorilla, this script clean the resulting CSV file, isolates only relevant columns, and calculates the LexTALE score for Spanish. Participants' scores are given one of four categories: Hit, miss, correct rejection, and false alarm. The counts of responses in these categories yield the final score. 

## What to note before starting analysis: 
  # If you have run distinct version of this task and wish to bind them into one data frame, the randomiser column must be removed or changed. 

## Required packages for analysis. Load these first. If not, install them.
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)

### Load & Clean Data
df <- read.csv(file.choose()) 
  # Create df. file.choose() function will open a pop-up window to select the specific CSV that you want. You can also replace file.choose() with a quoted "" file path, if you are working on an R project, or shared directory. 
df <- rbind(df, read.csv(file.choose())) 
  # Repeat the rbind() process as many times as necessary. Remember that all columns (order and name) need to be identical to bind successfully.

## Dataframe Cleanup
  # First, select all columns necessary for analysis. 
df <- df_clean %>%
  mutate(Trial.Number = as.numeric(as.character(Trial.Number))) %>%
  filter(display=="Trials" & Screen.Name == "Screen 1") %>%
      # Extract the display name for each Trial, and for the screen name where the decision was made. 
  select(Participant.Public.ID, Participant.Private.ID, Task.Name,
         Spreadsheet.Name, Trial.Number, Reaction.Time, Timed.Out, Response,
         Correct, ANSWER, Word)
      # Select all columns which contain the information that you need to complete the analysis, such as reaction time data, decision data, the lexical item itself, participant information, etc. 

  # Second, drop all levels which have been removed from the resulting data frame.
df_clean <- droplevels(df_clean)

## Remove timed-out trials from the cleaned dataframe
df_clean <- filter(df_clean, is.na(Timed.Out)) %>% # No NAs!
  select(-Timed.Out)

## Review resulting Reaction Time distributions
ggplot(df_clean, aes(x=Reaction.Time))+geom_histogram
  # Can add +facet_wrap(~Participant.Public.ID) to see individual distributions. 
  # To review number of trials per participant: 
  table(df_clean$Participant.Public.ID)
  View(df_clean) 
  
  
## Remove short Response Times
  # The number determination depends on the individual. 200-250 milliseconds is typical as a short RT threshold because within this window, linguistic form processing is unlikely to take place. 
df_clean <- filter(df_clean, Reaction.Time > 200)

### Get response type counts
  # The classification system is as follows: 
    # Hit (hit) = the word is real, and the participant says it is a word.
    # Miss (miss) = the word is real, and the participant rejects it. 
    # False Alarm (fa) = The word is fake, and the participant says it is a word. 
    # Correct rejection (cr) = The word is fake, and the participant rejects it.
  # Note: This code uses if-else statements to create the classification based on the response type coded by you, and the Correct coding, determined by you. The columns in your df which are important are the columns which indicate whether the word is real, and how the participant responded to it. 

df_clean <- df_clean %>%
  mutate(respType = "hit", 
         respType = ifelse(ANSWER == "Sí" & Correct == 0, "miss", respType), 
         respType = ifelse(ANSWER == "No" & Correct == 1, "cr", respType), 
         respType = ifelse(ANSWER == "No" & Correct == 0, "fa", respType))
View(df_clean)

  # Create a new df which counts all of the hit, miss, cr, and fa counts per participant. 
df_scores <- df_clean %>%
  group_by(Participant.Public.ID, respType) %>% # Participant ID column, respType from previous if-else code
  summarise(count = n()) %>% # Tally up scores for respType
  spread(respType, count) 

  # If there are no counts in a category, they may turn up as NA, but we need numbers! Here are examples of transforming resulting NA: 
df_scores[is.na(df_scores$fa),]$fa <- 0 
df_scores[is.na(df_scores$miss),]$miss <- 0

### Derive LexTALE score from hit, miss, cr, fa counts
  # The scoring equiation for this version of the LexTALE is: HIT rate - 2*FA rate
df_scores$lexTALEscore <- df_scores$hit - 2*df_scores$fa

### Examine Accuracy of Participants
  # Note: Did anyone randomly hit the same key/response most of the time? Check high hit and fa rates. 

xtabs(~Correct+ANSWER + Participant.Public.ID, df_clean) 

hist(df_scores$lexTALEscore)

ggplot(df_scores, aes(x = hit/(hit+miss), y - fa/(fa+cr))) +
  geom_point()+
  geom_abline(aes(intercept = 0, slope = 1)) + 
  geom_hline(aes(yintercept = 0.5)) + 
  geom_vline(aes(xintercept = 0.5))

### BONUS: If you need to filter participants by LexTALE score threshold:
df_scores_keep <- df_scores %>%
  mutate(include = "0", 
         include = ifelse(lexTALEscore >= -14 & lexTALEscore <= 19, "1", include)) # Use if-else statement to write 0 (Exclude) outside of a certain threshold, 1 to include the participant's scores and advance them through the filder. 

View(df_scores_keep)

### Write LexTALE scores to CSV: 
View(df_scores) # Double check that all the scores fall within normal range for LexTALE and that no scores look abnormal or miscalculated. 
write.csv(df_scores, file="your_file_path.csv", row.names=TRUE)
  