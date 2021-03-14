library(tidyverse)

df <- NULL
numbers <- c('1', '2', '3', '4', '5', '6', '7')

#folder <- "~/Desktop/SPL_domain/Famous_People/raw_data_log"      # path to folder that holds multiple .log files
#file_list <- list.files(path=folder, pattern="*.log") # create list of all .csv files in folder

for (i in c(1:14, 16:39, 41:52, 54:80,151,401)){

  log.file <- sprintf('~/Desktop/SPL_domain/Famous_People/raw_data_log/%s.log', i)
  log <- read.table(log.file,
                    sep = '\t', flush = TRUE, header = TRUE, quote = "")
  
  
  # question order: social, temporal, spatial
  # Dataframe consisting of all the records of qualitative data 
  for (name in unique(log$stimulus)){
    log.name <- log %>%
      filter(stimulus == name)
    
    Q1.number <- as.character(log.name[1,4])
    Q1.number
    Q2.number <- as.character(log.name[2,4])
    Q2.number
    Q3.number <- as.character(log.name[3,4])
    Q3.number
    
    if (Q1.number %in% numbers & Q2.number %in% numbers & Q3.number %in% numbers){
      row <- c(name, Q1.number, Q2.number, Q3.number)
      row
      df <- rbind(df, row)
      df
    }
  }
}

df <- as.data.frame(df)
colnames(df) <- c("Name", "Q1", "Q2", "Q3")
df$Q1 <- as.numeric(df$Q1)
df$Q2 <- as.numeric(df$Q2)
df$Q3 <- as.numeric(df$Q3)
name <- trimws(gsub("\\d", '', df$Name))

score.Q1 <- NULL
score.Q2 <- NULL
score.Q3 <- NULL


# Getting the scores for each observation
for (name in unique(df$Name)){
  df_name <- df %>%
    filter(Name == name)
  
  # Social
  score.name.Quad1 <- df_name %>%
    mutate(Score = Q1 ) %>%
    select(Name, Score)
  score.Q1 <- rbind(score.Q1, score.name.Quad1)
  
  # Temporal
  score.name.Quad2 <- df_name %>%
    mutate(Score =  Q2 ) %>%
    select(Name, Score)
  score.Q2 <- rbind(score.Q2, score.name.Quad2)
  
  # Spatial
  score.name.Quad3 <- df_name %>%
    mutate(Score = Q3 ) %>%
    select(Name, Score)
  score.Q3 <- rbind(score.Q3, score.name.Quad3)
  


score.Q1.new <- NULL
score.Q2.new <- NULL
score.Q3.new <- NULL

?as.data.table
# Finding the median of each famous person
# Social
for (name in unique(score.Q1$Name)){
  score.Q1.name <- score.Q1 %>%
    filter(Name == name)
  frequency <- nrow(score.Q1.name)
  sd <- sqrt(var(score.Q1.name$Score))
  mean <- mean(score.Q1.name$Score)
  med <- median(score.Q1.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q1.name <- data.frame(name, med, mean, frequency)
      score.Q1.new <- rbind(score.Q1.new, score.Q1.name)
    }
  }
}

# Temporal
for (name in unique(score.Q2$Name)){
  score.Q2.name <- score.Q2 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q2.name)
  sd <- sqrt(var(score.Q2.name$Score))
  mean <- mean(score.Q2.name$Score)
  med <- median(score.Q2.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q2.name <- data.frame(name, med, mean, frequency)
      score.Q2.new <- rbind(score.Q2.new, score.Q2.name)
    }
  }
}

# Spatial
for (name in unique(score.Q3$Name)){
  score.Q3.name <- score.Q3 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q3.name)
  sd <- sqrt(var(score.Q3.name$Score))
  mean <- mean(score.Q3.name$Score)
  med <- median(score.Q3.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q3.name <- data.frame(name, med, mean, frequency)
      score.Q3.new <- rbind(score.Q3.new, score.Q3.name)
    }
  }
}


score.Q1.new <- score.Q1.new[order(-score.Q1.new$mean),]
score.Q2.new <- score.Q2.new[order(-score.Q2.new$mean),]
score.Q3.new <- score.Q3.new[order(-score.Q3.new$mean),]
}

write.csv(score.Q1.new, '~/Desktop/SPL_domain/Famous_People/DomainGeneralQuant/Social.csv')
write.csv(score.Q2.new, '~/Desktop/SPL_domain/Famous_People/DomainGeneralQuant/Temporal.csv')
write.csv(score.Q3.new, '~/Desktop/SPL_domain/Famous_People/DomainGeneralQuant/Spatial.csv')
