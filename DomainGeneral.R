df <- NULL
numbers <- c('0','1', '2', '3', '4', '5', '6', '7')
for (i in c(1:52, 54:80)){
log.file <- sprintf('~/Desktop/SPL_domain/Famous People/raw_data/%s.log', i)
log <- read.table(log.file,
                  sep = '\t', flush = TRUE, header = TRUE, quote = "")


# question order: social, temporal, spatial
# Dataframe consisting of all the records of qualitative data 
  for (name in unique(log$stimulus)){
    log.name <- log %>%
      filter(stimulus == name)
  
    Q1.number <- as.character(log.name[1,4])
    Q2.number <- as.character(log.name[2,4])
    Q3.number <- as.character(log.name[3,4])
    
    if (Q1.number %in% numbers & Q2.number %in% numbers & Q3.number %in% numbers){
      row <- c(name, Q1.number, Q2.number, Q3.number)
      df <- rbind(df, row)
    }
  }
}

df <- as.data.frame(df)
colnames(df) <- c('Name', 'Q1', 'Q2', 'Q3')
df$Q1 <- as.numeric(df$Q1)
df$Q2 <- as.numeric(df$Q2)
df$Q3 <- as.numeric(df$Q3)
name <- trimws(gsub("\\d", '', df$Name))

score.Q1 <- NULL
score.Q2 <- NULL
score.Q3 <- NULL
score.Q4 <- NULL
score.Q5 <- NULL
score.Q6 <- NULL
score.Q7 <- NULL
score.Q8 <- NULL

# Getting the scores for each observation
for (name in unique(df$Name)){
  df_name <- df %>%
                filter(Name == name)
  
  # High/High/High
  score.name.Quad1 <- df_name %>%
    mutate(Score = Q1 + Q2 + Q3) %>%
    select(Name, Score)
  score.Q1 <- rbind(score.Q1, score.name.Quad1)
  
  # Low/High/High
  score.name.Quad2 <- df_name %>%
    mutate(Score = (8-Q1) + Q2 + Q3) %>%
    select(Name, Score)
  score.Q2 <- rbind(score.Q2, score.name.Quad2)
  
  # High/Low/High
  score.name.Quad3 <- df_name %>%
    mutate(Score = Q1 + (8-Q2) + Q3) %>%
    select(Name, Score)
  score.Q3 <- rbind(score.Q3, score.name.Quad3)
  
  # High/High/Low
  score.name.Quad4 <- df_name %>%
    mutate(Score = Q1 + Q2 + (8-Q3)) %>%
    select(Name, Score)
  score.Q4 <- rbind(score.Q4, score.name.Quad4)
  
  # Low/Low/High
  score.name.Quad5 <- df_name %>%
    mutate(Score = (8-Q1) + (8-Q2) + Q3) %>%
    select(Name, Score)
  score.Q5 <- rbind(score.Q5, score.name.Quad5)
  
  # Low/High/Low
  score.name.Quad6 <- df_name %>%
    mutate(Score = (8-Q1) + Q2 + (8-Q3)) %>%
    select(Name, Score)
  score.Q6 <- rbind(score.Q6, score.name.Quad6)
  
  # High/Low/Low
  score.name.Quad7 <- df_name %>%
    mutate(Score = Q1 + (8-Q2) + (8-Q3)) %>%
    select(Name, Score)
  score.Q7 <- rbind(score.Q7, score.name.Quad7)
  
  # Low/Low/Low
  score.name.Quad8 <- df_name %>%
    mutate(Score = (8-Q1) + (8-Q2) + (8-Q3)) %>%
    select(Name, Score)
  score.Q8 <- rbind(score.Q8, score.name.Quad8)
}

score.Q1.new <- NULL
score.Q2.new <- NULL
score.Q3.new <- NULL
score.Q4.new <- NULL
score.Q5.new <- NULL
score.Q6.new <- NULL
score.Q7.new <- NULL
score.Q8.new <- NULL
?as.data.table
# Finding the median of each famous person
# High/High/Low
for (name in unique(score.Q1$Name)){
  score.Q1.name <- score.Q1 %>%
                      filter(Name == name)
  frequency <- nrow(score.Q1.name)
  sd <- sqrt(var(score.Q1.name$Score))
  mean <- mean(score.Q1.name$Score)
  med <- median(score.Q1.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q1.name <- data.frame(name, med, frequency)
      score.Q1.new <- rbind(score.Q1.new, score.Q1.name)
    }
  }
}

# Low/High/High
for (name in unique(score.Q2$Name)){
  score.Q2.name <- score.Q2 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q2.name)
  sd <- sqrt(var(score.Q2.name$Score))
  mean <- mean(score.Q2.name$Score)
  med <- median(score.Q2.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q2.name <- data.frame(name, med, frequency)
      score.Q2.new <- rbind(score.Q2.new, score.Q2.name)
    }
  }
}

# High/Low/High
for (name in unique(score.Q3$Name)){
  score.Q3.name <- score.Q3 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q3.name)
  sd <- sqrt(var(score.Q3.name$Score))
  mean <- mean(score.Q3.name$Score)
  med <- median(score.Q3.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q3.name <- data.frame(name, med, frequency)
      score.Q3.new <- rbind(score.Q3.new, score.Q3.name)
    }
  }
}

# High/High/Low
for (name in unique(score.Q4$Name)){
  score.Q4.name <- score.Q4 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q4.name)
  sd <- sqrt(var(score.Q4.name$Score))
  mean <- mean(score.Q4.name$Score)
  med <- median(score.Q4.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q4.name <- data.frame(name, med, frequency)
      score.Q4.new <- rbind(score.Q4.new, score.Q4.name)
    }
  }
}

# Low/Low/High
for (name in unique(score.Q5$Name)){
  score.Q5.name <- score.Q5 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q5.name)
  sd <- sqrt(var(score.Q5.name$Score))
  mean <- mean(score.Q5.name$Score)
  med <- median(score.Q5.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q5.name <- data.frame(name, med, frequency)
      score.Q5.new <- rbind(score.Q5.new, score.Q5.name)
    }
  }
}

# Low/High/Low
for (name in unique(score.Q6$Name)){
  score.Q6.name <- score.Q6 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q6.name)
  sd <- sqrt(var(score.Q6.name$Score))
  mean <- mean(score.Q6.name$Score)
  med <- median(score.Q6.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q6.name <- data.frame(name, med, frequency)
      score.Q6.new <- rbind(score.Q6.new, score.Q6.name)
    }
  }
}

# High/Low/Low
for (name in unique(score.Q7$Name)){
  score.Q7.name <- score.Q7 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q7.name)
  sd <- sqrt(var(score.Q7.name$Score))
  mean <- mean(score.Q7.name$Score)
  med <- median(score.Q7.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q7.name <- data.frame(name, med, frequency)
      score.Q7.new <- rbind(score.Q7.new, score.Q7.name)
    }
  }
}

# Low/Low/Low
for (name in unique(score.Q8$Name)){
  score.Q8.name <- score.Q8 %>%
    filter(Name == name)
  
  frequency <- nrow(score.Q8.name)
  sd <- sqrt(var(score.Q8.name$Score))
  mean <- mean(score.Q8.name$Score)
  med <- median(score.Q8.name$Score)
  
  if (is.na(sd) == FALSE){
    if (med < (mean + sd) & med > (mean - sd)){
      score.Q8.name <- data.frame(name, med, frequency)
      score.Q8.new <- rbind(score.Q8.new, score.Q8.name)
    }
  }
}

score.Q1.new <- score.Q1.new[order(-score.Q1.new$med),]
score.Q2.new <- score.Q2.new[order(-score.Q2.new$med),]
score.Q3.new <- score.Q3.new[order(-score.Q3.new$med),]
score.Q4.new <- score.Q4.new[order(-score.Q4.new$med),]
score.Q5.new <- score.Q5.new[order(-score.Q5.new$med),]
score.Q6.new <- score.Q6.new[order(-score.Q6.new$med),]
score.Q7.new <- score.Q7.new[order(-score.Q7.new$med),]
score.Q8.new <- score.Q8.new[order(-score.Q8.new$med),]


WriteXLS(score.Q1.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q1.xls')
WriteXLS(score.Q2.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q2.xls')
WriteXLS(score.Q3.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q3.xls')
WriteXLS(score.Q4.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q4.xls')
WriteXLS(score.Q5.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q5.xls')
WriteXLS(score.Q6.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q6.xls')
WriteXLS(score.Q7.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q7.xls')
WriteXLS(score.Q8.new, '/Users/gnolasco/Downloads/DomainGeneralQuant/Q8.xls')
