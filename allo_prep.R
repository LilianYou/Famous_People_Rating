library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(heatmaply)


#############################################################################################
#########                                                                  ##################
########                 PART I. Social Distance                          ###################
#########                                                                  ##################
#############################################################################################

# read data
social_raw <- read.csv("~/Desktop/SPL_domain/Famous_People/allo_prep/social.csv", header = F)

social_df <- social_raw %>% filter(V1 == "MEAN"|V3 == "con") %>% 
  select(-V1, -V20)
social0_ind <- social_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(c(1,3,5,7,9,11,13,15,17,19))
social0_dis <- social_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))
social0_con <- social_df %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

social_sd <- social_raw %>% filter(V1 == "STDEV"|V3 == "con")%>% 
  select(-V1, -V20)
social0_dis_sd <- social_sd %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))
social0_con_sd <- social_sd %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

# extract a list of all the names as an axis
col_n <- function(r){
  r <- sapply(str_split(r,"/"), "[",1)
}

people_name0 <- social0_ind[1]
people_name <- map_chr(people_name0[,1], col_n) %>% sort()

# create a data frame with people col and row names
df <- data.frame(matrix(ncol = 10, nrow = 10))
colnames(df) <- people_name
rownames(df) <- rev(people_name)

# fill in data for different variable purposes

dis_social0 <- df
dis_social <- df

dis_social0_sd <- df
dis_social_sd <- df

con_social0 <- df
con_social <- df

con_social0_sd <- df
con_social_sd <- df

# read 1 by 1
for (i in 1:10){
    for (j in 1:9) {
      x = sapply(str_split(social0_ind[i,j],"/"), "[",1)
      y = sapply(str_split(social0_ind[i,j],"/"), "[",2)
      if (x == "washginton"){
        x = "washington"
      }
      if (y == "mandela "){
        y = "mandela"
      }
      dis_social0[x,y] = as.numeric(as.character(social0_dis[i,j]))
      dis_social0_sd[x,y] = as.numeric(as.character(social0_dis_sd[i,j]))
      con_social0[x,y] = as.numeric(as.character(social0_con[i,j]))
      con_social0_sd[x,y] = as.numeric(as.character(social0_con_sd[i,j]))
      }
  }

# average symmetric ones
for (i in 1:10){
  for (j in 1:9) {
    x = sapply(str_split(social0_ind[i,j],"/"), "[",1)
    y = sapply(str_split(social0_ind[i,j],"/"), "[",2)
    if (x == "washginton"){
      x = "washington"
    }
    if (y == "mandela "){
      y = "mandela"
    }
    dis_social[x,y] = round(mean(c(dis_social0[x,y], dis_social0[y,x])), digits = 2)
    dis_social_sd[x,y] = round(mean(c(dis_social0_sd[x,y], dis_social0_sd[y,x])), digits = 2)
    con_social[x,y] = round(mean(c(con_social0[x,y], con_social0[y,x])), digits = 2)
    con_social_sd[x,y] = round(mean(c(con_social0_sd[x,y], con_social0_sd[y,x])), digits = 2)
  }
}

write_csv(dis_social, "~/Desktop/SPL_domain/Famous_People/allo_prep/social_distance.csv")
write_csv(dis_social_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/social_distance_sd.csv")
write_csv(con_social, "~/Desktop/SPL_domain/Famous_People/allo_prep/social_confidence.csv")
write_csv(con_social_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/social_confidence_sd.csv")


#heatmaply(dis_df, dendrogram = "none")

#############################################################################################
#########                                                                  ##################
########                 PART II. Spatial Distance                          ###################
#########                                                                  ##################
#############################################################################################

# read data
spatial_raw <- read.csv("~/Desktop/SPL_domain/Famous_People/allo_prep/spatial.csv", header = F)
# fix raw data typo
spatial_raw[40,"V14"] <- 7
the_data <- spatial_raw[25:43, "V14"]
mean_44 <- mean(as.numeric(as.character(spatial_raw[25:43, "V14"]))) #5.789474
sd_45 <- sd(as.numeric(as.character(spatial_raw[25:43, "V14"]))) # 1.436777
se_46 <- sd(as.numeric(as.character(spatial_raw[25:43, "V14"])))/sqrt(length(spatial_raw[25:43, "V14"])) # 0.3296192

spatial_raw_corrected <- read.csv("~/Desktop/SPL_domain/Famous_People/allo_prep/spatial_corrected.csv", header = F)
spatial_df <- spatial_raw_corrected %>% filter(V1 == "MEAN"|V3 == "con") %>% 
  select(-V1, -V20)
spatial0_ind <- spatial_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(c(1,3,5,7,9,11,13,15,17,19))

spatial0_dis <- spatial_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))


spatial0_con <- spatial_df %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

spatial_sd <- spatial_raw_corrected %>% filter(V1 == "STDEV"|V3 == "con")%>% 
  select(-V1, -V20)
spatial0_dis_sd <- spatial_sd %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))
spatial0_con_sd <- spatial_sd %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

# fill in data for different variable purposes

dis_spatial0 <- df
dis_spatial <- df

dis_spatial0_sd <- df
dis_spatial_sd <- df

con_spatial0 <- df
con_spatial <- df

con_spatial0_sd <- df
con_spatial_sd <- df

# read 1 by 1
for (i in 1:10){
  for (j in 1:9) {
    x = sapply(str_split(spatial0_ind[i,j],"/"), "[",1)
    y = sapply(str_split(spatial0_ind[i,j],"/"), "[",2)
    if (x == "washginton"){
      x = "washington"
    }
    if (y == "mandela "){
      y = "mandela"
    }
    if (x == "ghan"){
      x = "ghandi"
    }
   dis_spatial0[x,y] = as.numeric(as.character(spatial0_dis[i,j]))
    dis_spatial0_sd[x,y] = as.numeric(as.character(spatial0_dis_sd[i,j]))
    con_spatial0[x,y] = as.numeric(as.character(spatial0_con[i,j]))
    con_spatial0_sd[x,y] = as.numeric(as.character(spatial0_con_sd[i,j]))
  }
}

# average symmetric ones
for (i in 1:10){
  for (j in 1:9) {
    x = sapply(str_split(spatial0_ind[i,j],"/"), "[",1)
    y = sapply(str_split(spatial0_ind[i,j],"/"), "[",2)
    if (x == "washginton"){
      x = "washington"
    }
    if (y == "mandela "){
      y = "mandela"
    }
    if (x == "ghan"){
      x = "ghandi"
    }
    dis_spatial[x,y] = round(mean(c(dis_spatial0[x,y], dis_spatial0[y,x])), digits = 2)
    dis_spatial_sd[x,y] = round(mean(c(dis_spatial0_sd[x,y], dis_spatial0_sd[y,x])), digits = 2)
    con_spatial[x,y] = round(mean(c(con_spatial0[x,y], con_spatial0[y,x])), digits = 2)
    con_spatial_sd[x,y] = round(mean(c(con_spatial0_sd[x,y], con_spatial0_sd[y,x])), digits = 2)
  }
}

write_csv(dis_spatial, "~/Desktop/SPL_domain/Famous_People/allo_prep/spatial_distance.csv")
write_csv(dis_spatial_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/spatial_distance_sd.csv")
write_csv(con_spatial, "~/Desktop/SPL_domain/Famous_People/allo_prep/spatial_confidence.csv")
write_csv(con_spatial_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/spatial_confidence_sd.csv")

#############################################################################################
#########                                                                  ##################
########                 PART III. Temporal Distance                       ###################
#########                                                                  ##################
#############################################################################################

# read data
temporal_raw <- read.csv("~/Desktop/SPL_domain/Famous_People/allo_prep/temporal.csv", header = F)

temporal_df <- temporal_raw %>% filter(V1 == "MEAN"|V3 == "con") %>% 
  select(-V1, -V20)
temporal0_ind <- temporal_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(c(1,3,5,7,9,11,13,15,17,19))
temporal0_dis <- temporal_df %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))
temporal0_con <- temporal_df %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

temporal_sd <- temporal_raw %>% filter(V1 == "STDEV"|V3 == "con")%>% 
  select(-V1, -V20)
temporal0_dis_sd <- temporal_sd %>% select(-c(V3,V5,V7,V9,V11,V13,V15,V17, V19)) %>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))
temporal0_con_sd <- temporal_sd %>% select(c(V3,V5,V7,V9,V11,V13,V15,V17, V19))%>% 
  slice(-c(1,3,5,7,9,11,13,15,17,19))

# fill in data for different variable purposes

dis_temporal0 <- df
dis_temporal <- df

dis_temporal0_sd <- df
dis_temporal_sd <- df

con_temporal0 <- df
con_temporal <- df

con_temporal0_sd <- df
con_temporal_sd <- df

# read 1 by 1
for (i in 1:10){
  for (j in 1:9) {
    x = sapply(str_split(temporal0_ind[i,j],"/"), "[",1)
    y = sapply(str_split(temporal0_ind[i,j],"/"), "[",2)
    if (x == "washginton"){
      x = "washington"
    }
    if (y == "mandela "){
      y = "mandela"
    }
    if (x == "ghan"){
      x = "ghandi"
    }
    dis_temporal0[x,y] = as.numeric(as.character(temporal0_dis[i,j]))
    dis_temporal0_sd[x,y] = as.numeric(as.character(temporal0_dis_sd[i,j]))
    con_temporal0[x,y] = as.numeric(as.character(temporal0_con[i,j]))
    con_temporal0_sd[x,y] = as.numeric(as.character(temporal0_con_sd[i,j]))
  }
}

# average symmetric ones
for (i in 1:10){
  for (j in 1:9) {
    x = sapply(str_split(temporal0_ind[i,j],"/"), "[",1)
    y = sapply(str_split(temporal0_ind[i,j],"/"), "[",2)
    if (x == "washginton"){
      x = "washington"
    }
    if (y == "mandela "){
      y = "mandela"
    }
    if (x == "ghan"){
      x = "ghandi"
    }
    dis_temporal[x,y] = round(mean(c(dis_temporal0[x,y], dis_temporal0[y,x])), digits = 2)
    dis_temporal_sd[x,y] = round(mean(c(dis_temporal0_sd[x,y], dis_temporal0_sd[y,x])), digits = 2)
    con_temporal[x,y] = round(mean(c(con_temporal0[x,y], con_temporal0[y,x])), digits = 2)
    con_temporal_sd[x,y] = round(mean(c(con_temporal0_sd[x,y], con_temporal0_sd[y,x])), digits = 2)
  }
}

write_csv(dis_temporal, "~/Desktop/SPL_domain/Famous_People/allo_prep/temporal_distance.csv")
write_csv(dis_temporal_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/temporal_distance_sd.csv")
write_csv(con_temporal, "~/Desktop/SPL_domain/Famous_People/allo_prep/temporal_confidence.csv")
write_csv(con_temporal_sd, "~/Desktop/SPL_domain/Famous_People/allo_prep/temporal_confidence_sd.csv")
