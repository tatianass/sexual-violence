library(dplyr)


#########################################
#### Union of Different Year ############
#########################################


save_df <- function(df_result, name){
  file_name = paste(paste("../data/", name, sep=""), "_union.csv", sep="")
  write.table(df_result, file = file_name, sep = ";", row.names = F, quote = T)
}

union <- function(name){
  #loading data
  f1_name <- paste(paste("../data/", name, sep=""), "_2003_2008.csv", sep="")
  f2_name <- paste(paste("../data/", name, sep=""), "_2009_2010.csv", sep="")
  df1 <- read.csv(f1_name, sep = ";", header = T, stringsAsFactors = F)
  df2 <- read.csv(f2_name, sep = ";", header = T, stringsAsFactors = F)
  
  #getting col that is not in the other dataframe
  if(name == ch_sexual_violence_against_children){
    df_match <- df1 %>%
      select(country_territory, match_definition, child_definition)
  }else{
    df_match <- df1 %>%
      select(country_territory, match_definition)
  }
  
  
  df2_join <- left_join(df2, df_match, by = c('country_territory'))
  
  df2 <- distinct(df2_join)
  
  df = rbind(df1, df2)
  
  #change NA with -1
  df[is.na(df)] <- -1
  
  #saving
  save_df(df, name)
  
}

ch_rape <- "rape"
ch_sexual_violence <- "sexual_violence"
ch_sexual_violence_against_children <- "sexual_violence_against_children"

databases <- c(ch_rape, ch_sexual_violence, ch_sexual_violence_against_children)

for (i in databases) {
  union(i)
}