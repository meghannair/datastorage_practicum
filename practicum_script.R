library("readxl")
library("tidyverse")

#Setting the filename and reading the data. Since the file has 2 sheets, reading them seperately

filename <- '/Users/meghannair/Documents/MastersCourseWork/Semester 2/practicum/nfl_elo_latest.xlsx'
sheet1 <- read_excel(filename, sheet = "Sheet 1")
sheet2 <- read_excel(filename, sheet = "Sheet1")

#Appending the data together and saving in one dataframe
df <- rbind(sheet1, sheet2)

#Removing playoff column since it is all nulls
df <- subset(df, select = -c(playoff) )

#There are some columns with nulls stored as string 'NA'. Replacing them to NAs.
df[df == "NA"] <- NA

#Printing the number of NAs per column
df %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

#Printing all column names
print(colnames(df))

#Replacing all null elo values 1500 which is the default value

df$elo1_pre <- ifelse(is.na(df$elo1_pre), 1500, df$elo1_pre)
df$elo2_pre <- ifelse(is.na(df$elo2_pre), 1500, df$elo2_pre)
df$elo1_post <- ifelse(is.na(df$elo1_post), 1500, df$elo1_post)
df$elo2_post <- ifelse(is.na(df$elo2_post), 1500, df$elo2_post)

#Since we have only few rows with null values, I am removing them for the analysis
df <- na.omit(df)


#Some of the numerical columns are saved as characters, so converting them to numeric
thecols <- c("elo1_pre","elo2_pre","elo_prob1","elo_prob2","elo1_post","elo2_post","qbelo1_pre","qbelo2_pre"  ,  
             "qb1_value_pre","qb2_value_pre","qb1_adj","qb2_adj","qbelo_prob1"   ,
             "qbelo_prob2","qb1_game_value","qb2_game_value","qb1_value_post","qb2_value_post","qbelo1_post",
             "qbelo2_post"  ,"score1", "score2")
for (i in thecols){
  df[[i]] <- as.numeric(df[[i]])
}


#Creating a boxplot for all numerical columns
df_long <- tidyr::gather(subset(df, select = thecols ), key = "variable", value = "value")
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#From the chart we can determine that for the columns 'elo1_pre', 'elo2_pre', 'elo_prob2', 'elo2_post', we have true outliers
#Printing the outlier values
for (i in c("elo1_pre","elo2_pre","elo_prob1","elo_prob2","elo1_post","elo2_post")){
  print(i)
  print(boxplot.stats(df[[i]])$out)
}


#For elo values with outliers, we can replace them default value of 1500. For probability values, we can take 1-probability of the other team

df[df$elo1_pre == 1, "elo1_pre"] <- 1500
df[df$elo2_pre == 8000, "elo2_pre"] <- 1500

x3 <- df %>% filter(elo_prob2 == 4000)
df[df$elo_prob2 == 4000, "elo_prob2"] <- 1-x3$elo_prob1

df[df$elo2_post == -1504, "elo2_post"] <- 1500
df[df$elo1_post ==  1, "elo1_post"] <- 1500

#Boxplot after dealing with outlier data
df_long <- tidyr::gather(subset(df, select = thecols ), key = "variable", value = "value")
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Frequency distribution for elo_prob1
ggplot(df, aes(x=elo1_post)) + geom_histogram()

#Frequency distribution for elo_prob2
ggplot(df, aes(x=elo2_post)) + geom_histogram()

cleaned_filename <- '/Users/meghannair/Documents/MastersCourseWork/Semester 2/practicum/nfl_elo_cleaned.csv'
write.csv(df, cleaned_filename)
