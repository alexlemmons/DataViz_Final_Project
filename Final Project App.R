## Read in Datasets
combine_df <- read.csv("data/combine.csv")

b <- read.csv("data/combine data.csv")

c <- read.csv("data/combine2000_2021.csv")

## Replace NULL and 0 to NA

df = combine
  
  
df[df == "NULL"] <- NA

df[df == 0] <- NA
View(df)

library(stringr)
df_QB <- df %>%
  filter(str_detect(`Camp Jersey Number`, 'QB')) %>%
  print()

df_QB <- df_QB %>%
  select(-(`Left Turn` : `Swim Rush Right`))

df_QB <- df_QB %>%
  select(-(`Snap Time #1` : `Directional Left Placement Out`))

df_QB <- df_QB %>%
  select(-(`DraftGrade` : `CrossCheckGrade`))

view(df_QB)