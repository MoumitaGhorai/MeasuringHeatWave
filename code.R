#import temperature file
column_names_temp <- colnames(df_temp)

#only keep the variables needed
column_names_temp_1 <- column_names_temp[c(6:3657)]

#check to see the format
column_names_temp_1[1]

#restructuring the column names to something more useful
column_names_temp_2 <- gsub('^(.{5}).', '\\2', str_replace_all(column_names_temp_1, substr(column_names_temp_1, start = 17, stop = 27), ""))

#check if correctly working 
column_names_temp_2[1]

#making the column names look nicer
column_names_temp_3 <- paste0("join_X_", sub("(^.{4})({2,3}.)(.*)", "\\3\\2\\1", column_names_temp_2) )

#check
column_names_temp_3[1]

#adding other variables to the vector of names
column_names_temp_4 <- c("STATE", "district", "long", "lat",  column_names_temp_3)

#replacing the oldnames with the newnames
for(i in 1:3657) names(df_temp)[names(df_temp) == column_names_temp[i]] = column_names_temp_4[i]

#calculating the heat days (number of days the temperature was more than 90)

#fetching necessary packages
library(dplyr)
library(tidyr)
library(stringr)

#removing unecessary columns
df_temp_new <- df_temp[-c(1,3,4,5, 3658)]
df_temp$district <- as.character(df_temp$district)
class(df_temp$district)


#calculating number fo heat days >90
df_temp_new %>% 
  pivot_longer(
    -district, 
    names_to = c("day", "year"), 
    names_pattern = "(^.{12})(.{5})", 
    values_to = "temperature"
  ) %>% 
  group_by(year, district) %>% 
  summarise(n = sum(temperature >= 90)) %>% 
  pivot_wider(names_from = "year", values_from = "n")

