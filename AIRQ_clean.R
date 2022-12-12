library(dplyr)
library(tidyr)
library(readr)

#a function to calculate percentile for each value in a column
perc.rank <- function(x){
  y <- rank(x[!is.na(x)])/length(x[!is.na(x)])
  z <- x
  z[!is.na(x)] <- y
  return(z)
} 

#ASSIGNING DISADVANTAGE INDEX TO EACH BAY AREA CENSUS TRACT
df<-read_csv("airq.csv")

#getting Bay Area data
df1 <- subset(df, `California County` %in% c('Contra Costa','San Francisco',
                                              'Solano','Marin','San Mateo',
                                              'Sonoma','Alameda','Napa','Santa Clara'))

#calculating the Air Pollution Index
df2 <- df1 %>%
  select(`California County`,`Approximate Location`, ZIP,`Ozone Pctl`,`PM2.5 Pctl`,`Diesel PM Pctl`,
         `Pesticides Pctl`,`Tox. Release Pctl`,`Traffic Pctl`,`Census Tract`,`Total Population`)%>%

df2 <- mutate(df2, airq_score = rowMeans(select(df2, c(4:8)), na.rm = TRUE)) %>%
       mutate(airq_index = round(
         perc.rank(airq_score), digits = 4)*100)

write.csv(df2, "graph1.csv")


#calculating the SES Index
df4 <- df1 %>%
  select(`California County`,`Approximate Location`, ZIP, Education, 
         `Linguistic Isolation`, Poverty, Unemployment, `Housing Burden`,
         `Total Population`,`Census Tract`)

for (i in 4:8){
  colname <- paste0("pctl", as.character(i))
  a  <-  as.numeric(apply(df4[i], 2, function(x) {round(perc.rank(x), digits = 4)*100}))
  df4[colname] <- a
}

df4 <- 
  mutate(df4, socio_score = rowMeans(select(df4, c(11:15)), na.rm = TRUE)) %>%
  mutate(ses_index = round(
    perc.rank(socio_score), digits = 4)*100)

write.csv(df4,"graph2.csv")

#getting the indexes in one data frame
df_scores <- data.frame(select(df2,c(1:3,10,11,13)),select(df4,c(4:8,17))) %>%
  mutate(disadv = ses_index * airq_index)%>%
  mutate(disadv_index = round(
    perc.rank(disadv), digits = 4)*100) %>%
  #finding the 95% percentile of disadvantage index
  mutate(bins = ntile(disadv_index,20)) 

write.csv(df_scores,"disadvantaged.csv")

#getting the data for the top %5 most disadvantaged communities  
df_hierarchy20 <- df_scores %>%
  subset(bins %in% c('20'))
write.csv(df_hierarchy20,"hierarchy.csv")

#number of disadvantaged people, by county
df_hie_pop <- df_hierarchy20 %>%
  group_by(California.County)%>%
  summarize(popu = sum(Total.Population))

#for hierarchy chart
df_hie_pop <- df_hierarchy20 %>%
  group_by(California.County,Approximate.Location)%>%
  summarize(popu = sum(Total.Population))
  
write.csv(df_hie_pop,"hierarchy.csv")

#finding the number of the top %5 most disadvantaged communities for pie chart
df_pie20 <- df_hierarchy20 %>%
  group_by(California.County)%>%
  summarize(n_count = n())
write.csv(df_pie20,"pie20.csv")

#finding the number of the top %5 most advantaged communities for pie chart
df_hierarchy1 <- df_scores %>%
  subset(bins %in% c('1'))
df_pie1 <- df_hierarchy1 %>%
  group_by(California.County)%>%
  summarize(n_count = n())
write.csv(df_pie1,"pie1.csv")

#RACIAL RATIO
df10<-read_csv("demographic.csv")

#Bay Area Data
df11 <- subset(df10, `California County` %in% c('Contra Costa','San Francisco',
                                             'Solano','Marin','San Mateo',
                                             'Sonoma','Alameda','Napa','Santa Clara'))

#Racial makeup, the most disadvantaged population
df12 <- data.frame(select(df11,c(1,5,6,10:15)),df_scores$disadv_index) %>%
        subset(`Census.Tract` %in% c(df_hierarchy20$Census.Tract))%>%
        mutate(Hispanic = round(Total.Population*df12$Hispanic..../100))%>%
        mutate(White = round(Total.Population*df12$White..../100))%>%
        mutate(Black = round(Total.Population*df12$African.American..../100))%>%
        mutate(NativeAmerican = round(Total.Population*df12$Native.American..../100))%>%
        mutate(Asian = round(Total.Population*df12$Asian.American..../100))%>%
        mutate(Other = round(Total.Population*df12$Other.Multiple..../100))%>%
        select(c(11:16))

#Racial makeup, the most advantaged population
df13 <- data.frame(select(df11,c(1,5,6,10:15)),df_scores$disadv_index) %>%
  subset(`Census.Tract` %in% c(df_hierarchy1$Census.Tract))%>%
  mutate(Hispanic = round(Total.Population*df13$Hispanic..../100))%>%
  mutate(White = round(Total.Population*df13$White..../100))%>%
  mutate(Black = round(Total.Population*df13$African.American..../100))%>%
  mutate(NativeAmerican = round(Total.Population*df13$Native.American..../100))%>%
  mutate(Asian = round(Total.Population*df13$Asian.American..../100))%>%
  mutate(Other = round(Total.Population*df13$Other.Multiple..../100))%>%
  select(c(11:16))

#Racial makeup, Bay Area population
df14 <- data.frame(select(df11,c(1,5,6,10:15)))%>%
  mutate(Hispanic = round(Total.Population*df14$Hispanic..../100))%>%
  mutate(White = round(Total.Population*df14$White..../100))%>%
  mutate(Black = round(Total.Population*df14$African.American..../100))%>%
  mutate(NativeAmerican = round(Total.Population*df14$Native.American..../100))%>%
  mutate(Asian = round(Total.Population*df14$Asian.American..../100))%>%
  mutate(Other = round(Total.Population*df14$Other.Multiple..../100))%>%
  select(c(10:15))

#calculating ratio
df15 <- data.frame("20"=colSums(df12),
                   "1" =colSums(df13),
                   bay_area =colSums(df14,na.rm=T))%>%
  mutate(dis_ratio = round(df15$X20/sum(df15$X20)*100,digits=4))%>%
  mutate(adv_ratio = round(df15$X1/sum(df15$X1)*100,digits=4))%>%
  mutate(bay_ratio = round(df15$bay_area/sum(df15$bay_area)*100,digits=4))

write.csv(df15,"racial_ratio.csv")
    



