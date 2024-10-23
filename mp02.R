library(dplyr)
library(tidyverse)
##importing data
NAME_BASICS <- read.csv("name_basics_small.csv")
glimpse(NAME_BASICS)

TITLE_BASICS <- read.csv("title_basics_small.csv")
glimpse(TITLE_BASICS)

TITLE_EPISODES <- read.csv("title_episodes_small.csv")
glimpse(TITLE_EPISODES)

TITLE_RATINGS <- read.csv("title_ratings_small.csv")
glimpse(TITLE_RATINGS)

TITLE_CREW <- read.csv("title_crew_small.csv")
glimpse(TITLE_CREW)

TITLE_PRINCIPALS <- read.csv("title_principals_small.csv")
glimpse(TITLE_PRINCIPALS)

#Data sub-sampling
NAME_BASICS <- NAME_BASICS |> 
  filter(str_count(knownForTitles, ",") > 1)

#Graph freq of number of ratings
TITLE_RATINGS |>
  ggplot(aes(x=numVotes)) + 
  geom_histogram(bins=30) +
  xlab("Number of IMDB Ratings") + 
  ylab("Number of Titles") + 
  ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
  theme_bw() + 
  scale_x_log10(label=scales::comma) + 
  scale_y_continuous(label=scales::comma)

#drop 75% of data 
TITLE_RATINGS |>
  pull(numVotes) |>
  quantile()

#more filtering
TITLE_EPISODES <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_EPISODES_1 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))
TITLE_EPISODES_2 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(parentTconst == tconst))

TITLE_EPISODES <- bind_rows(TITLE_EPISODES_1,
                            TITLE_EPISODES_2) |>
  distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  semi_join(TITLE_RATINGS, join_by(tconst == tconst))


rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)

#Initial Exploration - changing str to dbl
NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric(birthYear),
         deathYear = as.numeric(deathYear))

##Task 2 
#q1
t2q1 <- TITLE_BASICS |>
  filter(titleType =="movie" | titleType =="tvEpisode" | titleType =="tvSeries") |>
  group_by(titleType) |> 
  summarise(unique_count = n_distinct(tconst))
print(t2q1)

#q2 - assuming 115 is the oldest. 
t2q2 <- NAME_BASICS |>
  select(primaryName, birthYear,deathYear) |>
  mutate(age = 2024 - birthYear) |>
  filter(is.na(deathYear)) |>
  filter(age<115) |>
  arrange(desc(age)) |>
  slice(1)

print(t2q2)

#q3 

t2q3 <- TITLE_RATINGS |>
  filter(averageRating==10) |>
  filter(numVotes>=200000) |>
  pull(tconst)

t2q3_ep <- TITLE_BASICS |>
  filter(tconst==t2q3) |>
  pull(originalTitle)

t2q3_series <- TITLE_EPISODES |>
  filter(tconst==t2q3) |>
  pull(parentTconst)

t2q3_seriesName <- TITLE_BASICS |>
  filter(tconst==t2q3_series) |>
pull(originalTitle)

print(paste("The episode name is" , t2q3_ep, ", which is part of the" , t2q3_seriesName, "series."))

#q4 - combined titles to names
t2q4 <- NAME_BASICS |>
  separate_longer_delim(knownForTitles, ",") |>
  rename("tconst" = knownForTitles) |>
  left_join(TITLE_BASICS, by = "tconst") |>
  filter(primaryName=="Mark Hamill") |>
  select(primaryTitle)

print(t2q4)

#q5
t2q5 <- TITLE_EPISODES |>
  group_by(parentTconst) |>  
  summarise(No_episodes = n_distinct(tconst), .groups = 'drop') |>
  filter(No_episodes>12) |>
  rename("tconst" = parentTconst) |>
  left_join(TITLE_RATINGS, by = "tconst") |>
  left_join(TITLE_BASICS, by = "tconst") |>
  arrange(desc(averageRating)) |>
  select(primaryTitle, No_episodes, averageRating,numVotes) |>
  slice(1)

print(t2q5)

#q6
library(ggplot2)
library(RColorBrewer)

t2q6 <- TITLE_BASICS |>
  filter(primaryTitle=="Happy Days" & startYear==1974) |>
  pull(tconst)
  
t2q6_eps <- TITLE_EPISODES |>
  filter(parentTconst == t2q6) |>
  left_join(TITLE_RATINGS, by = "tconst") |>
  left_join(TITLE_BASICS, by = "tconst") |>
  mutate(season = as.numeric(seasonNumber)) |>
  mutate(yearAir = as.numeric(startYear)) |>
  mutate(episodeNum = as.numeric(episodeNumber)) |>
  arrange(season, episodeNum) |>
  select(-originalTitle, -isAdult, -titleType,-runtimeMinutes, -genres, -endYear, -seasonNumber,-episodeNumber )

print(t2q6_eps)

#szn graphs


scatter_eps <- ggplot(t2q6_eps, aes(x = season, y = averageRating)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "purple") +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_fill_brewer(palette = "Set1")+
  labs(title = "Average Ratings by Episode and Season", 
       x = "Season", 
       y = "Average Rating",
       color="season") 
print(scatter_eps)

#szn table
t2q6_szn <- t2q6_eps |>
  group_by(season) |>
  summarize(AverageSznRating = mean(averageRating), year_aired = min(startYear)) |>
  mutate(year_air = as.numeric(year_aired)) |>
  arrange(season)
print(t2q6_szn)
print("We see that the show had an overall decline in popularity. After 1977 or season 5, the show had a more rapid decline. Season 8 (year 1981) saw a bit of an increase from the previous year but still declines in poplarity overall ")


##Task 3

#q1

max_votes <- TITLE_RATINGS |> 
  arrange(desc(numVotes)) |>
  slice(1) |>
  pull(numVotes)
  
print(max_votes)

#defining large number of votes as above 75% quartile which is 970
quartiles <- quantile(TITLE_RATINGS$numVotes, probs = c(0.25, 0.5, 0.75))
print(quartiles)

#confirmed these 5 are box hits  


hits_top5 <- TITLE_RATINGS |> 
  #filter(numVotes>970) |>
  mutate(rank = (50 *(averageRating/10))+(50*(numVotes/max_votes))) |>
  left_join(TITLE_BASICS, by = "tconst") |>
  select(primaryTitle, rank,genres,tconst, titleType,averageRating, numVotes) |>
  filter(titleType == "movie") |>
  arrange(desc(rank)) |>
  slice(1:5)
  
print(hits_top5)

#q2
#confirmed these 5 are NOT box hits  
hits_low <- TITLE_RATINGS |> 
  filter(numVotes>970) |>
  mutate(rank = (50 *(averageRating/10))+(50*(numVotes/max_votes))) |>
  left_join(TITLE_BASICS, by = "tconst") |>
  select(primaryTitle, rank,genres,tconst, titleType,averageRating, numVotes)  |>
  filter(titleType == "movie") |>
  arrange(rank) |>
  slice(1:5)

print(hits_low)
  
#q3 picking morgan freeman
hits <- TITLE_RATINGS |> 
  #filter(numVotes>970) |>
  mutate(rank = (50 *(averageRating/10))+(50*(numVotes/max_votes))) |>
  left_join(TITLE_BASICS, by = "tconst") |>
  select(primaryTitle, rank,genres,tconst, titleType,averageRating, numVotes) |>
  filter(titleType == "movie") |>
  arrange(desc(rank))

morg <- NAME_BASICS |>
  filter(primaryName == "Morgan Freeman") |>
  slice(1:2) 

print(morg)

# Nconst = nm0000151 or nm0293532

morg_movie <- TITLE_PRINCIPALS |>
  filter(nconst == "nm0000151" | nconst == "nm0293532") |>
  left_join(hits, by = "tconst") |>
  arrange(desc(rank)) |>
  slice(1:10) |>
  select(primaryTitle, rank, genres,tconst, titleType,averageRating, numVotes)
#confirm they have multiple projects with high scores
print(morg_movie)


#q4 - spot check for Brad Pitt
brad <- NAME_BASICS |>
  filter(primaryName == "Brad Pitt") 

print(brad)

# Nconst = nm0000093

brad_movie <- TITLE_PRINCIPALS |>
  filter(nconst == "nm0000093") |>
  left_join(hits, by = "tconst") |>
  arrange(desc(rank)) |>
  slice(1:10) |>
  select(primaryTitle, rank, genres,tconst, titleType,averageRating, numVotes)

#confirm they have multiple projects with high scores
print(brad_movie)

#q5

quartiles_hits <- quantile(hits$rank, probs = c(0.25, 0.5, 0.75))
print(quartiles_hits)


freq <- ggplot(hits, aes(x = rank)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Histogram of Values",
       x = "Values",
       y = "Frequency") +
  theme_minimal()
print(freq)

print("anything above 36 can be considered as a successful Movie. The mode is observed at 35, which indicates it is somewhat difficult to get a rank higher than the 3rd quartile")

##Task 4

gen <- TITLE_BASICS |>
  separate_longer_delim(genres, ",") |>
  filter(titleType=="movie") |>
  mutate(theme = genres) |>
  select (-genres, -originalTitle)
print(gen)

hits_gen <- hits |>
  filter(rank>36) |>
  left_join(gen, by = "tconst") |>
  group_by(theme) |>
  summarize(ranking = mean(rank, ra.rm = TRUE) , count_movie=n_distinct(tconst)) |>
  filter(count_movie>15) |>
  arrange(desc(ranking)) |>
  slice(1:10)
  

print(hits_gen)

#graph 1 - avg score and density
ggplot(hits_gen, aes(x = theme, y = ranking, fill = count_movie)) +  # Fill by genre for different colors
  geom_bar(stat = "identity") +  # Use the count values directly
  labs(title = "Avg Score per Top 10 Genre Among Successful Movies",
       x = "Genre",
       y = "Avg Score", 
       fill= "Movie Count") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#graph 2 df
hits_gen1 <- hits |>
  filter(rank>36) |>
  left_join(gen, by = "tconst") |>
  filter(theme=="Action" | theme=="Adventure" | theme=="Animation"  |theme=="Fantasy" | theme=="Horror" | theme=="Mystery" |theme=="Sci-Fi" |theme=="Thriller"|theme=="War")

print(hits_gen1)
#graph 2 
ggplot(hits_gen1, aes(x = theme, y = rank)) +  
  geom_boxplot() +  # Box plot geometry
  labs(title = "Box Plot of Distribution of Top 10 Ranked Genre",
       x = " ",
       y = "Ranking") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print("it appears that the Sci-Fi genre has a slight edge in success among the top 10 categories")

#graph 2 df alternative
hits_gen1alt <- hits |>
  #filter(rank>36) |>
  left_join(gen, by = "tconst") |>
  filter(theme=="Action" | theme=="Adventure" | theme=="Animation"  |theme=="Fantasy" | theme=="Horror" | theme=="Mystery" |theme=="Sci-Fi" |theme=="Thriller"|theme=="War")

print(hits_gen1alt)
#graph 2 alt
ggplot(hits_gen1alt, aes(x = theme, y = rank)) +  
  geom_boxplot() +  
  labs(title = "Box Plot of Distribution of Top 10 Ranked Genre inclusive of bad movies",
       x = " ",
       y = "Ranking") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print("if you include bad movies, you see that Animation and War movies have a betetr chance of success.")


#graph 3 df - 2010 data
hits_gen2010 <- hits |>
  filter(rank>36) |> 
  left_join(gen, by = "tconst") |>
  filter(startYear=="2010") |>
  filter(theme=="Action" | theme=="Adventure" | theme=="Animation"  |theme=="Fantasy" | theme=="Horror" | theme=="Mystery" |theme=="Sci-Fi" |theme=="Thriller"|theme=="War")

print(hits_gen2010)
#graph 3
ggplot(hits_gen2010, aes(x = theme, y = rank)) +  
  geom_boxplot() +  
  labs(title = "Box Plot of Distribution of Top 10 Ranked Genre in 2010",
       x = " ",
       y = "Ranking") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print("In 2010, we see that Sci-Fi remains as the favorite for success")

#graph 4 df - more recent data
hits_genRecent <- hits |>
  left_join(gen, by = "tconst") |>
  filter(rank>36) |>
  mutate(year=as.numeric(startYear)) |>
  arrange(desc(year)) |>
  filter(year == "2010" | year=="2011"|year=="2012" | year=="2013" | year=="2014" | year=="2015"|year=="2016"|year=="2017"|year=="2018"|year=="2019"|year=="2020" |year=="2021"|year=="2022"|year=="2023"|year=="2024" ) |>
  filter(theme=="Action" | theme=="Adventure" | theme=="Animation"  |theme=="Fantasy" | theme=="Horror" | theme=="Mystery" |theme=="Sci-Fi" |theme=="Thriller"|theme=="War")

print(hits_genRecent)
#graph 4
ggplot(hits_genRecent, aes(x = year, y = rank)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ theme) +
  labs(title = "Recent Ranking Trends by Top 10 Genres",
       x = " ",
       y = "Ranking") +
  theme_minimal()


print("This graph leads me to think that the next movie should be a Horror or War movie since there hasn't been a good outlier in a while.")

##Task 5 pick a director & 2 actors

#which director has the highest rated horror movie
dirHorror <- hits_gen1 |>
  filter(theme=="Horror") |>
  arrange(desc(rank))

print(dirHorror)
print("Since cultural reference are important to my criteria, I do not want to pick an old movie director. The movie Get out was among the top 5 rated and the most recent. I think I found my director")

#who is the director tconst = tt5052448
dirName <- TITLE_PRINCIPALS |>
  filter(tconst == "tt5052448") 
#fouund the director nconst = nm1443502
dirName1 <- NAME_BASICS |>
  filter(nconst=="nm1443502") |>
  pull(primaryName)

print(paste("My director is" , dirName1))

#looking for relevant actors
actHorror1 <- hits |>
  filter(rank>36) |>
  left_join(gen, by = "tconst") |>
  arrange(desc(rank)) |>
  filter(theme == "Comedy") |>
  slice(1:10)

print("I value comedy in Horror movies. Personally, I like humor will let your guard go down and makes the viewer more vulnerable for jump scares.")
print("Ranking the top 10 comedies, I notice Wolf of Wall Street, I remember enjoying the humor in this movie.")
#who is the actor tconst = tt0993846
actN1 <- TITLE_PRINCIPALS |>
  filter(tconst == "tt0993846") 

#who is the actress nconst = nm3053338
actName1 <- NAME_BASICS |>
  filter(nconst=="nm3053338") |>
  pull(primaryName)
print(paste("My first actress will be" , actName1))

#actor2
actHorror2 <- hits |>
  filter(rank>36) |>
  left_join(gen, by = "tconst") |>
  arrange(desc(rank)) |>
  filter(theme == "Thriller") 


print("We need someone to carry a serious tone to juxtapose the whimsicle actress. I will look into the best thrillers")
print("Ranking the top 10 thrillers, I decide to pick The Joker.")
#who is the actor tconst = tt7286456
actN2 <- TITLE_PRINCIPALS |>
  filter(tconst == "tt7286456") 

#who is the actress nconst = nm0001618
actName2 <- NAME_BASICS |>
  filter(nconst=="nm0001618") |>
  pull(primaryName)
print(paste("My second actor will be" , actName2))


#Task 6 - Silence of the Lambs tconst = tt0102926
staff <- NAME_BASICS |> 
  separate_longer_delim(knownForTitles, ",") |>
  filter(knownForTitles == "tt0102926") |>
  #separate_longer_delim(primaryProfession, ",") |>
  mutate(yearbirth = as.numeric(birthYear)) |> 
  mutate(age = 2024 - yearbirth) |>
  filter(age<=65)
print("I would reach out to anybody in the original cast who is 65 or youner to be respectful of the retirement age. The table below shows the possible staff member:")
print(staff)
