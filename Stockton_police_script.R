#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(janitor) # to clean data

#load original .csv files
stockton<-read.csv("ca_stockton_2020_04_01.csv")

```{r load the data, message=FALSE,  warning=FALSE}
stockton<-read.csv("ca_stockton_2020_04_01.csv")
```

First, lets take a look at what columns exist in our `stockton` data frame. 

```{r}
colnames(stockton)
```

How many stops do we have in our dataset? 

```{r Try the `nrow()` function.)}
nrow(stockton)
```

What date range does our data cover?
```{r echo=FALSE}
min(stockton$date)
```
to
```{r echo=FALSE}
max(stockton$date)
```

=====================
#  Clean the data
# =====================

Our analyses is only relevant for traffic stops so lets make sure our data frame is applicable for such data
```{r}
 unique(stockton$type)
```
=====================
#  Conduct descriptive analysis
# =====================

To find stop counts per year, we need to define a notion of year (recall that our data only has date). Use the year() and mutate() functions to add a new column called year to our stops data frame and then use count() to determine the number of stops per year.

```{r echo=FALSE}
stockton %>%
  mutate(year = year(date)) %>% 
  count(year)
```

Use count() to determine the number of stops by race
```{r echo=FALSE}
stockton %>% 
  count(subject_race) %>% 
  drop_na()
```

Let's make another table that gives us the proportion of stops by race.

```{r echo=FALSE}
stockton %>% 
  count(subject_race) %>% 
  drop_na() %>% 
  mutate(prop = n / sum(n))
```

How about counting how many stops by year and race?

```{r echo=FALSE}
stockton %>% 
  count(year = year(date), subject_race) %>% 
  drop_na()
```

Lets visualize

```{r visualize stops by year and race, echo=FALSE}
stockton%>% 
  count(year = year(date), subject_race) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = n, color =subject_race))+
  geom_point() +
  geom_line() 
```

Filter out the data. 

```{r}
df2015 <- stockton %>% filter(year(date) == 2015)
```
Look for racial [demographics](https://datacommons.org/tools/timeline#&place=geoId/0675000&statsVar=Count_Person_AmericanIndianOrAlaskaNativeAlone__Count_Person_AsianAlone__Count_Person_BlackOrAfricanAmericanAlone__Count_Person_HispanicOrLatino__Count_Person_NativeHawaiianOrOtherPacificIslanderAlone__Count_Person_SomeOtherRaceAlone__Count_Person_TwoOrMoreRaces__Count_Person_WhiteAlone) in Stockton (2015) i.e. number of hispanics, blacks, asian, etc. and create a data frame to establish comparison.

``` {r establish a baseline comparison}
population_2015 <- tibble(subject_race = c("asian/pacific islander","black", "hispanic", "other","white"),
  num_people = c(4272, 34772, 126048, 39432, 132471)) %>%
  mutate(subject_race = as.factor(subject_race))
```

```{r find the race proportion rates in the data set, echo=FALSE}
population_2015 %>% 
  mutate(proportion = num_people / sum(num_people))
```

Visualization

```{r echo=FALSE}
ggplot(population_2015)+ geom_col(aes(x = subject_race, y= num_people,fill=subject_race), alpha=0.5)+
  scale_y_continuous(labels=comma)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  labs(title = "Stockton race demographics 2015", x="Race", y="count")
```

---------------------
# Stop rates
# ------------------

```{r Use left join to complete benchmark test}
df2015 %>% 
  count(subject_race) %>% 
  drop_na() %>% 
  left_join(population_2015,by = "subject_race") %>%
  mutate(stop_rate = n / num_people)
```

```{r echo=FALSE}
df2015 %>% 
  count(subject_sex) %>% 
  drop_na() %>% 
  mutate(prop = n / sum(n))
```

```{r visualize, echo=FALSE}
df2015%>%
  group_by(subject_race,subject_sex,arrest_made) %>% 
  drop_na(subject_race,subject_sex,arrest_made) %>% 
  ggplot(aes(x = arrest_made, fill=arrest_made), alpha=0.5)+
  geom_bar()+
  scale_y_continuous(labels=comma)+
  facet_grid(subject_sex~subject_race)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90),legend.position = "none")+
  labs(title = "Arrest made by race and sex, Stockton 2015", x="Arrest made", y="count")
```

  ```{r echo=FALSE}
df2015%>% 
  drop_na(subject_race) %>% 
  ggplot(aes(x = outcome, fill=outcome), alpha=0.5)+
  geom_bar()+
  scale_y_continuous(labels=comma)+
  facet_wrap(~subject_race)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(),axis.text.x=element_blank())+
  labs(title = "Outcome of interaction by race, Stockton 2015", x="Outcome", y="count")
```

---------------------
  # Search rates
  # ------------------

```{r echo=FALSE}
df2015 %>% 
  group_by(subject_race) %>%
  drop_na(subject_race) %>% 
  summarize(search_rate = mean(search_conducted,na.rm = T),arrest_rate = mean(arrest_made, na.rm = T))

```
