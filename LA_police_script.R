#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(janitor) # to clean data

```{r load the data, message=FALSE,  warning=FALSE}
LA<-read.csv("ca_los_angeles_2020_04_01.csv")

```

=====================
  # Explore the data set
  # =====================

#look at what columns exist in our `Los Angeles` data frame. 

```{r}
colnames(LA)
```

How many stops do we have in our dataset? 

```{r Try the `nrow()` function.)}
nrow(LA)
```

What date range does our data cover?
```{r echo=FALSE}
min(LA$date,na.rm = T)
```
to
```{r echo=FALSE}
max(LA$date,na.rm = T)
```

Since we only have six months of data for 2018, let's filter it out. Note that there are ways to deal with partial years in analysis, but to make things easier for ourselves, let's focus on 2010-2017.

=====================
# Clean the data
# =====================

```{r}
LA <- filter(LA, year(date) < 2018)
```

How many rows do we have now?

```{r}
LA %>% nrow()
```
Our analyses will focus on traffic stops so lets make sure our data frame is applicable for such data

```{r}
 unique(LA$type)
```
Filter for traffic stops only
```{r}
LA <- LA %>% filter(type == "vehicular")
```
Double check the data frame (only contains traffic stops).
```{r}
 unique(LA$type)
```
View number of rows
```{r}
nrow(LA)
```

=====================
# Conduct descriptive analysis
# =====================


To find stop counts per year, we need to define a notion of year (recall that our data only has date). Use the year() and mutate() functions to add a new column called year to our LA data frame and then use count() to determine the number of stops per year.

```{r echo=FALSE}
LA %>%
  mutate(year = year(date)) %>% 
  count(year)
```

Use count() to determine the number of stops by race.

```{r echo=FALSE}
LA %>% 
  count(subject_race)
```

Let's make another table that gives us the proportion of stops by race.

```{r echo=FALSE}
LA %>% 
  count(subject_race) %>% 
  mutate(prop = n / sum(n))
```
How about counting how many stops by year and race?

```{r echo=FALSE}
LA%>% 
  count(year = year(date), subject_race)
```

Lets visualize

```{r visualize stops by year and race, echo=FALSE}
LA%>% 
  count(year = year(date), subject_race) %>% 
  ggplot(aes(x = year, y = n, color =subject_race))+
  geom_point() +
  geom_line() 
```
```{r}
df2017 <- LA %>% filter(year(date) == 2017)

```

Look for racial [demographics](https://datacommons.org/tools/timeline#place=geoId%2F0644000&statsVar=Count_Person_AmericanIndianOrAlaskaNativeAlone__Count_Person_AsianAlone__Count_Person_BlackOrAfricanAmericanAlone__Count_Person_HispanicOrLatino__Count_Person_NativeHawaiianOrOtherPacificIslanderAlone__Count_Person_SomeOtherRaceAlone__Count_Person_TwoOrMoreRaces__Count_Person_WhiteAlone) in Los Angeles (2017) i.e. number of hispanics, blacks, asian, etc. and create a data frame to establish comparison.

``` {r establish a baseline comparison}
population_2017 <- tibble(subject_race = c("asian/pacific islander","black", "hispanic", "other","white"),
  num_people = c(467248, 351971, 1922879, 1069295, 2061262)) %>%
  mutate(subject_race = as.factor(subject_race))

```
Lets see our newly formed numbers and with proportion rates.
```{r find the race proportion rates in the data set, echo=FALSE}
population_2017 %>% 
  mutate(proportion = num_people / sum(num_people))

```

Visualization

```{r echo=FALSE}
ggplot(population_2017)+ geom_col(aes(x = subject_race, y= num_people,fill=subject_race), alpha=0.5)+
  scale_y_continuous(labels=comma)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  labs(title = "Los Angeles race demographics 2017", x="Race", y="count")

```

# Stop rates
# ---------------------

Use left join () to complete benchmark test and mutate () to calculate the stop rate.  

```{r echo=FALSE}
df2017 %>% 
  count(subject_race) %>% 
  drop_na() %>% 
  left_join(population_2017,by = "subject_race") %>%
  mutate(stop_rate = n / num_people) 
```

is there a difference in the stop rate when we take into account sex?
  
  
  ```{r echo=FALSE}
df2017 %>% 
  count(subject_sex) %>% 
  mutate(prop = n / sum(n))
```
#Visualization
```{r visualize, echo=FALSE}
  ggplot(df2017)+ geom_bar(aes(x = subject_race,fill=subject_sex), alpha=0.5)+
  scale_y_continuous(labels=comma)+
  facet_wrap(~subject_sex)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90),legend.position = "none")+
  labs(title = "Demographics Los Angeles 2017", x="Race", y="count")
```

---------------------
# Vehicular stops based on region
# ---------------------

Lets find the regions with the highest number of vehicular stops. 

```{r echo=FALSE}
df2017%>% 
  count(region) %>% 
  arrange(desc(n))%>%
  head(n=30)
```

Visualize the racial demographics in the top regions
```{r echo=FALSE}
df2017 %>% 
  count(region,subject_race) %>% 
  arrange(desc(n))%>%
  head(n=7) %>% 
  ggplot(aes(x = subject_race,y=n, fill=subject_race)) +
  geom_col()+
  facet_wrap(~region)+
  theme_bw()+
  theme(panel.grid.major =element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  labs(title = "Vehicular stops by region", x="Region", y="count")
```

#Lets find the proportion for blacks in this region.

```{r echo=FALSE}
df2017 %>%
  filter(region == "METROPOLITAN DIVISN") %>%
  count(subject_race) %>% 
  mutate(proportion = n / sum(n))
```

What is the stop rate in this particular region?
  ```{r echo=FALSE}
df2017 %>%
  filter(region == "METROPOLITAN DIVISN") %>% 
  count(subject_race) %>% 
  drop_na() %>% 
  left_join(population_2017,by = "subject_race") %>%
  mutate(stop_rate = n / num_people) 
```
