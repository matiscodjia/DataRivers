data <- read.csv("../Data/BTC-EUR.csv")

#View data
head(data)
#First 6 rows
tail(data)
# Last 6 rows
View(data)
#Sheet popup of data (view like excel)
data[20,"Open"]
#Indexing (first dimension-> row, second one -> column)
data$High[5]
#Accessing some column

require ("tidyverse")
# Start analysis
data |> 
  select(Date,Open,Close) |>
  filter(Close > Open)

require(readxl)
titanic <- read_excel("../Data/titanic.xls")
head(titanic)
starwars |> 
  select(gender, mass,height,species) |>
  filter(species == "Human") |>
  na.omit() |>
  mutate(height = height /100) |>
  mutate(BMI = mass / height^2) |>
  group_by(gender) |>
  summarise(Average_BMI = mean(BMI))
str(titanic)
# Data structure
# Rename column
starwars
sw = starwars
sw |>
  select(name,height,mass,gender) |>
  rename(weight = mass) |>
  na.omit() |>
  mutate(height = height / 100) |>
  filter(gender %in% c("masculine", "feminine")) |>
  mutate(gender = recode(gender, masculine = "m", feminine = "f"))
  
