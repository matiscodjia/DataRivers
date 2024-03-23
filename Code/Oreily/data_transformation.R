library(nycflights13)
library(tidyverse)
nycflights13::flights

flights |>
  filter(month == 1 & day == 1)

flights |> 
  arrange(desc(dep_delay))

flights |>
  filter(dep_delay >= 60 & arr_time <= dep_time + dep_delay - 30)

flights|>
  filter(dest == "IAH" | dest == "HOU") 
?arrange
flights |> 
  arrange(desc(dep_delay))
