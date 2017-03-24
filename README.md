# stat695data [![Travis-CI Build Status](https://travis-ci.org/schloerke/stat695data.svg?branch=master)](https://travis-ci.org/schloerke/stat695data)


## Installation

You can install `stat695data` from github with:


``` r
drat::addRepo("schloerke")
install.packages("stat695data")
```

## Example

Load data:

``` r
# load data
data(mojo, package = "stat695data")
data(ncaa, package = "stat695data")
```

``` r

# full trelliscopejs exmaple
library(magrittr)
library(tidyr)
library(rbokeh)
library(dplyr)
library(ggplot2)

library(stat695data)
library(trelliscopejs)

# load data
data(ncaa, package = "stat695data")

# inspect data
tibble::glimpse(ncaa)

# look at purdue over time
ncaa %>%
  filter(team == "Purdue Boilermakers") %>%
  qplot(year, avg_minutes, data = .) +
    geom_line(aes(group = player))

# includes mean cognostics for every numeric column
ncaa %>%
  # filter(team %in% c("Purdue Boilermakers", "Kansas Jayhawks", "Iowa State Cyclones")) %>%
  ggplot(data = ., mapping = aes(year, avg_minutes)) +
    geom_hline(aes(yintercept = 15), color = "red", size = 2) +
    geom_line(aes(group = player)) +
    geom_point(size = 1) +
    xlim(2002, 2017) + ylim(0, 40) +
    facet_trelliscope(~ team + conference, nrow = 2, ncol = 4, path = "_ggplot_avg_mins")


# full control example
  # avg_minutes_mean, count, avg_points_mean are created as cognostics
  # can hover on points to find out player information
ncaa %>%
  # filter(team %in% c("Purdue Boilermakers", "Kansas Jayhawks", "Iowa State Cyclones")) %>%
  group_by(team, conference) %>%
  summarise(
    avg_minutes_mean = cog(mean(avg_minutes), desc = "average, mean player time"),
    count = cog(length(avg_minutes), desc = "number of players"),
    avg_points_mean = cog(mean(avg_points), desc = "average, mean number of points"),
    panel = panel(
      figure(
        xlab = "year", ylab = "average minutes",
        xlim = c(2002, 2017), ylim = c(0, 40)
      ) %>%
        ly_lines(year, avg_minutes, group = player, width = 2, alpha = 0.25) %>%
        ly_points(
          year, avg_minutes,
          size = 4,
          hover = data_frame(
            team = team,
            year = year,
            player = player,
            "average minutes" = avg_minutes
          )
        )
      )
  ) %>%
  trelliscope(
    name = "Average Playing Time per Player over Time",
    nrow = 2, ncol = 4,
    path = "_rbokeh_player_min"
  )

```

## Pokemon
``` r
# http://ryanhafen.com/blog/pokemon

library(readr)
# read the data (making "_id" columns strings)
pok <-
  read_csv("https://raw.githubusercontent.com/hafen/pokRdex/master/pokRdex_mod.csv") %>%
  mutate_at(vars(matches("_id$")), as.character)

# take a look
glimpse(pok)

# make trelliscopejs viewer
pok %>%
  mutate(panel = img_panel(url_image)) %>%
  trelliscope("pokemon", nrow = 3, ncol = 6,
    state = list(labels = c("pokemon", "pokedex")))
```


## gapminder

``` r
library(gapminder)

tibble::glimpse(gapminder)

# look per continent
qplot(year, lifeExp, data = gapminder, geom = "line", group = country) +
  facet_wrap(~ continent)

# split to each country
qplot(year, lifeExp, data = gapminder, geom = "line", group = country) +
  facet_trelliscope(~ country, nrow = 2, ncol = 4, path = "_gggapminder")

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# look at each country individually
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, country_model)
  ) %>%
  mutate(
    model_info = purrr::map(model, broom::glance)
  ) %>%
  unnest(model_info) %>%
  select(country, continent, data, r.squared) %>%
  mutate(
    panel = map_plot(data,
      ~ figure(ylim = c(10, 95), toolbar = NULL) %>%
          ly_lines(year, lifeExp, data = .x) %>%
          ly_points(year, lifeExp, hover = .x, data = .x) %>%
          theme_axis("x", major_label_orientation = 45)
    )
  ) %>%
  trelliscope(name = "rbokeh_gapminder", nrow = 2, ncol = 4)

```
