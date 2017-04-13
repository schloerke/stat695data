
library(datadr)
library(dplyr)
library(ggplot2)


rhcontrol <- function(job.reduces = 250, timeout = 0) {
  rhipeControl(mapred = list(
    "mapreduce.map.memory.mb" ="5000", # map physical memory
    "mapreduce.map.java.opts" ="-Xmx4000m", # map java memory
    "mapreduce.reduce.memory.mb" ="5000", # reduce physical memory
    "mapreduce.reduce.java.opts" ="-Xmx4000m", # reduce java memory
    "mapreduce.job.reduces" = job.reduces, # reducer has 250 tasks
    "mapreduce.task.timeout" = timeout # disable timeout
  ))
}

dt <- ddf(hdfsConn("/user/bschloe/taxi_data2"))
dt[[1]]

######################################
## Explore amount of money per trip
######################################

count_by_dollar <- function(x) {
  y <- subset(x, x$total_amount > 0 & x$total_amount < 300)
  y$total_amount_floor <- floor(y$total_amount)
  y %>% group_by_("total_amount_floor") %>% count()
}
dollar_counts <- drLapply(
  dt,
  count_by_dollar,
  output = hdfsConn("/user/bschloe/ex1/dollar_counts", autoYes = TRUE),
  control = rhcontrol(750)
)
dollar_counts[[1]]


dollar_by_amount <- divide(
  dollar_counts,
  "total_amount_floor",
  spill = 100000,
  output = hdfsConn("/user/bschloe/ex1/dollar_by_amount", autoYes = TRUE),
  control = rhcontrol(30)
)
dollar_by_amount[[1]]

get_sum <- function(x) {
  sum(x$n)
}
ans <- drLapply(dollar_by_amount, get_sum, combine = combRbind)
ans <- ans %>% arrange(total_amount_floor)
saveRDS(ans, "dollar_by_amount.rds")

ans <- readRDS("dollar_by_amount.rds")

qplot(total_amount_floor, val, data = ans, geom = "line") +
  scale_y_log10(
    breaks = 10^(2:6),
    labels = c("100", "1k", "10k", "100k", "1M")
  )




######################################
## Explore amount of money per trip with number of passengers
######################################

add_total_amount_floor <- function(x) {
  x$total_amount_floor <- floor(x$total_amount)
  x
}
get_p_money_counts <- function(x) {
  y <- subset(x, x$total_amount > 0 & x$total_amount < 300)
  y %>%
    group_by_("total_amount_floor", "passenger_count") %>%
    count()
}

dt_with_extra <- addTransform(dt, add_total_amount_floor)
dt_money_counts <- addTransform(dt_with_extra, get_p_money_counts)

p_dollar_by_amount <- divide(
  dt_money_counts,
  c("total_amount_floor", "passenger_count"),
  output = hdfsConn("/user/bschloe/ex3/counts", autoYes = TRUE),
  control = rhcontrol(300)
)
p_dollar_by_amount[[1]]

get_sum <- function(x) {
  sum(x$n)
}
ans <- drLapply(p_dollar_by_amount, get_sum, combine = combRbind)
ans <- ans %>% arrange(total_amount_floor)
saveRDS(ans, "p_dollar_by_amount.rds")
ans <- readRDS("p_dollar_by_amount.rds")

qplot(total_amount_floor, val, data = ans, geom = "line", color = as.factor(passenger_count)) +
  scale_y_log10(
    breaks = 10^(1:6),
    labels = c("10", "100", "1k", "10k", "100k", "1M")
  ) +
  labs(
    color = "Passenger Count",
    x = "Trip cost ($)",
    y = "count",
    title = "Counts of trip cost split by passenger count"
  )
