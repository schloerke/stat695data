library(datadr)
library(readr)
library(stringr)

dir.create("taxi")
setwd("~/taxi")

read_data <- function(year, month) {
  taxi_url <- str_c(
    "https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_", year, "-",
    str_pad(month, 2, side = "left", pad = "0"),
    ".csv"
  )
  parse_data(txt)
}

parse_data <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  ret <- suppressWarnings(readr::read_csv(
    txt,
    progress = FALSE,
    col_names = c(
      "VendorID",
      "tpep_pickup_datetime",
      "tpep_dropoff_datetime",
      "passenger_count",
      "trip_distance",
      "pickup_longitude",
      "pickup_latitude",
      "RatecodeID",
      "store_and_fwd_flag",
      "dropoff_longitude",
      "dropoff_latitude",
      "payment_type",
      "fare_amount",
      "extra",
      "mta_tax",
      "tip_amount",
      "tolls_amount",
      "improvement_surcharge",
      "total_amount"
    ),
    col_types = readr::cols(
      VendorID = readr::col_integer(),
      tpep_pickup_datetime = readr::col_datetime(format = ""),
      tpep_dropoff_datetime = readr::col_datetime(format = ""),
      passenger_count = readr::col_integer(),
      trip_distance = readr::col_double(),
      pickup_longitude = readr::col_double(),
      pickup_latitude = readr::col_double(),
      RatecodeID = readr::col_integer(),
      store_and_fwd_flag = readr::col_character(),
      dropoff_longitude = readr::col_double(),
      dropoff_latitude = readr::col_double(),
      payment_type = readr::col_integer(),
      fare_amount = readr::col_double(),
      extra = readr::col_double(),
      mta_tax = readr::col_double(),
      tip_amount = readr::col_double(),
      tolls_amount = readr::col_double(),
      improvement_surcharge = readr::col_double(),
      total_amount = readr::col_double()
    )
  ))
  ret
}

# h_conn <- hdfsConn("/user/bschloe/taxi_raw", auto = TRUE)
#
# for (year in 2014:2016) {
#   for (month in 1:12) {
#     month_data <- read_data(year, month)
#     addData(
#       h_conn,
#       list(
#         list(
#           c(year, month),
#           month_data
#         )
#       )
#     )
#   }
# }
#
# taxi_ddf <- ddf(h_conn)


# system("rm yell*")

# rhdel("/user/bschloe/taxi_raw")
# rhmkdir("/user/bschloe/taxi_raw")

rhdel("/user/bschloe/taxi_data")

rhdel("/user/bschloe/taxi_data2")

output <- hdfsConn("/user/bschloe/taxi_data2", autoYes = TRUE)




for (year in 2016:2016) {
  for (month in 1:12) {
    if (year == 2016 & month > 6) next
    # cur_files <- rhls("/user/bschloe/taxi_raw/")
    save_name <- str_c(year, "-", month)
    # if (length(cur_files$file) > 0) {
    #   if (str_c(save_name, "_1") %in% basename(cur_files$file)) {
    #     next
    #   }
    # }
    save_file <- str_c(
      "yellow_tripdata_", year, "-",
      str_pad(month, 2, side = "left", pad = "0"),
      ".csv"
    )
    system(str_c("wget -nc https://s3.amazonaws.com/nyc-tlc/trip+data/", save_file))
    readTextFileByChunk(
      input = save_file,
      output = output,
      linesPerBlock = 100000,
      fn = parse_data
      # fn = identity
    )
    # system(str_c("tail -n +2 ", save_file, " > tmpdata"))
    # system("split -b 200m -d tmpdata tmpdata_")
    # system("ls -lth")
    # tmp_files <- dir(pattern = "tmpdata_")
    # for (i in seq_along(tmp_files)) {
    #   tmp_file <- tmp_files[i]
    #   print(tmp_file)
    #   # rhput(normalizePath(tmp_file), str_c("/user/bschloe/taxi_raw/", save_name, "_", i))
    #   readTextFileByChunk(
    #     input = tmp_file,
    #     output = output,
    #     linesPerBlock = 100000,
    #     fn = parse_data
    #     # fn = identity
    #   )
    # }
    # system("rm tmpdata*")
  }
  # system(str_c("rm yellow_tripdata_", year, "*"))
}


hdfs_control <- rhipeControl(mapred = list(
  "mapreduce.reduce.memory.mb" ="5000",
  "mapreduce.map.memory.mb" ="5000",
  "mapreduce.map.java.opts" ="-Xmx4000m",
  "mapreduce.reduce.java.opts" ="-Xmx4000m"
))
dt <- ddf(output)
dt
#
# Distributed data frame backed by 'kvHDFS' connection
#
#  attribute      | value
# ----------------+--------------------------------------------------------------------------------------------------------------------------------------------
#  names          | VendorID(int), tpep_pickup_datetime(POS), tpep_dropoff_datetime(POS), passenger_count(POS), trip_distance(POS), and 14 more
#  nrow           | [empty] call updateAttributes(dat) to get this value
#  size (stored)  | 222.18 MB
#  size (object)  | [empty] call updateAttributes(dat) to get this value
#  # subsets      | [empty] call updateAttributes(dat) to get this value
#
# * Missing attributes: keys, splitSizeDistn, splitRowDistn, summary
#
#
dt <- updateAttributes(dt, control = hdfs_control)





#
# ewrap <- function(co1=NULL,before=NULL,after=NULL){
#   co <- substitute(co1); before=substitute(before)
#   j <- as.expression(bquote({
#     .(BE)
#     result <- mapply(function(.index,k,r){
#       .(CO)
#     },1:length(map.values),map.keys,map.values)
#     .(AF)
#   },list(CO=co,BE=before,AF=after)))
#   class(j) <- c(class(j),"rhmr-map")
#   j
# }
# # map1 <- expression({
# map1 <- ewrap({
#   # library(readr)
#   lines <- map.values
#   # if (grepl("tpep_pickup_datetime", map.values[[1]])) {
#   #   lines <- lines[-1]
#   # }
#   lines <- head(lines)
#   lines <- paste(lines, collapse = "\n")
#   # outputvalue <- parse_data(lines)
#   # output_key <- outputvalue$tpep_pickup_datetime[1]
#   outputvalue <- lines
#   outputkey <- map.keys[[1]]
#   rhcollect(outputkey, outputvalue)
# })
#
# # reduce1 <- expression(
# #   pre = {
# #     reduceoutputvalue <- data.frame()
# #   },
# #   reduce = {
# #     reduceoutputvalue <- rbind(reduceoutputvalue, do.call(rbind, reduce.values))
# #   },
# #   post = {
# #     reduceoutputkey <- reduce.key[1]
# #     attr(reduceoutputvalue, "location") <- reduce.key[1:3]
# #     names(attr(reduceoutputvalue, "location")) <- c("FIPS","county","state")
# #     rhcollect(reduceoutputkey, reduceoutputvalue)
# #   }
# # )
#
# mr1 <- rhwatch(
#   map      = map1,
#   input    = rhfmt("/user/bschloe/taxi_raw/", type = "text"),
#   output   = rhfmt("/user/bschloe/taxi_rhipe/", type = "sequence"),
#   parameters = "all",
#   mon.sec = 1,
#   debug = "collect",
#   readback = FALSE
# )
