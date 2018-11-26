# ================================================================================
# Build your own Navy 2.0
# Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey
# --------------------------------------------------------------------------------
# data processing 
# build your own Navy and see the implications of your shipbuilding plan
# ================================================================================

# --------------------------------------------------------------------------------

# input: navy_350_data.xlsx
# output: navy_data.Rda, ships_stats.Rda 

# --------------------------------------------------------------------------------
# load packages

library(magrittr)
library(tidyverse)
library(openxlsx)

# --------------------------------------------------------------------------------
# load data

navy_book <- loadWorkbook("data/navy_data.xlsx")

# --------------------------------------------------------------------------------
# transform data

navy_data <- list()

for(i in 2:length(names(navy_book))){
  # read sheet
  navy_data[[i-1]] <- read.xlsx(
    xlsxFile = navy_book,
    sheet = i,
    startRow = 3
  )
  
  # remove 2016
  navy_data[[i-1]] %<>%
    filter(FY >= 2018 & FY <= 2046)
  
  # turn . back into spaces in the variable names
  names(navy_data[[i-1]]) <- gsub("\\."," ", names(navy_data[[i-1]]))
  
  # name the sheet
  names(navy_data)[i-1] <- names(navy_book)[i]
}

# add blank sheet for use input
navy_data[["Blank Sheet"]] <- navy_data[["Build Plan"]]
navy_data[["Blank Sheet"]][,-1] <- 0

# --------------------------------------------------------------------------------
# save data: navy_data.Rda

save("navy_data", file = "data/navy_data.Rda")

# --------------------------------------------------------------------------------
# prepare for ship_stats_full

# add navy data for calculations within app
navy_data <- read.xlsx(
  xlsxFile = navy_book,
  sheet = 1,
  startRow = 7)

create_stat_frame <- function(var_name,dataset){
  new_frame <- dataset %>%
    filter(X1 == var_name) %>%
    select(-X1)
  names(new_frame) <- gsub("\\."," ", names(new_frame))
  return(new_frame)
}

ship_stats <- list()
for(i in 1:nrow(navy_data)){
  ship_stats[[i]] <- assign(gsub("\\."," ", navy_data$X1[i]),
    create_stat_frame(navy_data$X1[i],navy_data)
)}

names(ship_stats) <- gsub("\\."," ", navy_data$X1)  

# --------------------------------------------------------------------------------
# save data 

save("ship_stats", file = "data/ship_stats.Rda")

# --------------------------------------------------------------------------------
# transform data 

new_ship_info <- loadWorkbook("data/new_ship_stats.xlsx")

new_ship <- read.xlsx(
  xlsxFile = new_ship_info,
  sheet = 1,
  startRow = 1
)

new_ship_stats <- list()

for(i in 1:nrow(new_ship)){
  new_ship_stats[[i]] <- assign(gsub("\\."," ", new_ship$X1[i]),
                                    create_stat_frame(new_ship$X1[i],new_ship))
}

names(new_ship_stats) <- gsub("\\."," ", new_ship$X1)  


# Combine two stats list which have the same structure
ship_stats_full <- mapply(cbind,ship_stats,new_ship_stats,SIMPLIFY = FALSE)

# --------------------------------------------------------------------------------
# save data 

save("ship_stats_full", file = "data/ship_stats_full.Rda")

# --------------------------------------------------------------------------------
# load data 

load("data/navy_data.Rda")

# --------------------------------------------------------------------------------
# transform data 

future_ships <- c("CVN-X","Cruiser-X","Destroyer-X","Frigate-X","SSN-X","SSGN-X","SSBN-X","AWS-X")

for(i in 1:length(future_ships)){

    col <- which(names(navy_data[[1]]) == future_ships[i])
    if(is_empty(col)) col <- length(navy_data[[1]]) + 1
    years <- nrow(navy_data[[1]])
    stats_col <- which(names(ship_stats_full[[1]]) == future_ships[i])
    
    navy_data[["Build Plan"]][col] <- rep(0, years)
    navy_data[["R&D Cost"]][col] <- rep(0, years) 
    navy_data[["Fleet Plan"]][col] <- rep(0, years)
    navy_data[["O&S Yearly"]][col] <- 
        rep(unlist(ship_stats_full[["Yearly O&S"]][stats_col]), years)
    navy_data[["Acquisition Yearly"]][col] <-
        rep(unlist(ship_stats_full[["Acquisition Cost"]][stats_col]), years)
    navy_data[["Planned Work"]][col] <- rep(0, years)
    
    navy_data[["Free Capacity"]][col] <- c(
        rep(0, unlist(ship_stats_full[["First Available"]][stats_col]) - navy_data[[1]]$FY[1]),
        rep(unlist(ship_stats_full[["Max Build Per Year"]][stats_col]),
            navy_data[[1]]$FY[nrow(navy_data[[1]])] -
                unlist(ship_stats_full[["First Available"]][stats_col]) + 1))
    
    navy_data[["Sum Planned Work"]][col] <- rep(0, years)
    navy_data[["Sum Free Capacity"]][col] <- 
        cumsum(navy_data[["Free Capacity"]][col])
    navy_data[["Max New Builds"]][col] <- 
        floor(navy_data[["Sum Free Capacity"]][col])
    navy_data[["Max Cut Builds"]][col] <- rep(0, years)
    navy_data[["Max New O&S"]][col] <- navy_data[["Max New Builds"]][col] * 
        navy_data[["O&S Yearly"]][col]
    navy_data[["Max Cut O&S"]][col] <- rep(0, years)
    navy_data[["Max New Acq"]][col] <- navy_data[["Free Capacity"]][col] *
        navy_data[["Acquisition Yearly"]][col]
    navy_data[["Max Cut Acq"]][col] <- rep(0, years)
    navy_data[["Blank Sheet"]][col] <- rep(0, years)
    
    for(j in 1:length(navy_data)){
        names(navy_data[[j]])[col] <- future_ships[i]
    }
}

# --------------------------------------------------------------------------------
# save data: navy_data_full.Rda

save(navy_data, file = "data/navy_data_full.Rda")
