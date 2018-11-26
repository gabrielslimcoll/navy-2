# ================================================================================
# Build your own: Navy + 
# Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey
# --------------------------------------------------------------------------------
# app functions 
# build your own Navy and see the implications of your shipbuilding plan
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(tidyverse)
library(forcats)
library(extrafont)
library(extrafontdb)

# --------------------------------------------------------------------------------
# sort platforms

Aircraft_Carriers <- c("Nimitz", "Ford", "CVN-X")
Large_Surface_Combatants <-
  c(
    "Ticonderoga",
    "Burke",
    "BurkeIII",
    "Zumwalt",
    "FutureLSC",
    "Cruiser-X",
    "Destroyer-X"
  )

Small_Surface_Combatants <-
  c("Avenger",
    "LittoralCombatShip",
    "FastFrigate",
    "FutureSSC",
    "Frigate-X")

Attack_Submarines <-
  c("LosAngeles",
    "LosAngelesi",
    "Seawolf",
    "Virginia",
    "Virginiai",
    "SSN-X")

Cruise_Missile_Submarines <- c("OhioSSGN", "SSGN-X")

Ballistic_Missile_Submarines <- c("OhioSSBN", "Columbia", "SSBN-X")

Amphibious_Warfare_Ships <-
  c("WhidbeyIsland",
    "Wasp",
    "HarpersFerry",
    "SanAntonio",
    "America",
    "LXR",
    "AWS-X")

Logistics_and_Support <- c("CLF", "SupportVessels")

All <-
  c(
    Aircraft_Carriers,
    Large_Surface_Combatants,
    Small_Surface_Combatants,
    Attack_Submarines,
    Cruise_Missile_Submarines,
    Ballistic_Missile_Submarines,
    Amphibious_Warfare_Ships,
    Logistics_and_Support
  )

All_list <-
  list(
    Aircraft_Carriers,
    Large_Surface_Combatants,
    Small_Surface_Combatants,
    Attack_Submarines,
    Cruise_Missile_Submarines,
    Ballistic_Missile_Submarines,
    Amphibious_Warfare_Ships,
    Logistics_and_Support
  )

names(All_list) <- c(
  "Aircraft Carriers",
  "Large Surface Combatants",
  "Small Surface Combatants",
  "Attack Submarines",
  
  "Cruise Missile Submarines",
  "Ballistic Missile Submarines",
  "Amphibious Warfare Ships",
  "Logistics and Support"
)

# ================================================================================
# update_current_ship

# This function should be called only when observeEvent() actually sees the user inputs; it should not iterate through all
# ships again and again even if user only changes the slider of one ship

update_current_ship <- function(# note: returns the updated vector of ship counts, given the user's choice of a
  # ...new level for 2046
  # ...ship_name: character string giving ship name
  # ...input: shiny input object
  # ...navy_data: list of data frames
  
  ship_name,
  input,
  current,
  session = getDefaultReactiveDomain()) {
  input_name <- gsub(" ", "_", ship_name)
  if (class(input) == "numeric" | class(input) == "integer") {
    new_number <- input
  } else
    new_number <- input$slider
  orig_number <- current$navy_data[["Fleet Plan"]][nrow(current$navy_data[["Fleet Plan"]]),
                                                   ship_name]
  
  if (!is.null(new_number)) {
    if (new_number == orig_number)
      return(current$navy_data[["Fleet Plan"]][, ship_name])
    
    if (new_number > orig_number) {
      new_builds <- new_number - orig_number
      achieved <-
        min(which(current$navy_data[["Max New Builds"]][, ship_name] >= new_builds))
      
      new_yearly_quantities <-
        current$navy_data[["Fleet Plan"]][, ship_name] + new_builds
      new_yearly_quantities[1:achieved] <-
        current$navy_data[["Fleet Plan"]][1:achieved, ship_name] +
        current$navy_data[["Max New Builds"]][1:achieved, ship_name]
      new_yearly_quantities[achieved] <-
        current$navy_data[["Fleet Plan"]][achieved, ship_name] + new_builds
      
      return(new_yearly_quantities)
    }
    
    if (new_number < orig_number) {
      cut_builds <- orig_number - new_number
      achieved <-
        min(which(current$navy_data[["Max Cut Builds"]][, ship_name] >= cut_builds))
      
      new_yearly_quantities <-
        current$navy_data[["Fleet Plan"]][, ship_name] - cut_builds
      new_yearly_quantities[1:achieved] <-
        current$navy_data[["Fleet Plan"]][1:achieved, ship_name] -
        current$navy_data[["Max Cut Builds"]][1:achieved, ship_name]
      new_yearly_quantities[achieved] <-
        current$navy_data[["Fleet Plan"]][achieved, ship_name] - cut_builds
      
      return(new_yearly_quantities)
    }
  }
}

# ================================================================================
# add_diigtheme

# note: a ggplot object to add the theme to

add_diigtheme <- function(passed_plot) {
  themed_plot <- passed_plot +
    theme(
      plot.title = element_text(
        family = "Open Sans",
        color = "#554449",
        size = 18,
        face = "bold",
        hjust = .5
      ),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#FCFCFC"),
      plot.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      legend.background = element_rect(fill = "#FCFCFC", colour = "#FCFCFC"),
      panel.grid.major.x = element_line(size = .1, color = "grey80"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size = .1, color = "grey80"),
      panel.grid.minor.y = element_line(size = .1, color = "grey80"),
      legend.text = element_text(
        size = 12,
        family = "Open Sans",
        color = "#554449"
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#FCFCFC", color = "#FCFCFC"),
      legend.key.width = unit(2, "line"),
      
      axis.text.x = element_text(
        size = 12,
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 0, 0, 0),
        angle = 0
      ),
      
      axis.ticks.length = unit(.00, "cm"),
      
      axis.text.y = element_text(
        size = 12,
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 5, 0, 0)
      ),
      
      axis.title.x = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans",
        margin = margin(15, 0, 0, 0)
      ),
      
      axis.title.y = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans",
        margin = margin(0, 15, 0, 0)
      ),
      
      legend.direction = "horizontal"
    )
  
  return(themed_plot)
}

# ================================================================================
# find_budget_y_limits

# returns a vector for y-axis plot limits: c(minimum, maximum)
# note: a budget data frame, formatted for plotting

find_budget_y_limits <- function(budget_data) {
  datamax <- budget_data %>%
    filter(
      budget_cat == "Added O&S" |
        budget_cat == "Added Acq" | budget_cat == "Added R&D" |
        budget_cat == "O&S" |
        budget_cat == "Acquisition" | budget_cat == "R&D"
    ) %>%
    group_by(FY) %>%
    mutate(Total = sum(Total)) %>%
    ungroup %>%
    summarize(datamax = max(Total)) %>%
    unlist
  datamin <- budget_data %>%
    filter(
      budget_cat == "Cut O&S" |
        budget_cat == "Cut Acq" | budget_cat == "Cut R&D" |
        budget_cat == "O&S" |
        budget_cat == "Acquisition" | budget_cat == "R&D"
    ) %>%
    group_by(FY) %>%
    mutate(Total = sum(Total)) %>%
    ungroup %>%
    summarize(datamin = min(Total)) %>%
    unlist
  ymax <- (ceiling((datamax + 1) / 10000000000) * 10000000000)
  ymin <- (floor((datamin) / 10000000000) * 10000000000)
  
  return(c(ymin, ymax))
}

# ================================================================================
# get_ships_data

# note: current_ships, current ships dataframe
# ...input: shiny input object

get_ships_data <- function(current_ships,
                           navy_data,
                           stats_df,
                           input,
                           session = getDefaultReactiveDomain()) {
  baseline <- navy_data[["Fleet Plan"]]
  new_plan <- current_ships
  
  if (input$AircraftCarriers == 1) {
    baseline <- baseline[names(baseline) %in% c("FY", Aircraft_Carriers)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Aircraft_Carriers)]
  }
  
  if (input$LSC == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Large_Surface_Combatants)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Large_Surface_Combatants)]
  }
  
  if (input$SSC == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Small_Surface_Combatants)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Small_Surface_Combatants)]
  }
  
  if (input$SSN == 1) {
    baseline <- baseline[names(baseline) %in% c("FY", Attack_Submarines)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Attack_Submarines)]
  }
  
  if (input$SSGN == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Cruise_Missile_Submarines)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Cruise_Missile_Submarines)]
  }
  
  if (input$SSBN == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Ballistic_Missile_Submarines)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Ballistic_Missile_Submarines)]
  }
  
  if (input$AWS == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Amphibious_Warfare_Ships)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Amphibious_Warfare_Ships)]
  }
  
  if (input$LogisticsSupport == 1) {
    baseline <-
      baseline[names(baseline) %in% c("FY", Logistics_and_Support)]
    new_plan <-
      new_plan[names(new_plan) %in% c("FY", Logistics_and_Support)]
  }
  
  if (input$top_chart == "Line" | input$top_chart == "Area") {
    if (input$top_y != "Ships" & !(input$top_y %in% c(# "Aircraft Capacity",
      # "VLS Tubes",
      "Speed (knots)",
      "Tonnage (lbs)"))) {
      for (i in 1:(ncol(baseline) - 1)) {
        baseline[, i + 1] <-
          round(baseline[, i + 1] * stats_df[[input$top_y]][, (names(baseline)[-1])[i]])
        new_plan[, i + 1] <-
          round(new_plan[, i + 1] * stats_df[[input$top_y]][, (names(baseline)[-1])[i]])
      }
      
      baseline$Total <- rowSums(baseline[, -1])
      baseline$Category <- "old plan ships"
      new_plan$Total <- rowSums(new_plan[, -1])
      new_plan$Category <- "new plan ships"
    }
    else if (input$top_y %in% c(# "Aircraft Capacity",
      # "VLS Tubes",
      "Speed (knots)",
      "Tonnage (lbs)")) {
      for (i in 1:(ncol(baseline) - 1)) {
        baseline[, i + 1] <-
          round(baseline[, i + 1] * stats_df[[input$top_y]][, (names(baseline)[-1])[i]])
        new_plan[, i + 1] <-
          round(new_plan[, i + 1] * stats_df[[input$top_y]][, (names(baseline)[-1])[i]])
      }
      
      # note: find the column name where the performance value equals to zero and
      # ...when calculating total number of ships, exclude them in sum
      
      col <-
        names(stats_df[[input$top_y]])[which(stats_df[[input$top_y]][1, ] == 0)]
      
      if (length(col) != 0) {
        baseline$Total <- rowSums(baseline[, -1]) /
          rowSums(navy_data[["Fleet Plan"]][names(navy_data[["Fleet Plan"]]) %in%
                                              names(baseline)[-1] &
                                              !names(navy_data[["Fleet Plan"]]) %in% col])
        baseline$Category <- "old plan ships"
        
        new_plan$Total <- rowSums(new_plan[, -1]) /
          rowSums(current_ships[names(current_ships) %in%
                                  names(baseline)[-1] &
                                  !names(current_ships) %in% col])
        new_plan$Category <- "new plan ships"
        
      } else{
        baseline$Total <- rowSums(baseline[, -1]) /
          rowSums(navy_data[["Fleet Plan"]][names(navy_data[["Fleet Plan"]]) %in%
                                              names(baseline)[-1]])
        baseline$Category <- "old plan ships"
        new_plan$Total <- rowSums(new_plan[, -1]) /
          rowSums(current_ships[names(current_ships) %in%  names(baseline)[-1]])
        new_plan$Category <- "new plan ships"
      }
    }
    
    else{
      baseline$Total <- rowSums(baseline[, -1])
      baseline$Category <- "old plan ships"
      new_plan$Total <- rowSums(new_plan[, -1])
      new_plan$Category <- "new plan ships"
    }
  }
  
  return(bind_rows(baseline, new_plan))
}

# ================================================================================
# apply_new_ship

apply_new_ship <- function(navy_data,
                           new_name,
                           stats_df,
                           rd_start,
                           rd_duration) {
  col <- which(names(navy_data[[1]]) == new_name)
  years <- nrow(navy_data[[1]])
  stats_col <- which(names(stats_df[[1]]) == new_name)
  
  navy_data[["Build Plan"]][col] <- rep(0, years)
  navy_data[["R&D Cost"]][col] <- c(
    rep(0, rd_start - navy_data[[1]]$FY[1]),
    rep(unlist(stats_df[["R&D Cost"]][stats_col]) / rd_duration,
        rd_duration),
    rep(0, navy_data[[1]]$FY[nrow(navy_data[[1]])] -
          rd_start - rd_duration + 1)
  )
  navy_data[["Fleet Plan"]][col] <- rep(0, years)
  navy_data[["O&S Yearly"]][col] <-
    rep(unlist(stats_df[["Yearly O&S"]][stats_col]), years)
  navy_data[["Acquisition Yearly"]][col] <-
    rep(unlist(stats_df[["Acquisition Cost"]][stats_col]), years)
  navy_data[["Planned Work"]][col] <- rep(0, years)
  navy_data[["Free Capacity"]][col] <- c(rep(0, unlist(stats_df[["First Available"]][stats_col])
                                             - navy_data[[1]]$FY[1]),
                                         rep(unlist(stats_df[["Max Build Per Year"]][stats_col]),
                                             navy_data[[1]]$FY[nrow(navy_data[[1]])] -
                                               unlist(stats_df[["First Available"]][stats_col]) + 1))
  navy_data[["Sum Planned Work"]][col] <- rep(0, years)
  navy_data[["Sum Free Capacity"]][col] <-
    cumsum(navy_data[["Free Capacity"]][col])
  navy_data[["Max New Builds"]][col] <-
    floor(navy_data[["Sum Free Capacity"]][col])
  navy_data[["Max Cut Builds"]][col] <- rep(0, years)
  navy_data[["Max New O&S"]][col] <-
    navy_data[["Max New Builds"]][col] *
    navy_data[["O&S Yearly"]][col]
  navy_data[["Max Cut O&S"]][col] <- rep(0, years)
  navy_data[["Max New Acq"]][col] <-
    navy_data[["Free Capacity"]][col] *
    navy_data[["Acquisition Yearly"]][col]
  navy_data[["Max Cut Acq"]][col] <- rep(0, years)
  navy_data[["Blank Sheet"]][col] <- rep(0, years)
  
  for (i in 1:length(navy_data)) {
    names(navy_data[[i]])[col] <- new_name
  }
  
  return(navy_data)
}

# ================================================================================
# get_budget_change_data

# note: returns a tibble in long form for the budget plot

get_budget_change_data <- function(current,
                                   input,
                                   session = getDefaultReactiveDomain()) {
  ship_change <- current$navy_data[["Fleet Plan"]]
  ship_change[-1] <- current$ships[-1] - ship_change[-1]
  
  # added O&S
  added_os <- ship_change
  
  # start count o&s in the year after ship built
  added_os$FY <- added_os$FY + 1
  added_os <- bind_rows(filter(current$navy_data[["Blank Sheet"]], FY == min(FY)),
                        filter(added_os, FY != max(FY)))
  
  added_os[-1] <-
    added_os[-1] * current$navy_data[["O&S Yearly"]][-1]
  added_os[-1][added_os[-1] < 0] <- 0
  added_os$Total <- rowSums(added_os[-1])
  added_os$budget_cat <- "Added O&S"
  
  # cut O&S
  cut_os <- ship_change
  
  # start count o&s in the year after ship built
  cut_os$FY <- cut_os$FY + 1
  cut_os <- bind_rows(filter(current$navy_data[["Blank Sheet"]], FY == min(FY)),
                      filter(cut_os, FY != max(FY)))
  
  cut_os[-1] <- cut_os[-1] * current$navy_data[["O&S Yearly"]][-1]
  cut_os[-1][cut_os[-1] > 0] <- 0
  cut_os$Total <- rowSums(cut_os[-1])
  cut_os$budget_cat <- "Cut O&S"
  
  # ================================================================================
  # get_added_acq
  
  get_added_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] > 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == max(ship_change[[shipname]]))[1]
    
    added_acq <-
      rep(0.0, times = length(current$navy_data[["Max New Acq"]][[shipname]]))
    
    added_acq[1:(final_yr)] <-
      current$navy_data[["Max New Acq"]][[shipname]][1:(final_yr)]
    
    if ((ship_change[[shipname]][final_yr] <
         current$navy_data[["Sum Free Capacity"]][[shipname]][final_yr]) &
        (ship_change[[shipname]][final_yr] >=
         current$navy_data[["Sum Free Capacity"]][[shipname]][final_yr -
                                                              1])) {
      added_acq[final_yr] <- added_acq[final_yr] -
        (current$navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (current$navy_data[["Sum Free Capacity"]][[shipname]][final_yr] -
              ship_change[[shipname]][final_yr]))
    }
    
    return(added_acq)
  }
  
  added_acq <- ship_change
  added_acq[-1] <- sapply(names(added_acq[-1]), get_added_acq)
  added_acq$Total <- rowSums(added_acq[-1])
  added_acq$budget_cat <- "Added Acq"
  
  # ================================================================================
  # get_cut_acq
  
  get_cut_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] < 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == min(ship_change[[shipname]]))[1]
    
    cut_acq <-
      rep(0.0, times = length(current$navy_data[["Max Cut Acq"]][[shipname]]))
    
    cut_acq[1:(final_yr)] <-
      -1 * (current$navy_data[["Max Cut Acq"]][[shipname]][1:(final_yr)])
    
    if (-1 * ship_change[[shipname]][final_yr] <
        current$navy_data[["Sum Planned Work"]][[shipname]][final_yr]) {
      cut_acq[final_yr] <- cut_acq[final_yr] +
        (current$navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (current$navy_data[["Sum Planned Work"]][[shipname]][final_yr] +
              ship_change[[shipname]][final_yr]))
    }
    
    return(cut_acq)
  }
  
  cut_acq <- ship_change
  cut_acq[-1] <- sapply(names(cut_acq[-1]), get_cut_acq)
  
  rd_test <- ship_change
  
  # for existing builds, find the column index of keep or cancel
  current_keep_col <- c()
  current_keep_col[1] <- 0
  current_cancel_col <- c()
  current_cancel_col[1] <- 0
  current_cancel_yr <- list()
  
  
  for (i in 2:(ncol(rd_test) - 8)) {
    col <- paste0(colnames(rd_test[i]), "-", "cancel")
    if (input[[col]] == "Keep") {
      current_keep_col[i] <- i
    } else if (input[[col]] == "Retire") {
      current_cancel_col[i] <- i
      current_cancel_yr[[as.character(i)]] <-
        input[[paste0(colnames(rd_test)[i], "-", "Yr")]]
    }
  }
  
  # remove all NA and 0 in these two column index vectors
  current_keep_col <- current_keep_col[!is.na(current_keep_col)]
  current_keep_col <- current_keep_col[current_keep_col != 0]
  
  if (!is.null(current_cancel_col)) {
    current_cancel_col <- current_cancel_col[!is.na(current_cancel_col)]
    current_cancel_col <-
      current_cancel_col[current_cancel_col != 0]
  }
  
  
  # only cancelling the existing builds will cause budget change --- keeping them will not
  if (length(current_cancel_col) != 0) {
    rd_test[, current_cancel_col][rd_test[, current_cancel_col] < 0] <-
      -1
  }
  rd_test[, current_keep_col] <- 0
  rd_test[, 1] <- 0
  
  # building new ships will cause added R&D; cancelling it will not cause budget change
  rd_future <- current$navy_data[["R&D Cost"]]
  rd_future[, 2:28][rd_future[, 2:28] != 0] <- 0
  rd_future[, c(1, 29:36)][rd_future[, c(1, 29:36)] > 0] <- 1
  
  rd_full <- rd_test + rd_future
  
  # rd change
  rd_change <- rd_full * current$navy_data[["R&D Cost"]]
  
  # added rd
  added_rd <- rd_change
  added_rd[added_rd < 0] <- 0
  added_rd$Total <- rowSums(added_rd[-1])
  added_rd$budget_cat <- "Added R&D"
  
  # cut rd
  cut_rd <- rd_change
  cut_rd[-1][cut_rd[-1] > 0] <- 0
  cut_rd$Total <- rowSums(cut_rd[-1])
  cut_rd$budget_cat <- "Cut R&D"
  
  # --------------------------------------------------------------------------------
  
  # continue updating cut_acq, address the situations when user retire the fleet
  if (length(current_cancel_col) > 0) {
    retire_yr <- c()
    for (i in 1:length(current_cancel_col)) {
      retire_yr[i] <-
        current_cancel_yr[[as.character(current_cancel_col[i])]]
      cut_acq[, current_cancel_col[i]] <-
        (c(rep(1, retire_yr[i] - 2018), rep(0, 2046 - retire_yr[i] + 1)) - c(rep(1, 2046 -
                                                                                   2017))) *
        navy_data[["Planned Work"]][, current_cancel_col[i]] * navy_data$`Acquisition Yearly`[, current_cancel_col[i]]
    }
  }
  cut_acq$Total <- rowSums(cut_acq[-1])
  cut_acq$budget_cat <- "Cut Acq"
  
  # --------------------------------------------------------------------------------
  
  return(bind_rows(added_acq, added_os, added_rd, cut_rd, cut_acq, cut_os))
}

# ================================================================================
# get_budget_total_data

# note: returns a tibble in long form for the budget plot
# ...current_ships: tibble of current ships
# navy_data: list of data frames

get_budget_total_data <- function(current,
                                  current_ships,
                                  navy_data,
                                  input,
                                  session = getDefaultReactiveDomain()) {
  ship_total <- current_ships
  ship_change <- navy_data[["Fleet Plan"]]
  ship_change[-1] <- current_ships[-1] - ship_change[-1]
  
  # find which ship user chooses to keep/retire (valid for current ships)
  current_keep_col <- c()
  current_cancel_col <- c()
  current_cancel_yr <- list()
  
  for (i in 2:(ncol(ship_total) - 8)) {
    col <- paste0(colnames(ship_total[i]), "-", "cancel")
    if (input[[col]] == "Keep") {
      current_keep_col[i] <- i
    } else if (input[[col]] == "Retire") {
      current_cancel_col[i] <- i
      current_cancel_yr[[as.character(i)]] <-
        input[[paste0(colnames(ship_total)[i], "-", "Yr")]]
    }
  }
  
  current_keep_col <- current_keep_col[!is.na(current_keep_col)]
  
  if (!is.null(current_cancel_col)) {
    current_cancel_col <- current_cancel_col[!is.na(current_cancel_col)]
  }
  
  # O&S
  os <- ship_total
  
  # start count o&s in the year after the ship built
  os$FY <- os$FY + 1
  os <- bind_rows(filter(ship_total, FY == min(FY)),
                  filter(os, FY != max(FY)))
  
  os[-1] <- os[-1] * navy_data[["O&S Yearly"]][-1]
  os$Total <- rowSums(os[-1])
  os$budget_cat <- "O&S"
  
  # ================================================================================
  # get_added_acq
  
  # acquisition
  get_added_acq <- function(shipname) {
    if (!any(ship_change[[shipname]] > 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == max(ship_change[[shipname]]))[1]
    
    added_acq <-
      rep(0.0, times = length(navy_data[["Max New Acq"]][[shipname]]))
    
    added_acq[1:(final_yr)] <-
      navy_data[["Max New Acq"]][[shipname]][1:(final_yr)]
    
    if (ship_change[[shipname]][final_yr] <
        navy_data[["Sum Free Capacity"]][[shipname]][final_yr] &
        (ship_change[[shipname]][final_yr] >=
         current$navy_data[["Sum Free Capacity"]][[shipname]][final_yr -
                                                              1])) {
      added_acq[final_yr] <- added_acq[final_yr] -
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Free Capacity"]][[shipname]][final_yr] -
              ship_change[[shipname]][final_yr]))
    }
    
    return(added_acq)
  }
  
  acq <- navy_data[["Planned Work"]]
  acq[-1] <- sapply(names(acq[-1]), get_added_acq)
  
  # data processing for cancelled ships: retire ships based on original fleet plan
  if (length(current_cancel_col) > 0) {
    acq[, current_cancel_col] <- 0
    retire_yr <- c()
    for (i in 1:length(current_cancel_col)) {
      retire_yr[i] <-
        current_cancel_yr[[as.character(current_cancel_col[i])]]
      
      # cancel planned work based on retirement year and retirement ships type
      navy_data[["Planned Work"]][, current_cancel_col[i]] <-
        c(rep(1, retire_yr[i] - 2018), rep(0, 2046 - retire_yr[i] + 1)) *
        navy_data[["Planned Work"]][, current_cancel_col[i]]
    }
  }
  
  acq[-1] <- acq[-1] +
    (navy_data[["Planned Work"]][-1] * navy_data[["Acquisition Yearly"]][-1])
  
  # ================================================================================
  # get_cut_acq
  
  get_cut_acq <- function(shipname) {
    # if not building more ships, the "added" part will all be 0
    if (!any(ship_change[[shipname]] < 0)) {
      return(rep(0.0, times = length(ship_change[[shipname]])))
    }
    
    final_yr <-
      which(ship_change[[shipname]] == min(ship_change[[shipname]]))[1]
    
    cut_acq <-
      rep(0.0, times = length(navy_data[["Max Cut Acq"]][[shipname]]))
    
    cut_acq[1:(final_yr)] <-
      -1 * (navy_data[["Max Cut Acq"]][[shipname]][1:(final_yr)])
    
    if (-1 * ship_change[[shipname]][final_yr] <
        navy_data[["Sum Planned Work"]][[shipname]][final_yr]) {
      cut_acq[final_yr] <- cut_acq[final_yr] +
        (navy_data[["Acquisition Yearly"]][[shipname]][final_yr] *
           (navy_data[["Sum Planned Work"]][[shipname]][final_yr] +
              ship_change[[shipname]][final_yr]))
    }
    
    return(cut_acq)
  }
  
  cut_acq <- navy_data[["Planned Work"]]
  cut_acq[-1] <- sapply(names(cut_acq[-1]), get_cut_acq)
  
  cut_acq[, current_cancel_col] <- 0
  
  acq[-1] <- acq[-1] + cut_acq[-1]
  acq$Total <- rowSums(acq[-1])
  acq$budget_cat <- "Acquisition"
  
  # --------------------------------------------------------------------------------
  
  # total R&D
  rd_current <- current$ships
  rd_current[rd_current != 0] <- 1
  
  rd_future <- current$navy_data[["R&D Cost"]]
  rd_future[, 29:36][rd_future[, 29:36] > 0] <- 1
  rd_future[, 1:28][rd_future[, 1:28] != 0] <- 0
  
  rd_full <- rd_current + rd_future
  rd <- rd_full * current$navy_data[["R&D Cost"]]
  rd$Total <- rowSums(rd[-1])
  rd$budget_cat <- "R&D"
  
  return(bind_rows(acq, os, rd))
}

# --------------------------------------------------------------------------------
# deflate_frame

deflate_frame <- function(frame) {
  deflate <-
    c(
      "2017" = 1.02050053248136,
      "2018" = 1.04233226837061,
      "2019" = 1.065406461,
      "2020" = 1.089368122,
      "2021" = 1.113862265,
      "2022" = 1.138907152,
      "2023" = 1.164515166,
      "2024" = 1.190698969,
      "2025" = 1.217471508,
      "2026" = 1.24484602,
      "2027" = 1.272836041,
      "2028" = 1.301455409,
      "2029" = 1.330718276,
      "2030" = 1.360639111,
      "2031" = 1.391232707,
      "2032" = 1.422514192,
      "2033" = 1.454499032,
      "2034" = 1.487203043,
      "2035" = 1.520642395,
      "2036" = 1.554833621,
      "2037" = 1.589793627,
      "2038" = 1.6255397,
      "2039" = 1.662089513,
      "2040" = 1.699461139,
      "2041" = 1.737673055,
      "2042" = 1.776744156,
      "2043" = 1.81669376,
      "2044" = 1.85754162,
      "2045" = 1.899307932,
      "2046" = 1.942013349
    )
  
  deflated_frame <- as_tibble(sapply(names(frame), function(var_name) {
    if (var_name == "FY" |
        var_name == "Category" | var_name == "budget_cat") {
      return(frame[[var_name]])
    }
    return(round(frame[[var_name]] / deflate[as.character(frame[["FY"]])]))
  })
  ,
  validate = FALSE)
  deflated_frame[,
                 which(!(names(deflated_frame) %in% c("Category", "budget_cat")))] %<>% sapply(as.numeric)
  
  return(deflated_frame)
}

# ================================================================================
# format_ylab

# note: formats the y-axis labels for the ship plot
# ...x: a vector of axis labels, as numeric

format_ylab <- function(x) {
  find_one <- function(x) {
    if (is.na(x))
      return(NULL)
    if (abs(x) < 1)
      return(as.character(round(x, 3)))
    if (abs(x) < 10)
      return(as.character(round(x, 2)))
    if (abs(x) < 100)
      return(as.character(round(x, 1)))
    if (abs(x) < 1000)
      return(as.character(round(x)))
    if (abs(x) < 1e4)
      return(paste0(as.character(round(x / 1e3, 2)), "k"))
    if (abs(x) < 1e5)
      return(paste0(as.character(round(x / 1e3, 1)), "k"))
    if (abs(x) < 1e6)
      return(paste0(as.character(round(x / 1e3)), "k"))
    if (abs(x) < 1e7)
      return(paste0(as.character(round(x / 1e6, 2)), "M"))
    if (abs(x) < 1e8)
      return(paste0(as.character(round(x / 1e6, 1)), "M"))
    if (abs(x) < 1e9)
      return(paste0(as.character(round(x / 1e6)), "M"))
    if (abs(x) < 1e10)
      return(paste0(as.character(round(x / 1e9, 2)), "B"))
    if (abs(x) < 1e11)
      return(paste0(as.character(round(x / 1e9, 1)), "B"))
    if (abs(x) < 1e12)
      return(paste0(as.character(round(x / 1e9)), "B"))
    if (abs(x) < 1e13)
      return(paste0(as.character(round(x / 1e12, 2)), "T"))
    if (abs(x) < 1e14)
      return(paste0(as.character(round(x / 1e12, 1)), "T"))
    if (abs(x) < 1e15)
      return(paste0(as.character(round(x / 1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# ================================================================================
# format_ylab2

# note: formats the y-axis labels for the ship plot
# ...x: a vector of axis labels, as numeric

format_ylab2 <- function(x) {
  find_one <- function(x) {
    if (is.na(x))
      return(NULL)
    if (abs(x) < 1)
      return(as.character(round(x, 3)))
    if (abs(x) < 10)
      return(as.character(round(x, 2)))
    if (abs(x) < 100)
      return(as.character(round(x, 1)))
    if (abs(x) < 1000)
      return(as.character(round(x)))
    if (abs(x) < 1e4)
      return(paste0("$", as.character(round(x / 1e3, 2)), "k"))
    if (abs(x) < 1e5)
      return(paste0("$", as.character(round(x / 1e3, 1)), "k"))
    if (abs(x) < 1e6)
      return(paste0("$", as.character(round(x / 1e3)), "k"))
    if (abs(x) < 1e7)
      return(paste0("$", as.character(round(x / 1e6, 2)), "M"))
    if (abs(x) < 1e8)
      return(paste0("$", as.character(round(x / 1e6, 1)), "M"))
    if (abs(x) < 1e9)
      return(paste0("$", as.character(round(x / 1e6)), "M"))
    if (abs(x) < 1e10)
      return(paste0("$", as.character(round(x / 1e9, 2)), "B"))
    if (abs(x) < 1e11)
      return(paste0("$", as.character(round(x / 1e9, 1)), "B"))
    if (abs(x) < 1e12)
      return(paste0("$", as.character(round(x / 1e9)), "B"))
    if (abs(x) < 1e13)
      return(paste0("$", as.character(round(x / 1e12, 2)), "T"))
    if (abs(x) < 1e14)
      return(paste0("$", as.character(round(x / 1e12, 1)), "T"))
    if (abs(x) < 1e15)
      return(paste0("$", as.character(round(x / 1e12)), "T"))
    return(x)
  }
  return(sapply(x, find_one))
  return(x)
}

# ================================================================================
# create_change_tip

# note: returns a html-formatted string to use in the budget change chart tooltip
# hover_year: year the mouse is over
# budget_dataset: current budget data

create_change_tip <- function(hover_year,
                              budget_dataset,
                              session = getDefaultReactiveDomain()) {
  this_year <- filter(budget_dataset, FY == hover_year)
  until_now <- filter(budget_dataset, FY <= hover_year)
  first_year <- min(until_now$FY)
  
  tip <- paste0(
    # "<div align = 'center'>",
    "<div align = 'left'>",
    "<b>Budget for FY",
    hover_year,
    "</b><br>",
    br(),
    "<div align = 'left'>",
    " Added Acq: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Added Acq")]
          / 1e9, 2),
    "B<br/>",
    " Cut Acq: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Cut Acq")]
          / 1e9, 2),
    "B<br/>",
    " Added O&S: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Added O&S")]
          / 1e9, 2),
    "B<br/>",
    " Cut O&S: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Cut O&S")]
          / 1e9, 2),
    "B<br/>",
    " Added R&D: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Added R&D")]
          / 1e9, 2),
    "B<br/>",
    " Cut R&D: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "Cut R&D")]
          / 1e9, 2),
    "B<br/>",
    "<u> ",
    " Net Change: $",
    round(summarize(this_year, sum(Total)) / 1e9, 2),
    "B</u><br/>",
    br(),
    "<div align = 'left'>",
    "<b>Budget for FY",
    first_year,
    "-",
    hover_year,
    "</b><br>",
    br(),
    "<div align = 'left'>",
    " Added Acq: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Added Acq"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    " Cut Acq: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Cut Acq"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    " Added O&S: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Added O&S"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    " Cut O&S: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Cut O&S"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    " Added R&D: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Added R&D"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    " Cut R&D: $</b>",
    round(summarize(
      filter(until_now, budget_cat == "Cut R&D"), sum(Total)
    ) /
      1e9, 2),
    "B<br/>",
    "<u> Net Change: $",
    round(summarize(until_now, sum(Total)) / 1e9, 2),
    "B</u><br/>",
    br(),
    "</div>",
    
    br(),
    
    "<div align = 'left'>",
    "<b>Annual Avg. Budget for FY",
    first_year,
    "-",
    hover_year,
    "</b><br>",
    br(),
    "<div align = 'left'>",
    " Added Acq: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Added Acq"), sum(Total)
    ) /
      1e9) / (hover_year - 2017), 2),
    "B<br/>",
    " Cut Acq: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Cut Acq"), sum(Total)
    ) /
      1e9) / (hover_year - 2017), 2),
    "B<br/>",
    " Added O&S: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Added O&S"), sum(Total)
    ) /
      1e9) / (hover_year = 2017), 2),
    "B<br/>",
    " Cut O&S: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Cut O&S"), sum(Total)
    ) /
      1e9) / (hover_year - 2017), 2),
    "B<br/>",
    " Added R&D: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Added R&D"), sum(Total)
    ) /
      1e9) / (hover_year - 2017), 2),
    "B<br/>",
    " Cut R&D: $</b>",
    round((summarize(
      filter(until_now, budget_cat == "Cut R&D"), sum(Total)
    ) /
      1e9) / (hover_year - 2017), 2),
    "B<br/>",
    "<u> Net Change: $",
    round((summarize(
      until_now, sum(Total)
    ) / 1e9) / (hover_year - 2017), 2),
    "B</u><br/>",
    br(),
    "</div>"
  )
  
  return(tip)
}

# ================================================================================
# create_total_tip

# note: returns a html-formatted string to use in the budget total chart tooltip
# hover_year: year the mouse is over
# budget_dataset: current budget data

create_total_tip <- function(hover_year,
                             budget_dataset,
                             session = getDefaultReactiveDomain()) {
  this_year <- filter(budget_dataset, FY == hover_year)
  until_now <- filter(budget_dataset, FY <= hover_year)
  first_year <- min(until_now$FY)
  
  tip <- paste0(
    "<div align = 'left'>",
    "<b>Budget for FY",
    hover_year,
    "</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $",
    round(this_year$Total[which(this_year$budget_cat == "Acquisition")]
          / 1e9, 2),
    "B<br/>",
    " O&S: $",
    round(this_year$Total[which(this_year$budget_cat == "O&S")]
          / 1e9, 2),
    "B<br/>",
    " (build your own) R&D: $</b>",
    round(this_year$Total[which(this_year$budget_cat == "R&D")]
          / 1e9, 2),
    "B<br/>",
    "<u> ",
    " Total: $",
    round(summarize(this_year, sum(Total)) / 1e9, 2),
    "B<br/></u>",
    br(),
    "<div align = 'left'>",
    "<b>Budget for FY",
    first_year,
    "-",
    hover_year,
    "</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "Acquisition")]
              / 1e9), 2),
    "B<br/>",
    " O&S: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "O&S")]
              / 1e9), 2),
    "B<br/>",
    " (build your own) R&D: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "R&D")]
              / 1e9), 2),
    "B<br/>",
    "<u> ",
    " Total: $",
    round(summarize(until_now, sum(Total)) / 1e9, 2),
    "B<br/></u>",
    "</div>",
    br(),
    "<div align = 'left'>",
    "<b>Annual Avg. Budget for FY",
    first_year,
    "-",
    hover_year,
    "</b><br>",
    "<div align = 'left'>",
    br(),
    " Acquisition: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "Acquisition")]
              / 1e9) / (hover_year - 2017), 2),
    "B<br/>",
    " O&S: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "O&S")]
              / 1e9), 2),
    "B<br/>",
    " (build your own) R&D: $</b>",
    round(sum(until_now$Total[which(until_now$budget_cat == "R&D")]
              / 1e9), 2),
    "B<br/>",
    "<u> ",
    " Average: $",
    round((summarize(
      until_now, sum(Total)
    ) / 1e9) / (hover_year - 2017), 2),
    "B<br/></u>",
    "</div>"
    
  )
  
  return(tip)
}

# ================================================================================
# update_title

# note: make ggplot title for both the top chart and the bottom chart reactive
# ...populates the title field with a dynamic title, if appropriate

update_title <- function(input,
                         session = getDefaultReactiveDomain()) {
  if (input$AircraftCarriers == 1) {
    title_part1 <- "Aircraft Carriers"
  } else if (input$LSC == 1) {
    title_part1 <- "Large Surface Combatants"
  } else if (input$SSC == 1) {
    title_part1 <- "Small Surface Combatants"
  } else if (input$SSN == 1) {
    title_part1 <- "Attack Submarines"
  } else if (input$SSGN == 1) {
    title_part1 <- "Cruise Missile Submarines"
  } else if (input$SSBN == 1) {
    title_part1 <- "Ballistic Missile Submarines"
  } else if (input$AWS == 1) {
    title_part1 <- "Amphibious Warfare Ships"
  } else if (input$LogisticsSupport == 1) {
    title_part1 <- "Logistics and Support"
  } else{
    title_part1 <- ""
  }
  
  title_top_part2 <- input$top_y
  title_top_part3 <- input$top_y
  
  title <- c()
  
  if (input$top_chart == "Line") {
    if (input$top_y == "Ships") {
      title[1] <- paste("USN", title_part1, "Inventory")
    }
    
    else if (input$top_y == "Speed (knots)") {
      title[1] <- paste("Avg. Speed (knots) for USN", title_part1)
    } else if (input$top_y == "Tonnage (lbs)") {
      title[1] <- paste("Avg. Tonnage (lbs) for USN", title_part1)
    } else{
      title[1] <- paste("USN", title_part1, title_top_part2)
    }
    
  } else{
    if (input$top_y == "Ships") {
      title[1] <- paste("USN", title_part1, "Inventory")
    }
    
    else if (input$top_y == "Speed (knots)") {
      title[1] <- paste("Avg. Speed (knots) for USN", title_part1)
    } else if (input$top_y == "Tonnage (lbs)") {
      title[1] <- paste("Avg. Tonnage (lbs) for USN", title_part1)
    } else{
      title[1] <- paste("USN", title_part1, title_top_part3)
    }
  }
  
  
  if (input$bottom_chart == "Total") {
    title[2] <- paste("Annual Funding for USN", title_part1)
  } else{
    title[2] <-
      paste("Change in Annual Funding for USN", title_part1)
  }
  return(title)
}

# ================================================================================
# (currently hidden) update_live_data

# update_live_data <- function(
#     current, ship_name, hex_code,
#     label_string, legend_order = "auto"){
#     # update the live data to include the new ship
#     ship_col <- which(names(current$ships) == ship_name)
#     if(is_empty(ship_col)){
#         ship_col <- length(current$ships) + 1}
#     current$ships[ship_col] <- current$navy_data[["Fleet Plan"]][[ship_name]]
#     names(current$ships)[ship_col] <- ship_name
#
#     # define a new area-graph color, label, and legend order for the new ship
#     if(!(ship_name %in% names(current$area_colors))){
#         current$area_colors %<>% append(c("tempname" = hex_code))
#         names(current$area_colors)[which(names(current$area_colors) == "tempname")] <-
#             ship_name}
#
#
#     if(!(ship_name %in% names(current$area_labels))){
#         current$area_labels %<>% append(c("tempname" = label_string))
#         names(current$area_labels)[which(names(current$area_labels) == "tempname")] <-
#             ship_name}
#
#     if(legend_order == "auto") legend_order <- length(current$legend_order)
#
#     if(!(ship_name %in% current$legend_order)){
#         current$legend_order %<>% append(ship_name, legend_order)}
#
#     return(current)
# }
#
#

# ================================================================================
# remove_from_current

# set the number of aricrafts in current to zero
remove_from_current <- function(current, ship_name, input) {
  if (!(ship_name %in% names(current$navy_data[[1]]))) {
    stop("remove_from_current failed - wrong ship name?")
  }
  
  current$ships[which(names(current$ships) == ship_name)] <- 0
  
  return(current)
}

# ================================================================================
# subset_budget_total

subset_budget_total <- function(shown, input) {
  if (input$AircraftCarriers == 1) {
    shown <- select(shown, FY, Aircraft_Carriers, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$LSC == 1) {
    shown <- select(shown, FY, Large_Surface_Combatants, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$SSC == 1) {
    shown <- select(shown, FY, Small_Surface_Combatants, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$SSN == 1) {
    shown <- select(shown, FY, Attack_Submarines, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$SSGN == 1) {
    shown <- select(shown, FY, Cruise_Missile_Submarines, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$SSBN == 1) {
    shown <- select(shown, FY, Ballistic_Missile_Submarines, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$AWS == 1) {
    shown <- select(shown, FY, Amphibious_Warfare_Ships, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  if (input$LogisticsSupport == 1) {
    shown <- select(shown, FY, Logistics_and_Support, budget_cat)
    shown$Total <- rowSums(shown[, -c(1, ncol(shown))])
  }
  
  return(shown)
}

# ================================================================================
# add_up_all_previous

# note: x is a numeric vector --- this function can sum up all the previous
# ...numbers including the current one it works like the Excel SUM() function

add_up_all_previous <- function(x) {
  for (i in 2:length(x)) {
    x[i] <- x[i - 1] + x[i]
  }
  return(x)
}

# ================================================================================
# update_max_new_OS

# x is a numeric vector --- this function will calculate the Max New O&S
update_max_new_OS <- function(current, shipname) {
  for (i in 2:length(current$navy_data$`Max New O&S`[[shipname]])) {
    current$navy_data$`Max New O&S`[[shipname]] <-
      current$navy_data$`O&S Yearly`[[shipname]][i] *
      current$navy_data$`Max New Builds`[[shipname]][i - 1]
  }
  return(current$navy_data$`Max New O&S`[[shipname]])
}

# ================================================================================
# update_planned_work

# update planned work after we update max build per year
update_planned_work <- function(shipname,
                                current,
                                input) {
  planned_work <- c()
  
  for (i in 1:nrow(current$navy_data$`Build Plan`)) {
    if (i <= (nrow(current$navy_data$`Build Plan`) - 2)) {
      planned_work[i] <-
        min(current$navy_data$`Build Plan`[[shipname]][i], input$br) +
        min(max(current$navy_data$`Build Plan`[[shipname]][i + 1] -
                  input$br, 0),
            input$br) +
        min(max(current$navy_data$`Build Plan`[[shipname]][i + 2] -
                  2 * input$br, 0),
            input$br)
    } else if (i == (nrow(current$navy_data$`Build Plan`) - 1)) {
      planned_work[i] <-
        min(current$navy_data$`Build Plan`[[shipname]][i], input$br) +
        min(max(current$navy_data$`Build Plan`[[shipname]][i + 1] -
                  input$br, 0),
            input$br)
    } else if (i == (nrow(current$navy_data$`Build Plan`))) {
      planned_work[i] <-
        min(current$navy_data$`Build Plan`[[shipname]][i], input$br)
    }
  }
  return(planned_work)
}

# ================================================================================
# update_bsModal_stats

# update statistics in the bsModal --- all in one function
update_bsModal_stats <- function(current,
                                 shipname,
                                 input) {
  shipname_abbr <- gsub("-", "", shipname)
  
  current$ship_stats_full$`Acquisition Cost`[[shipname]] <-
    input[[paste0(shipname_abbr, "_acquisition_cost")]]
  current$navy_data$`Acquisition Yearly`[[shipname]] <-
    input[[paste0(shipname_abbr, "_acquisition_cost")]]
  current$ship_stats_full$`Max Build Per Year`[[shipname]] <-
    input[[paste0(shipname_abbr, "_br")]]
  
  # Planned Work
  current$navy_data$`Planned Work`[[shipname]] <-
    update_planned_work(shipname, current, input)
  
  # Sum planned work
  current$navy_data$`Sum Planned Work`[[shipname]] <-
    add_up_all_previous(current$navy_data$`Planned Work`[[shipname]])
  
  # free capacity
  current$navy_data$`Free Capacity`[[shipname]] <-
    current$ship_stats_full$`Max Build Per Year`[[shipname]] - current$navy_data$`Planned Work`[[shipname]]
  
  # update sum free capacity
  current$navy_data$`Sum Free Capacity`[[shipname]] <-
    add_up_all_previous(current$navy_data$`Free Capacity`[[shipname]])
  
  # update Max New Builds
  current$navy_data$`Max New Builds`[[shipname]] <-
    floor(current$navy_data$`Sum Free Capacity`[[shipname]])
  
  # update max new acq
  current$navy_data$`Max New Acq`[[shipname]] <-
    (current$ship_stats_full$`Max Build Per Year`[[shipname]] - current$navy_data$`Planned Work`[[shipname]]) *
    current$ship_stats_full$`Acquisition Cost`[[shipname]]
  
  # update max cut acq
  current$navy_data$`Max Cut Acq`[[shipname]] <-
    current$navy_data$`Planned Work`[[shipname]] *
    current$ship_stats_full$`Acquisition Cost`[[shipname]]
  
  # all the O&S statistics
  current$ship_stats_full$`Direct Cost`[[shipname]] <-
    input[[paste0(shipname_abbr, "_d_os")]]
  current$ship_stats_full$`Indirect Cost`[[shipname]] <-
    input[[paste0(shipname_abbr, "_i_os")]]
  current$ship_stats_full$`Overhead Cost`[[shipname]] <-
    input[[paste0(shipname_abbr, "_o_os")]]
  
  current$ship_stats_full$`Total Cost`[[shipname]] <-
    input[[paste0(shipname_abbr, "_d_os")]] +
    input[[paste0(shipname_abbr, "_i_os")]] +
    input[[paste0(shipname_abbr, "_o_os")]]
  
  current$ship_stats_full$`Yearly O&S`[[shipname]] <-
    current$ship_stats_full$`Total Cost`[[shipname]]
  
  current$navy_data$`O&S Yearly`[[shipname]] <-
    current$ship_stats_full$`Total Cost`[[shipname]]
  
  # max new O&S
  current$navy_data$`Max New O&S`[[shipname]] <-
    update_max_new_OS(current, shipname)
  
  # all the personnel statistics
  current$ship_stats_full$`Direct Personnel`[[shipname]] <-
    input[[paste0(shipname_abbr, "_d_p")]]
  current$ship_stats_full$`Indirect Personnel`[[shipname]] <-
    input[[paste0(shipname_abbr, "_i_p")]]
  current$ship_stats_full$`Overhead Personnel`[[shipname]] <-
    input[[paste0(shipname_abbr, "_o_p")]]
  current$ship_stats_full$`Total Personnel`[[shipname]] <-
    input[[paste0(shipname_abbr, "_d_p")]] +
    input[[paste0(shipname_abbr, "_i_p")]] +
    input[[paste0(shipname_abbr, "_o_p")]]
  
}

# ================================================================================
# Popover.template

# general function for the hover function

Popover.template <-
  function(Abbreviated.Name,
           Reference.Number,
           FullName,
           Nickname,
           Purpose,
           imagename,
           session) {
    a <- MasterFile[13, Reference.Number]
    
    if (a == 0) {
      a <- "NA"
    }
    addPopover(
      session,
      Abbreviated.Name,
      title = NA,
      content = HTML(
        paste0(
          "<br/>",
          "<div align = 'center'",
          '<h3><b>',
          FullName,
          '</b></h3>',
          "<br/>",
          Nickname,
          "<img src = " ,
          imagename,
          " alt='', style='width:250px;height:auto;'>",
          "<div align = 'left'>",
          Purpose,
          "<br/>",
          "<b> Acquisition cost: </b>",
          "$",
          format_ylab(MasterFile[1, Reference.Number]),
          "<br/>",
          "<b> Annual O&S cost: </b>",
          "$",
          format_ylab(MasterFile[2, Reference.Number]),
          "<br/>",
          "<b> Default Max Build Per Year: </b>",
          "",
          MasterFile[4, Reference.Number],
          "<br/>",
          "<br/>",
          "<b> Direct Cost: </b>",
          "$",
          format_ylab(MasterFile[5, Reference.Number]),
          "<br/>",
          "<b> Indirect Cost: </b>",
          "$",
          "",
          format_ylab(MasterFile[6, Reference.Number]),
          "<br/>",
          "<b> Overhead Cost: </b>",
          "$",
          "",
          format_ylab(MasterFile[7, Reference.Number]),
          "<br/>",
          "<b> Total Cost: </b>",
          "",
          "$",
          format_ylab(MasterFile[8, Reference.Number]),
          "<br/>",
          "<br/>",
          "<b> Direct Personnel: </b>",
          "",
          MasterFile[9, Reference.Number],
          "<br/>",
          "<b> Indirect Personnel: </b>",
          "",
          MasterFile[10, Reference.Number],
          "<br/>",
          "<b> Overhead Personnel: </b>",
          "",
          MasterFile[11, Reference.Number],
          "<br/>",
          "<b> Total Personnel: </b>",
          "",
          MasterFile[12, Reference.Number],
          "<br/>",
          "<br/>",
          "<b> Speed (knots): </b>",
          a,
          "",
          "<br/>",
          "<b> Tonnage (lbs): </b>",
          MasterFile[14, Reference.Number],
          "",
          "<br/>",
          "<b> Aircraft Capacity: </b>",
          "",
          MasterFile[15, Reference.Number],
          "<br/>",
          "<b> VLS Tubes: </b>",
          "",
          MasterFile[16, Reference.Number],
          "<br/>",
          "<b> Crew: </b>",
          "",
          MasterFile[17, Reference.Number],
          "<br/>",
          "<b> Officers: </b>",
          "",
          MasterFile[18, Reference.Number],
          "<br/>",
          "<b> Enlisted: </b>",
          "",
          MasterFile[19, Reference.Number],
          "<br/>",
          "<br/>"
        )
      ),
      placement = "right",
      trigger = 'hover'
    )
  }

# ================================================================================
