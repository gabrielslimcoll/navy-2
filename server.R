# ================================================================================
# Build your own Navy 2.0
# Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey
# --------------------------------------------------------------------------------
# server 
# build your own Navy and see the implications of your shipbuilding plan
# ================================================================================

# load packages ------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(extrafont)
library(extrafontdb)
library(magrittr)
library(scales)

# load R files -------------------------------------------------------------------

source("app_functions.R")
source("module_functions.R")

# load data ----------------------------------------------------------------------

MasterFile = read.csv("data/ship_info.csv", header = TRUE)

fields <- c(
  "Nimitz",
  "Ford",
  "Ticonderoga",
  "Burke",
  "BurkeIII",
  "Zumwalt",
  "FutureLSC",
  "Avenger",
  "LittoralCombatShip",
  "FastFrigate",
  "FutureSSC",
  "LosAngeles",
  "LosAngelesi",
  "Seawolf",
  "Virginia",
  "Virginiai",
  "OhioSSGN",
  "OhioSSBN",
  "Columbia",
  "WhidbeyIsland",
  "Wasp",
  "HarpersFerry",
  "SanAntonio",
  "America",
  "LXR",
  "CLF",
  "SupportVessels",
  "text",
  "text2"
)

# ================================================================================
# begin server 

shinyServer(function(input, output, session) {

  # --------------------------------------------------------------------------------
  # read data and create reactive current$ships frame

  load("data/navy_data_full.Rda")
  load("data/ship_stats_full.Rda")
  
  current <- reactiveValues(
    ships = navy_data[["Fleet Plan"]],
    ships_retire = navy_data[["Fleet Plan"]],
    navy_data = navy_data,
    ship_stats_full = ship_stats_full
  )

  # --------------------------------------------------------------------------------
  # keep(change) or cancel current$ships based on used input

  # ================================================================================
  # Current ships

  # --------------------------------------------------------------------------------
  # Aircraft Carriers
  
  callModule(
    OldShipNoSlider,
    "Nimitz",
    current,
    shipname = "Nimitz",
    FullName = "Nimitz class",
    NickName = "CVN",
    Purpose = " ",
    imagename = 'Nimitz2.png'
  )
  
  callModule(
    OldShipSlider,
    "Ford",
    current,
    shipname = "Ford",
    FullName = "Ford class",
    NickName = "CVN",
    Purpose = " ",
    imagename = 'Ford2.png'
  )
  
  # Large Surface Combatants
  callModule(
    OldShipNoSlider,
    "Ticonderoga",
    current,
    shipname = "Ticonderoga",
    FullName = "Ticonderoga class",
    NickName = "CG",
    Purpose = " ",
    imagename = 'Ticonderoga2.png'
  )
  
  callModule(
    OldShipSlider,
    "Burke",
    current,
    shipname = "Burke",
    FullName = "Burke class (Flight I, II)",
    NickName = "DDG",
    Purpose = " ",
    imagename = 'Burke2.png'
  )
  
  callModule(
    OldShipSlider,
    "BurkeIII",
    current,
    shipname = "BurkeIII",
    FullName = "Burke class (Flight III)",
    NickName = "DDG",
    Purpose = " ",
    imagename = 'Burke2.png'
  )
  
  callModule(
    OldShipSlider,
    "Zumwalt",
    current,
    shipname = "Zumwalt",
    FullName = "Zumwalt class",
    NickName = "DDG",
    Purpose = " ",
    imagename = 'Zumwalt2.png'
  )
  
  callModule(
    OldShipSlider,
    "FutureLSC",
    current,
    shipname = "FutureLSC",
    FullName = "Future Large Surface Combatant",
    NickName = "",
    Purpose = " ",
    imagename = 'space.png'
  )
  
  # --------------------------------------------------------------------------------
  # Small Surface Combatants
  
  callModule(
    OldShipNoSlider,
    "Avenger",
    current,
    shipname = "Avenger",
    FullName = "Avenger class",
    NickName = "MCM",
    Purpose = " ",
    imagename = 'Avenger4.png'
  )
  
  callModule(
    OldShipSlider,
    "LittoralCombatShip",
    current,
    shipname = "LittoralCombatShip",
    FullName = "Littoral Combat Ship",
    NickName = "",
    Purpose = " ",
    imagename = 'lcs.png'
  )
  
  callModule(
    OldShipSlider,
    "FastFrigate",
    current,
    shipname = "FastFrigate",
    FullName = "FFG(X)",
    NickName = "",
    Purpose = " ",
    imagename = 'space.png'
  )
  
  callModule(
    OldShipSlider,
    "FutureSSC",
    current,
    shipname = "FutureSSC",
    FullName = "Future Small Surface Combatant",
    NickName = "",
    Purpose = " ",
    imagename = 'space.png'
  )
  
  # --------------------------------------------------------------------------------
  # Attack Submarines
  
  callModule(
    OldShipNoSlider,
    "LosAngeles",
    current,
    shipname = "LosAngeles",
    FullName = "Los Angeles class",
    NickName = "SSN",
    Purpose = " ",
    imagename = 'LosAngeles2.png'
  )
  
  callModule(
    OldShipNoSlider,
    "LosAngelesi",
    current,
    shipname = "LosAngelesi",
    FullName = "Los Angeles class - improved",
    NickName = "SSN",
    Purpose = " ",
    imagename = 'LosAngeles2.png'
  )
  
  callModule(
    OldShipNoSlider,
    "Seawolf",
    current,
    shipname = "Seawolf",
    FullName = "Seawolf class",
    NickName = "SSN",
    Purpose = " ",
    imagename = 'Seawolf2.png'
  )
  
  callModule(
    OldShipSlider,
    "Virginia",
    current,
    shipname = "Virginia",
    FullName = "Virginia class",
    NickName = "SSN",
    Purpose = " ",
    imagename = 'Virginia2.png'
  )
  
  callModule(
    OldShipSlider,
    "Virginiai",
    current,
    shipname = "Virginiai",
    FullName = "Virginia class - improved",
    NickName = "SSN",
    Purpose = " ",
    imagename = 'Virginia2.png'
  )
  
  # --------------------------------------------------------------------------------
  # Cruise Missile Submarines
  
  callModule(
    OldShipNoSlider,
    "OhioSSGN",
    current,
    shipname = "OhioSSGN",
    FullName = "Ohio class(SSGN)",
    NickName = "SSGN",
    Purpose = " ",
    imagename = 'Ohio2.png'
  )
  
  # --------------------------------------------------------------------------------
  # Ballistic Missile Submarines
  
  callModule(
    OldShipNoSlider,
    "OhioSSBN",
    current,
    shipname = "OhioSSBN",
    FullName = "Ohio class(SSBN)",
    NickName = "SSBN",
    Purpose = " ",
    imagename = 'Ohio2.png'
  )
  
  callModule(
    OldShipSlider,
    "Columbia",
    current,
    shipname = "Columbia",
    FullName = "Columbia class",
    NickName = "SSBN",
    Purpose = " ",
    imagename = 'space.png'
  )
  
  # --------------------------------------------------------------------------------
  # Amphibious Warfare Ships
  
  callModule(
    OldShipNoSlider,
    "WhidbeyIsland",
    current,
    shipname = "WhidbeyIsland",
    FullName = "Whidbey Island class",
    NickName = "LSD",
    Purpose = " ",
    imagename = 'WhidbeyIsland2.png'
  )
  
  callModule(
    OldShipNoSlider,
    "Wasp",
    current,
    shipname = "Wasp",
    FullName = "Wasp class",
    NickName = "LHD",
    Purpose = " ",
    imagename = 'Wasp2.png'
  )
  
  callModule(
    OldShipNoSlider,
    "HarpersFerry",
    current,
    shipname = "HarpersFerry",
    FullName = "Harpers Ferry class",
    NickName = "LSD",
    Purpose = " ",
    imagename = 'HarpersFerry2.png'
  )
  
  callModule(
    OldShipSlider,
    "SanAntonio",
    current,
    shipname = "SanAntonio",
    FullName = "San Antonio class",
    NickName = "LPD",
    Purpose = " ",
    imagename = 'SanAntonio2.png'
  )
  
  callModule(
    OldShipSlider,
    "America",
    current,
    shipname = "America",
    FullName = "America class",
    NickName = "LHA",
    Purpose = " ",
    imagename = 'America2.png'
  )
  
  callModule(
    OldShipSlider,
    "LXR",
    current,
    shipname = "LXR",
    FullName = "LX(R)",
    NickName = "",
    Purpose = " ",
    imagename = 'space.png'
  )
  
  # --------------------------------------------------------------------------------
  # Logistics and Support
  
  callModule(
    LS,
    "CLF",
    current,
    shipname = "CLF",
    FullName = "Combat Logistics Force",
    NickName = "",
    Purpose = " ",
    imagename = 'LewisandClarke2.png'
  )
  
  callModule(
    LS,
    "SupportVessels",
    current,
    shipname = "SupportVessels",
    FullName = "Support Vessels",
    NickName = "",
    Purpose = " ",
    imagename = 'Emory2.png'
  )

# ================================================================================
# Future ships    

  callModule(NewShipModule, "CVN-X", current, shipname = "CVN-X")
  callModule(NewShipModule, "Cruiser-X", current, shipname = "Cruiser-X")
  callModule(NewShipModule, "Destroyer-X", current, shipname = "Destroyer-X")
  callModule(NewShipModule, "Frigate-X", current, shipname = "Frigate-X")
  callModule(NewShipModule, "SSN-X", current, shipname = "SSN-X")
  callModule(NewShipModule, "SSGN-X", current, shipname = "SSGN-X")
  callModule(NewShipModule, "SSBN-X", current, shipname = "SSBN-X")
  callModule(NewShipModule, "AWS-X", current, shipname = "AWS-X")
  
# -------------------------------------------------------------------------------- 
# update current$ship_stats_full using numbers in bsModal window
  
  # Current ships
  callModule(OldShipbsModal, "Nimitz", current, shipname = "Nimitz")
  callModule(OldShipbsModal, "Ford", current, shipname = "Ford")
  
  callModule(OldShipbsModal, "Ticonderoga", current, shipname = "Ticonderoga")
  callModule(OldShipbsModal, "Burke", current, shipname = "Burke")
  callModule(OldShipbsModal, "BurkeIII", current, shipname = "BurkeIII")
  callModule(OldShipbsModal, "Zumwalt", current, shipname = "Zumwalt")
  callModule(OldShipbsModal, "FutureLSC", current, shipname = "FutureLSC")
  
  callModule(OldShipbsModal, "Avenger", current, shipname = "Avenger")
  callModule(OldShipbsModal, "LittoralCombatShip", current, shipname = "LittoralCombatShip")
  callModule(OldShipbsModal, "FastFrigate", current, shipname = "FastFrigate")
  callModule(OldShipbsModal, "FutureSSC", current, shipname = "FutureSSC")
  
  callModule(OldShipbsModal, "LosAngeles", current, shipname = "LosAngeles")
  callModule(OldShipbsModal, "LosAngelesi", current, shipname = "LosAngelesi")
  callModule(OldShipbsModal, "Seawolf", current, shipname = "Seawolf")
  callModule(OldShipbsModal, "Virginia", current, shipname = "Virginia")
  callModule(OldShipbsModal, "Virginiai", current, shipname = "Virginiai")
  
  callModule(OldShipbsModal, "OhioSSGN", current, shipname = "OhioSSGN")
  
  callModule(OldShipbsModal, "OhioSSBN", current, shipname = "OhioSSBN")
  callModule(OldShipbsModal, "Columbia", current, shipname = "Columbia")
  
  callModule(OldShipbsModal, "WhidbeyIsland", current, shipname = "WhidbeyIsland")
  callModule(OldShipbsModal, "Wasp", current, shipname = "Wasp")
  callModule(OldShipbsModal, "HarpersFerry", current, shipname = "HarpersFerry")
  callModule(OldShipbsModal, "SanAntonio", current, shipname = "SanAntonio")
  callModule(OldShipbsModal, "America", current, shipname = "America")
  callModule(OldShipbsModal, "LXR", current, shipname = "LXR")
  
  callModule(OldShipbsModal, "CLF", current, shipname = "CLF")
  callModule(OldShipbsModal, "SupportVessels", current, shipname = "SupportVessels")
  
  # Future ships
    callModule(newshipbsModal, "CVN-X", current, shipname = "CVN-X")
  callModule(newshipbsModal, "Cruiser-X", current, shipname = "Cruiser-X")
  callModule(newshipbsModal, "Destroyer-X", current, shipname = "Destroyer-X")
  callModule(newshipbsModal, "Frigate-X", current, shipname = "Frigate-X")
  callModule(newshipbsModal, "SSN-X", current, shipname = "SSN-X")
  callModule(newshipbsModal, "SSGN-X", current, shipname = "SSGN-X")
  callModule(newshipbsModal, "SSBN-X", current, shipname = "SSBN-X")
  callModule(newshipbsModal, "AWS-X", current, shipname = "AWS-X")
  
  # ================================================================================
  
  # --------------------------------------------------------------------------------
  # format data for use by ships plot
  
  ships_dataset <- reactive({
    return(
      get_ships_data(
        current$ships,
        navy_data,
        stats_df = current$ship_stats_full,
        input
      )
    )
  })
  
  
  # --------------------------------------------------------------------------------
  # create format for y-axis ($_B)
  
  formaty1 <- function(x) {
    x <- gsub("000", "", x)
    x <- gsub("500", ".5", x)
    x <- gsub("250", ".25", x)
    x <- gsub("750", ".75", x)
    paste("$", x, "B", sep = "")
  }
  
  output$ships <- renderPlot({
    ship_plot()
  })
  
  
  ship_plot <- reactive({
    shown <- ships_dataset()
    
    # deflate if requested and appropriate
    if (input$checkbox == TRUE & input$top_y %in% c("Direct Cost",
                                                    "Indirect Cost",
                                                    "Overhead Cost",
                                                    "Total Cost")) {
      shown <- deflate_frame(shown)
      
    }
    
    # find plot limits
    ymax <- max(c(max(shown$Total), signif(shown$Total, 2)))
    if (input$top_y == "Ships")
      ymax <- max(ymax, 375)
    ymin <- 0
    
    # options unique to line chart
    if (input$top_chart == "Line") {
      p <-
        ggplot(data = shown, aes(x = FY, y = Total, color = Category)) +
        geom_line(size = 1) +
        scale_color_manual(
          values = c("#C76363", "#788ca8"),
          labels = c("New Ship Plan ", "Old Ship Plan ")
        )
    }
    
    if (input$top_chart == "Area" &
        input$top_y %in% c("Speed (knots)",
                           "Tonnage (lbs)")) {
      shown <- filter(shown, Category == "new plan ships")
      p <-
        ggplot(data = shown, aes(x = FY, y = Total, color = Category)) +
        geom_area(alpha = .80,
                  color = "#4D7FA3",
                  fill = "#4D7FA3")
    } else if (input$top_chart == "Area" &
               (!input$top_y %in% c("Speed (knots)",
                                    "Tonnage (lbs)"))) {
      shown <- shown %>% filter(Category == "new plan ships") %>%
        select(-Category)
      
      shown <- gather(data = shown,
                      key = "Ship_type",
                      value = "Quantity",-FY,
                      -Total)
      
      shown$Ship_type <- factor(
        shown$Ship_type,
        c(
          "Nimitz",
          "Ford",
          "Ticonderoga",
          "Burke",
          "BurkeIII",
          "Zumwalt",
          "FutureLSC",
          "Avenger",
          "LittoralCombatShip",
          "FastFrigate",
          "FutureSSC",
          "LosAngeles",
          "LosAngelesi",
          "Seawolf",
          "Virginia",
          "Virginiai",
          "OhioSSGN",
          "OhioSSBN",
          "Columbia",
          "WhidbeyIsland",
          "Wasp",
          "HarpersFerry",
          "SanAntonio",
          "America",
          "LXR",
          "CLF",
          "SupportVessels",
          "CVN-X",
          "Cruiser-X",
          "Destroyer-X",
          "Frigate-X",
          "SSN-X",
          "SSGN-X",
          "SSBN-X",
          "AWS-X"
        )
      )
      
      if (input$AircraftCarriers == 1 |
          input$LSC == 1 |
          input$SSC == 1 |
          input$SSN == 1 | input$SSGN == 1 | input$SSBN == 1 |
          input$AWS == 1 | input$LogisticsSupport == 1) {
        p <-
          ggplot(data = shown, aes(
            x = FY,
            y = Quantity,
            fill = Ship_type
          )) +
          geom_area() +
          scale_fill_manual(
            values = c(
              "Nimitz" = "#4D7FA3",
              "Ford" = "#C74745",
              "Ticonderoga" = "#4D7FA3",
              "Burke" = "#C74745",
              "BurkeIII" = "#0E9E87",
              "Zumwalt" = "#566377",
              "FutureLSC" = "#53A2A6",
              "Avenger" = "#4D7FA3",
              "LittoralCombatShip" = "#C74745",
              "FastFrigate" = "#0E9E87",
              "FutureSSC" = "#566377",
              "LosAngeles" = "#4D7FA3",
              "LosAngelesi" = "#C74745",
              "Seawolf" = "#0E9E87",
              "Virginia" = "#566377",
              "Virginiai" = "#53A2A6",
              "OhioSSGN" = "#4D7FA3",
              "OhioSSBN" = "#C74745",
              "Columbia" = "#0E9E87",
              "WhidbeyIsland" = "#4D7FA3",
              "Wasp" = "#C74745",
              "HarpersFerry" = "#0E9E87",
              "SanAntonio" = "#566377",
              "America" = "#53A2A6",
              "LXR" = "#2F4D63",
              "CLF" = "#4D7FA3",
              "SupportVessels" = "#C74745",
              
              "CVN-X" = "#0E9E87",
              "Cruiser-X" = "#2F4D63",
              "Destroyer-X" = "#F2BC57",
              "Frigate-X" = "#53A2A6",
              "SSN-X" = "#2F4D63",
              "SSGN-X" = "#C74745",
              "SSBN-X" = "#566377",
              "AWS-X" = "#F2BC57"
            ),
            
            labels = c(
              "Nimitz" = "Nimitz ",
              "Ford" = "Ford ",
              "Ticonderoga" = "Ticonderoga ",
              "Burke" = "Burke I,II ",
              "BurkeIII" = "Burke III ",
              "Zumwalt" = "Zumwalt ",
              "FutureLSC" = "Future LSC ",
              "Avenger" = "Avenger ",
              "LittoralCombatShip" = "Littoral Combat Ship ",
              "FastFrigate" = "Fast Frigate ",
              "FutureSSC" = "Future SSC ",
              "LosAngeles" = "Los Angeles ",
              "LosAngelesi" = "Los Angeles (i) ",
              "Seawolf" = "Seawolf ",
              "Virginia" = "Virginia ",
              "Virginiai" = "Virginia (i) ",
              "OhioSSGN" = "Ohio (SSGN) ",
              "OhioSSBN" = "Ohio (SSBN) ",
              "Columbia" = "Columbia ",
              "WhidbeyIsland" = "Whidbey Island ",
              "Wasp" = "Wasp ",
              "HarpersFerry" = "Harpers Ferry ",
              "SanAntonio" = "San Antonio ",
              "America" = "America ",
              "LXR" = "LX(R) ",
              "CLF" = "Combat Logistics Force ",
              "SupportVessels" = "Support Vessels ",
              "CVN-X" = "CVN-X ",
              "Cruiser-X" = "Cruiser-X ",
              "Destroyer-X" = "Destroyer-X ",
              "Frigate-X" = "Frigate-X ",
              "SSN-X" = "SSN-X ",
              "SSGN-X" = "SSGN-X ",
              "SSBN-X" = "SSBN-X ",
              "AWS-X" = "AWS-X "
            )
          )
      } else{
        shown$Function[shown$Ship_type %in% Aircraft_Carriers] <-
          "Aircraft Carriers"
        shown$Function[shown$Ship_type %in% Large_Surface_Combatants] <-
          "Large Surface Combatants"
        shown$Function[shown$Ship_type %in% Small_Surface_Combatants] <-
          "Small Surface Combatants"
        shown$Function[shown$Ship_type %in% Attack_Submarines] <-
          "Attack Submarines"
        
        shown$Function[shown$Ship_type %in% Cruise_Missile_Submarines] <-
          "Cruise Missile Submarines"
        shown$Function[shown$Ship_type %in% Ballistic_Missile_Submarines] <-
          "Ballistic Missile Submarines"
        shown$Function[shown$Ship_type %in% Amphibious_Warfare_Ships] <-
          "Amphibious Warfare Ships"
        shown$Function[shown$Ship_type %in% Logistics_and_Support] <-
          "Logistics and Support"
        
        shown$Function <- factor(
          shown$Function,
          levels =
            c(
              "Aircraft Carriers",
              "Large Surface Combatants",
              "Small Surface Combatants",
              "Attack Submarines",
              
              "Cruise Missile Submarines",
              "Ballistic Missile Submarines",
              "Amphibious Warfare Ships",
              "Logistics and Support"
            )
        )
        
        shown <-
          shown %>% group_by(FY, Function) %>% mutate(Quantity = sum(Quantity)) %>% ungroup %>%
          select(FY, Total, Quantity, Function) %>% unique
        
        p <-
          ggplot(data = shown, aes(
            x = FY,
            y = Quantity,
            fill = Function
          )) +
          geom_area(alpha = .875) +
          scale_fill_manual(
            values = c(
              "Aircraft Carriers" = "#4D7FA3",
              "Large Surface Combatants" = "#C74745",
              "Small Surface Combatants" = "#0E9E87",
              "Attack Submarines" = "#566377",
              
              "Cruise Missile Submarines" = "#53A2A6",
              "Ballistic Missile Submarines" = "#2F4D63",
              "Amphibious Warfare Ships" = "#F2BC57",
              "Logistics and Support" = "#AEC3CC"
            ),
            
            labels = c(
              "Aircraft Carriers" = "Aircraft Carriers ",
              "Large Surface Combatants" = "Large Surface Combatants ",
              "Small Surface Combatants" = "Small Surface Combatants ",
              "Attack Submarines" = "Attack Submarines ",
              
              "Cruise Missile Submarines" = "Cruise Missile Submarines ",
              "Ballistic Missile Submarines" = "Ballistic Missile Submarines ",
              "Amphibious Warfare Ships" = "Amphibious Warfare Ships ",
              "Logistics and Support" = "Logistics and Support "
            )
          )
      }
    }
    
    # add options common to both views (Line/Area)
    p <- p + ggtitle(update_title(input)[1]) +
      
      scale_y_continuous(labels = (switch(
        input$top_y,
        "Ships" = format_ylab,
        "Direct Cost" = format_ylab2,
        "Indirect Cost" = format_ylab2,
        "Overhead Cost" = format_ylab2,
        "Total Cost" = format_ylab2,
        "Direct Personnel" = format_ylab,
        "Indirect Personnel" = format_ylab,
        "Overhead Personnel" = format_ylab,
        "Total Personnel" = format_ylab,
        "Crew" = format_ylab,
        "Officers" = format_ylab,
        "Enlisted" = format_ylab,
        "Aircraft Capacity" = format_ylab,
        "VLS Tubes" = format_ylab,
        "Speed (knots)" = format_ylab,
        "Tonnage (lbs)" = format_ylab
      ))) +
      
      scale_x_continuous(
        breaks = seq(2018, 2046, 2),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        nrow = 3 + (input$top_chart == "Area"),
        reverse = FALSE
      )) +
      xlab("Fiscal Year") +
      ylab(input$top_y)
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  
  # --------------------------------------------------------------------------------
  # format data for use by budget plot
  
  budget_dataset <- reactive({
    if (input$bottom_chart == "Change") {
      return(get_budget_change_data(current, input))
    } else if (input$bottom_chart == "Total") {
      return(get_budget_total_data(current, current$ships, current$navy_data, input))
    }
  })
  
  # --------------------------------------------------------------------------------
  # create budget plot
  
  output$budget <- renderPlot({
    budget_plot()
  })
  
  budget_plot <- reactive({
    shown <- budget_dataset()
    
    if (input$checkbox == TRUE) {
      shown %<>% deflate_frame()
    }
    
    # options specific to change chart
    if (input$bottom_chart == "Change") {
      shown$budget_cat <- factor(
        shown$budget_cat,
        levels = c(
          "Added O&S",
          "Added Acq",
          "Added R&D",
          "Cut O&S",
          "Cut Acq",
          "Cut R&D"
        )
      )
      
      p <- ggplot(data = shown, aes(x = FY, y = Total)) +
        geom_area(stat = 'identity',
                  alpha = .80,
                  aes(fill = budget_cat)) +
        ggtitle(update_title(input)[2]) +
        scale_fill_manual(
          values = c(
            "Added O&S" = "#115175",
            "Added Acq" = "#0a8672",
            "Added R&D" = "#F2A85A",
            "Cut R&D" = "#F2BC57",
            "Cut O&S" = "#788ca8",
            "Cut Acq" = "#0faa91"
          ),
          labels = c(
            "Added O&S" = "Added Operation & Support  ",
            "Added Acq" = "Added Acquisition  ",
            "Added R&D" = "Added R&D ",
            "Cut R&D" = "Decreased R&D",
            "Cut O&S" = "Decreased Operation & Support  ",
            "Cut Acq" = "Decreased Acquisition  "
          )
        )
      
      # add net change line and y limits
      netdata <- shown %>%
        group_by(FY) %>%
        summarize(Total = sum(Total))
      
      y_limits <- find_budget_y_limits(shown)
      
      p <- p +
        geom_line(data = netdata,
                  aes(color = "Net Change"),
                  size = 1) +
        scale_color_manual(values = "#C76363") +
        coord_cartesian(ylim = c(y_limits[1], y_limits[2]))
      coord_cartesian(ylim = y_limits[1])
    } else if (input$bottom_chart == "Total") {
      shown <- subset_budget_total(shown, input)
      
      y_limit <-
        max(shown$Total[shown$budget_cat == "Acquisition"]) +
        max(shown$Total[shown$budget_cat == "O&S"]) +
        max(shown$Total[shown$budget_cat == "R&D"])
      
      # if(y_limit > 80e9) y_limit <- y_limit * 1.1
      # else if(y_limit > 50e9) y_limit <- 80e9
      # else y_limit <- 50e9
      
      y_limits <- find_budget_y_limits(shown)
      
      p <- ggplot(data = shown, aes(x = FY, y = Total)) +
        geom_area(stat = 'identity',
                  alpha = .80,
                  aes(fill = budget_cat)) +
        ggtitle(update_title(input)[2]) +
        scale_fill_manual(
          values = c(
            "O&S" = "#115175",
            "Acquisition" = "#0a8672",
            "R&D" = "#F2BC57"
          ),
          labels = c(
            "O&S" = "Operations & Support  ",
            "Acquisition" = "Acquisition  ",
            "R&D" = "+Research & Development"
          )
        ) +
        coord_cartesian(ylim = c(0, y_limits[2]))
    }
    
    # options applied to both charts
    p <- p +
      scale_y_continuous(labels = format_ylab2) +
      scale_x_continuous(
        # breaks = c(shown$FY[1]:shown$FY[nrow(shown)]),
        breaks = seq(2018, 2046, 2),
        labels = function(x) {
          substring(as.character(x), 3, 4)
        }
      ) +
      guides(fill = guide_legend(
        keywidth = 1,
        keyheight = 1,
        # nrow= 3 + (input$bottom_chart == "Change"),
        nrow = 1 + (input$bottom_chart == "Change"),
        reverse = FALSE
      )) +
      xlab("Fiscal Year") +
      ylab("Constant FY18 Dollars") +
      
      
      theme(plot.caption = element_text(
        size = 12,
        face = "bold",
        color = "#554449",
        family = "Open Sans"
      )) +
      labs(caption = "Source: USN; CBO; GAO; CSIS analysis",
           size = 30,
           family = "Open Sans")
    
    p <- add_diigtheme(p)
    
    return(p)
  })
  
  # --------------------------------------------------------------------------------
  # hover feature for ships plot
  
  output$hover_info_ships <- renderUI({
    hover <- input$plot_hover_ships
    shown <- ships_dataset()
    
    if (is.null(hover))
      return(NULL)
    
    hover_year <- round(hover$x)
    
# --------------------------------------------------------------------------------
    
    # note: limit hover year to between 2018 and 2046 (otherwise hover_year can 
    # ...take values 2017 and 2047 even if these two years don't exist in the 
    # ...data)
    
    if ((hover_year < 2018) | hover_year > 2046)
      return(NULL)
    
# --------------------------------------------------------------------------------
    
    hover_year_index <- which(shown$FY == hover_year)[1]
    
    # shown = subset_aircraft_top_hover(input,shown)
    
    hover_ships_new <- shown %>%
      filter(Category == "new plan ships") %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    hover_ships_old <- shown %>%
      select(Total) %>%
      slice(hover_year_index) %>%
      unlist
    
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)

    # use HTML/CSS to change style of tooltip panel here
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0.90); ",
      "left:",
      left_px - 275,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    new_plan <- shown %>%
      filter(FY == hover_year & Category == "new plan ships") %>%
      select(-FY,-Total,-Category)
    
    
    if (input$AircraftCarriers != 1 &
        input$LSC != 1 &
        input$SSC != 1 &
        input$SSN != 1 & input$SSGN != 1 & input$SSBN != 1 &
        input$AWS != 1 & input$LogisticsSupport != 1) {
      
      # rearrange the columns in new_plan
      new_plan <- select(new_plan, one_of(All), everything())
      
      for (i in 1:length(All_list)) {
        col_head <- min(which(names(new_plan) %in% All_list[[i]]))
        col_tail <- max(which(names(new_plan) %in% All_list[[i]]))
        new_plan[names(All_list)[i]] <-
          rowSums(new_plan[, col_head:col_tail])
      }

      new_plan <- new_plan[, names(All_list)]
    }
    
    tooltip_string <- HTML(paste0(sapply(names(new_plan),
                                         function(shipname) {
                                           return(
                                             paste0(
                                               "<b>",
                                               shipname,
                                               "</b>",
                                               ": ",
                                               as.character(prettyNum(round(new_plan[[shipname]]), big.mark  = ",")),
                                               "&nbsp",
                                               "&nbsp",
                                               "&nbsp",
                                               "(",
                                               percent(round(new_plan[[shipname]] / hover_ships_new, 5)),
                                               ")"
                                             )
                                           )
                                         }),
                                  collapse = "<br>"))
    
    if (input$top_y == "Direct Cost" |
        input$top_y == "Indirect Cost" |
        input$top_y == "Overhead Cost" | input$top_y == "Total Cost") {
      tooltip_string <- HTML(paste0(sapply(names(new_plan),
                                           function(shipname) {
                                             return(
                                               paste0(
                                                 "<b>",
                                                 shipname,
                                                 "</b>",
                                                 ": ",
                                                 "<span style=font-weight: normal>",
                                                 format_ylab2(new_plan[[shipname]]),
                                                 "</span>",
                                                 
                                                 "&nbsp",
                                                 "&nbsp",
                                                 "&nbsp",
                                                 "(",
                                                 percent(round(new_plan[[shipname]] / hover_ships_new, 5)),
                                                 ")"
                                               )
                                             )
                                           }),
                                    collapse = "<br>"))
    }
    
    if (input$top_y == "Direct Cost" |
        input$top_y == "Indirect Cost" |
        input$top_y == "Overhead Cost" | input$top_y == "Total Cost") {
      hover_ships_new <- format_ylab2(hover_ships_new)
      hover_ships_old <- format_ylab2(hover_ships_old)
    } else{
      hover_ships_new <- prettyNum(round(hover_ships_new), big.mark = ",")
      hover_ships_old <- prettyNum(hover_ships_old, big.mark = ",")
    }
    
    
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<div align = 'left'>",
                  "<b><u>",
                  input$top_y,
                  " in FY",
                  hover_year,
                  ":</b></u><br>",
                  br(),
                  "<b> Old Plan: ",
                  "</b>",
                  hover_ships_old,
                  
                  br(),
                  "<b> New Plan: ",
                  "</b>",
                  hover_ships_new,
                  
                  br(),
                  "<br/>",
                  
                  if (!input$top_y %in% c(# "Aircraft Capacity",
                    # "VLS Tubes",
                    "Speed (knots)",
                    "Tonnage (lbs)")) {
                    br()
                    br()
                    tooltip_string
                  } else{
                    br()
                    br()
                    
                    
                    if (input$AircraftCarriers == 1 |
                        input$LSC == 1 |
                        input$SSC == 1 |
                        input$SSN == 1 | input$SSGN == 1 | input$SSBN == 1 |
                        input$AWS == 1 | input$LogisticsSupport == 1) {
                      HTML(paste0(sapply(names(new_plan),
                                         function(shipname) {
                                           return(paste0(
                                             "<b>",
                                             shipname,
                                             "</b>",
                                             ": ",
                                             as.character(
                                               prettyNum(current$ship_stats_full[[input$top_y]][[shipname]],
                                                         big.mark  = ",")
                                             )
                                           ))
                                         }),
                                  collapse = "<br>"))
                      
                    } else{
                      new_plan_performance <- current$ship_stats_full[[input$top_y]]
                      new_plan_performance <-
                        select(new_plan_performance, one_of(All), everything())
                      
                      col <- which(new_plan_performance[1, ] == 0)
                      
                      new_plan_ships <-
                        select(filter(current$ships, FY == hover_year), -FY)
                      new_plan_ships <-
                        select(new_plan_ships, one_of(All), everything())
                      
                      
                      if (length(col) != 0) {
                        new_plan_ships[, col] = 0
                      }
                      
                      for (i in 1:length(All_list)) {
                        col_head <-
                          min(which(names(new_plan_performance) %in% All_list[[i]]))
                        col_tail <-
                          max(which(names(new_plan_performance) %in% All_list[[i]]))
                        
                        new_plan_performance[names(All_list)[i]] <-
                          new_plan[[names(All_list)[i]]] /
                          rowSums(new_plan_ships[, col_head:col_tail])
                      }
                      
                      new_plan_performance <-
                        new_plan_performance[, names(All_list)]
                      
                      HTML(paste0(sapply(names(new_plan_performance),
                                         function(shipname) {
                                           return(paste0(
                                             "<b>",
                                             shipname,
                                             "</b>",
                                             ": ",
                                             as.character(prettyNum(
                                               new_plan_performance[[shipname]],
                                               big.mark  = ","
                                             ))
                                           ))
                                         }),
                                  collapse = "<br>"))
                      
                    }
                  }
                )
              )))
  })
  
# --------------------------------------------------------------------------------
# hover feature for budget plot

  output$hover_info_budget <- renderUI({
    hover <- input$plot_hover_budget
    
    if (is.null(hover)) {
      return()
    }
    
    hover_year <- round(hover$x)
    
    # --------------------------------------------------------------------------------
    
    # note: limit hover year to between 2018 and 2046 (otherwise hover_year can take 
    # ...values 2017 and 2047 even if these two years don't exist in the data)
    
    if ((hover_year < 2018) | hover_year > 2046)
      return(NULL)
    
    # --------------------------------------------------------------------------------
    
    # note: calculate point position INSIDE the image as percent of total dimensions
    # ...from left (horizontal) and from top (vertical)
    
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    
    # Use HTML/CSS to change style of tooltip panel here
    
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0.90); border-color: none;",
      "left:",
      left_px - 225,
      "px; top:",
      top_px + 2,
      "px;"
    )
    
    if (input$bottom_chart == "Change") {
      wellPanel(style = style,
                p(HTML(
                  create_change_tip(hover_year, budget_dataset())
                )))
    } else if (input$bottom_chart == "Total") {
      wellPanel(style = style,
                # p(HTML(create_total_tip(hover_year, budget_dataset())))
                p(HTML(
                  create_total_tip(
                    hover_year,
                    subset_budget_total(budget_dataset(), input)
                  )
                )))
    }
  })
  
  # --------------------------------------------------------------------------------
  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })

  # --------------------------------------------------------------------------------
  # disable feature 

  observeEvent(input$AircraftCarriers, ({
    # updateButton(session, "AircraftCarriers", disabled = input$AircraftCarriers)
    updateButton(session, "LSC", disabled = input$AircraftCarriers)
    updateButton(session, "SSC", disabled = input$AircraftCarriers)
    updateButton(session, "SSN", disabled = input$AircraftCarriers)
    updateButton(session, "SSGN", disabled = input$AircraftCarriers)
    updateButton(session, "SSBN", disabled = input$AircraftCarriers)
    updateButton(session, "AWS", disabled = input$AircraftCarriers)
    updateButton(session, "LogisticsSupport", disabled = input$AircraftCarriers)
  }))
  
  observeEvent(input$LSC, ({
    updateButton(session, "AircraftCarriers", disabled = input$LSC)
    # updateButton(session, "LSC", disabled = input$AircraftCarriers)
    updateButton(session, "SSC", disabled = input$LSC)
    updateButton(session, "SSN", disabled = input$LSC)
    updateButton(session, "SSGN", disabled = input$LSC)
    updateButton(session, "SSBN", disabled = input$LSC)
    updateButton(session, "AWS", disabled = input$LSC)
    updateButton(session, "LogisticsSupport", disabled = input$LSC)
  }))
  
  observeEvent(input$SSC, ({
    updateButton(session, "AircraftCarriers", disabled = input$SSC)
    updateButton(session, "LSC", disabled = input$SSC)
    # updateButton(session, "SSC", disabled = input$SSC)
    updateButton(session, "SSN", disabled = input$SSC)
    updateButton(session, "SSGN", disabled = input$SSC)
    updateButton(session, "SSBN", disabled = input$SSC)
    updateButton(session, "AWS", disabled = input$SSC)
    updateButton(session, "LogisticsSupport", disabled = input$SSC)
  }))
  
  observeEvent(input$SSN, ({
    updateButton(session, "AircraftCarriers", disabled = input$SSN)
    updateButton(session, "LSC", disabled = input$SSN)
    updateButton(session, "SSC", disabled = input$SSN)
    # updateButton(session, "SSN", disabled = input$SSN)
    updateButton(session, "SSGN", disabled = input$SSN)
    updateButton(session, "SSBN", disabled = input$SSN)
    updateButton(session, "AWS", disabled = input$SSN)
    updateButton(session, "LogisticsSupport", disabled = input$SSN)
  }))
  
  observeEvent(input$SSGN, ({
    updateButton(session, "AircraftCarriers", disabled = input$SSGN)
    updateButton(session, "LSC", disabled = input$SSGN)
    updateButton(session, "SSC", disabled = input$SSGN)
    updateButton(session, "SSN", disabled = input$SSGN)
    # updateButton(session, "SSGN", disabled = input$SSGN)
    updateButton(session, "SSBN", disabled = input$SSGN)
    updateButton(session, "AWS", disabled = input$SSGN)
    updateButton(session, "LogisticsSupport", disabled = input$SSGN)
  }))
  observeEvent(input$SSBN, ({
    updateButton(session, "AircraftCarriers", disabled = input$SSBN)
    updateButton(session, "LSC", disabled = input$SSBN)
    updateButton(session, "SSC", disabled = input$SSBN)
    updateButton(session, "SSN", disabled = input$SSBN)
    updateButton(session, "SSGN", disabled = input$SSBN)
    # updateButton(session, "SSBN", disabled = input$SSBN)
    updateButton(session, "AWS", disabled = input$SSBN)
    updateButton(session, "LogisticsSupport", disabled = input$SSBN)
  }))
  
  observeEvent(input$AWS, ({
    updateButton(session, "AircraftCarriers", disabled = input$AWS)
    updateButton(session, "LSC", disabled = input$AWS)
    updateButton(session, "SSC", disabled = input$AWS)
    updateButton(session, "SSN", disabled = input$AWS)
    updateButton(session, "SSGN", disabled = input$AWS)
    updateButton(session, "SSBN", disabled = input$AWS)
    # updateButton(session, "AWS", disabled = input$AWS)
    updateButton(session, "LogisticsSupport", disabled = input$AWS)
  }))
  
  observeEvent(input$LogisticsSupport, ({
    updateButton(session, "AircraftCarriers", disabled = input$LogisticsSupport)
    updateButton(session, "LSC", disabled = input$LogisticsSupport)
    updateButton(session, "SSC", disabled = input$LogisticsSupport)
    updateButton(session, "SSN", disabled = input$LogisticsSupport)
    updateButton(session, "SSGN", disabled = input$LogisticsSupport)
    updateButton(session, "SSBN", disabled = input$LogisticsSupport)
    updateButton(session, "AWS", disabled = input$LogisticsSupport)
    # updateButton(session, "LogisticsSupport", disabled = input$LogisticsSupport)
  }))
  
  observeEvent(input$submit, {
    email_string <- ifelse(input$text == "", "None", input$text)
    file <- file.path("responses",
                      paste0(email_string,
                             format(Sys.time(), '_%Y%m%d_%H%M%S'),
                             ".csv"))
    output_frame <- current$ships %>%
      filter(FY == max(FY))
    output_frame$email <- email_string
    output_frame$comments <-
      ifelse(input$text2 == "", "None", input$text2)
    
    write_csv(output_frame, file)
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Success!')
    
    
  })
  
  output$description <- renderUI({
    HTML(
      "<div align = 'center'>",
      "<b> Build your own Navy </b>",
      "<hr>",
      # "<br/>",
      "<h5><b> Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey </b></h4>",
      "<hr>",
      # "<br/>",
      "</div>",
      "<div align = 'left'>",
      "<h5> We designed this app to give our users the ability to build their own Navy force structure. In seconds, you can see the impact of the Navy's plan, as well as the impact of thousands of different plans. The app allows you to build more ships, retire existing ships, develop new ships, and change the build rate. And this goes far beyond the number of ships. We've included acquisition cost, operations and support costs, personnel numbers, aircraft capacity, and much more.</h5>",
      "<h5> The Navy faces the challenge of how to allocate resources between operating existing fleets, maintaining shipyard production lines, and developing new capabilities. These decisions involve many stakeholders, including leaders within Congress, the White House, industry, and the Department of Defense.</h5>",
      "<h5> Unlike in many private sector industries, stakeholders in the public sector do not share access to tools that make it easy to understand and plan for investments. While key stakeholders can hold different and sometimes competing perspectives, there are many areas of common ground. But progress requires more than a shared vision. It also relies on developing a better understanding of our current plans and the potential impact of new investments.</h5>",
      "<h5> The impact of potential government investments is generally evaluated in terms of financial costs. And the list of investment options that get evaluated is quite short, limited mainly to legislative proposals or agency recommendations.</h5>",
      "<h5> We rely primarily on the Congressional Budget Office for these types of assessments, who's role is to provide independent analysis of the budgetary and economic impact that public polices may have.</h5>",
      "<h5> Navy shipbuilding, a major military program that requires substantial planning and massive investments, is one of the key areas that CBO examines. When the Navy releases its annual thirty-year shipbuilding plan, the CBO estimates the financial costs of the plan. The final CBO report provides a helpful resource for Congress and the public to understand the Navy's intentions and to evaluate its budgetary implications. But why stop there?</h5>",
      "<h5> First, we should more comprehensively evaluate government investments to include other factors such as personnel requirements, the impact on jobs, and the implications on capacity or capabilities. Second, we should have the ability to evaluate a longer list of investment options and to compare those plans against each other. This app is designed to do just that for Navy ships, but it is only a small step in that direction.</h5>",
      "<h5> Our sources include: the U.S. Navy, Congressional Budget Office, Government Accountability Office, Naval Graphics, Naval Vessel Register, Office of Management and Budget, and Selected Acquisition Reports.</h5>",
      "</div>"
    )
  })
  
  # --------------------------------------------------------------------------------
  
  output$bottom_line <- renderUI({
    HTML(
      paste0(
        "<div align = 'left'>",
        "<h5> For more on our work, contact <a href='mailto:gcoll@csis.org?subject=Build%20your%20own%20Navy'>Gabriel Coll</a>. This is an ongoing project, and we are designing and building a new force structure app, so we welcome any recommendations.</h4>",
        "<h5> Learn more about our <a href=https://docs.google.com/spreadsheets/d/1VScFPIKcwriQ3pVOaG6a8TNRZb8kXioGTdHKSmjHt3c/edit?usp=sharing>numbers and sources</a> here, and find our apps on <a href=https://defense360.csis.org>Defense360</a>.</h4>"
      )
    )
  })
  
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("main-panel")
    
    session$onSessionEnded(stopApp)
    
  })
})

# ================================================================================