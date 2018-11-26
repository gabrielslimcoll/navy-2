# ================================================================================
# Build your own: Navy +  
# Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey
# --------------------------------------------------------------------------------
# module functions 
# build your own Navy and see the implications of your shipbuilding plan
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(extrafont)
library(extrafontdb)

# --------------------------------------------------------------------------------
# load data

load("data/navy_data_full.Rda")
load("data/ship_stats_full.Rda")
MasterFile = read.csv("data/ship_info.csv", header = TRUE)
newshipstats <-
  read.csv("data/new_ship_stats.csv",
           header = TRUE,
           stringsAsFactors = FALSE)

# --------------------------------------------------------------------------------
# begin functions

default_ships <- filter(navy_data[["Fleet Plan"]], FY == max(FY))
min_ships <-
  default_ships - filter(navy_data[["Max Cut Builds"]], FY == max(FY))
max_ships <-
  default_ships + filter(navy_data[["Max New Builds"]], FY == max(FY))
ship_stats_full <- ship_stats_full[5:length(ship_stats_full)]
colnames(newshipstats)[-1] <-
  c(
    "CVN-X",
    "Cruiser-X",
    "Destroyer-X",
    "Frigate-X",
    "SSN-X",
    "SSGN-X",
    "SSBN-X",
    "AWS-X"
  )

# ================================================================================
# 1st module type: OldShipNoSliderUI

# note: existing ships with no slider, i.e. Nimitz,Ticonderoga,Zumwalt,Avenger

OldShipNoSliderUI <-
  function(id,
           labelname,
           Nickname,
           Purpose,
           imagename,
           session) {
    ns <- NS(id)
    
    tagList(
      bsButton(
        inputId = ns("b"),
        label = strong(labelname),
        style = "basic",
        value = 1,
        type = "toggle",
        size = "small",
        block = TRUE,
        disabled = FALSE
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("b"), "'] == 1"),
        
        br(),
        
        fluidRow(
          conditionalPanel(
            condition = paste0("input['", ns("cancel"), "'] == 'Retire'"),
            
            column(3,
                   hidden(
                     sliderInput(
                       ns("Percent"),
                       "",
                       value = 1,
                       min = 0,
                       max = 1,
                       step = 0.01,
                       ticks = FALSE
                     )
                   ))
          ),
          
          conditionalPanel(condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
                           column(3, " ")),
          
          column(6,
                 radioButtons(
                   ns("cancel"),
                   "",
                   c("Keep", "Retire"),
                   selected = "Keep",
                   inline = TRUE
                 )),
          
          bsTooltip(
            ns("Yr"),
            "Set the retirement year of the fleet",
            "top",
            options = list(container = "body")
          ),
          bsTooltip(
            ns("Percent"),
            "Set the fraction of the fleet to retire",
            "top",
            options = list(container = "body")
          ),
          
          conditionalPanel(condition = paste0(
            "input['", ns("cancel"), "'] != 'Nothing'"
          ),
          column(2)),
          
          conditionalPanel(
            condition = paste0("input['", ns("cancel"), "'] != 'Nothing'"),
            column(1,
                   actionLink(
                     inputId = ns("c"),
                     label = strong(" "),
                     icon = icon("bars")
                   ))
          )
        ),
        
        
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
          hidden(
            sliderInput(
              inputId = ns("slider"),
              label = " ",
              value = unlist(default_ships[id]),
              min = unlist(min_ships[id]),
              max = unlist(max_ships[id]),
              step = 1,
              width = '90%',
              ticks = FALSE
            )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] == 'Retire'"),
          
          # column(3,
          sliderInput(
            ns("Yr"),
            "",
            value = 2019,
            min = 2018,
            max = 2046,
            width = '90%',
            ticks = FALSE,
            sep = ""
          )
        )
      ),
      
      bsTooltip(
        ns("slider"),
        "Set the inventory level for 2046",
        "top",
        options = list(container = "body")
      ),
      
      bsTooltip(
        ns("c"),
        "Change cost assumptions",
        "left",
        options = list(container = "body")
      )
    )
  }

# --------------------------------------------------------------------------------
# OldShipNoSlider

# note: before adding ignoreInit = TRUE, function "update_current_ship" runs when
# ... ObserveEvent is *created* but actually we only need to execute the
# ... handlerExpr(which in this case is to update reactiveValues according to user
# ... input) when user *changes* the inputs; in other words, we want the
# ... observeEvent() to be lazily evaluated

OldShipNoSlider <-
  function(input,
           output,
           session,
           mydata,
           shipname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$cancel
      input$Yr
      input$Percent
      input$slider
    },
    
    # note: in order to prevent the app from crashing when user type numbers
    # ...into the "numericInput" box, add "validate()" function to make sure
    # ...the two numericInputs are actually numbers and are in the right
    # ...retirement range
    
    # validate(
    #   need(is.numeric(input$Yr), "Please input a number"),
    #   need(input$Yr <= 2046 & input$Yr >= 2018, "Please input a number between 2018 and 2046"),
    #   need(is.numeric(input$Percent), "Please input a number"),
    #   need(input$Percent <= 1 & input$Percent >= 0, "Please input a number between 0 and 1")
    # )
    
    {
      if (input$cancel == "Keep") {
        mydata$ships[shipname] <-
          update_current_ship(shipname, input, mydata)
      } else if (input$cancel == "Retire") {
        mydata$ships[shipname] <-
          ceiling(c(rep(1, input$Yr - 2018), rep(0, 2046 - input$Yr + 1)) *
                    mydata$navy_data$`Fleet Plan`[shipname])
      }
    })
    
    Popover.template(ns("b"),
                     shipname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

# ================================================================================
# 2nd module type: OldShipSliderUI

# note: existing ships with slider,
# ...i.e. Ford,Burke,BurkeIII,FutureLSC

OldShipSliderUI <- function(id, labelname) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelname),
      style = "basic",
      value = 1,
      type = "toggle",
      size = "small",
      block = TRUE,
      disabled = FALSE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 1"),
      
      br(),
      
      fluidRow(
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] == 'Retire'"),
          
          column(3,
                 hidden(
                   sliderInput(
                     ns("Percent"),
                     "",
                     value = 1,
                     min = 0,
                     max = 1,
                     step = 0.01,
                     ticks = FALSE
                   )
                 ))
        ),
        
        conditionalPanel(condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
                         column(3)),
        
        column(6,
               radioButtons(
                 ns("cancel"),
                 "",
                 c("Keep", "Retire"),
                 selected = "Keep",
                 inline = TRUE
               )),
        
        bsTooltip(
          ns("Yr"),
          "Set the retirement year of the fleet",
          "top",
          options = list(container = "body")
        ),
        bsTooltip(
          ns("Percent"),
          "Set the fraction of the fleet to retire",
          "top",
          options = list(container = "body")
        ),
        
        conditionalPanel(condition = paste0(
          "input['", ns("cancel"), "'] != 'Nothing'"
        ),
        column(2)),
        
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] != 'Nothing'"),
          column(1,
                 actionLink(
                   inputId = ns("c"),
                   label = strong(" "),
                   icon = icon("bars")
                 ))
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
        sliderInput(
          inputId = ns("slider"),
          label = " ",
          value = unlist(default_ships[id]),
          min = unlist(min_ships[id]),
          max = unlist(max_ships[id]),
          step = 1,
          width = '90%',
          ticks = FALSE
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("cancel"), "'] == 'Retire'"),
        
        
        sliderInput(
          ns("Yr"),
          "",
          value = 2019,
          min = 2018,
          max = 2046,
          width = '90%',
          ticks = FALSE,
          sep = ""
        )
      )
    ),
    
    bsTooltip(
      ns("slider"),
      "Set the inventory level for 2046",
      "top",
      options = list(container = "body")
    ),
    
    bsTooltip(
      ns("c"),
      "Change cost assumptions",
      "left",
      options = list(container = "body")
    )
  )
}


OldShipSlider <-
  function(input,
           output,
           session,
           mydata,
           shipname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$cancel
      input$slider
      input$Yr
      input$Percent
    }, {
      if (input$cancel == "Keep") {
        mydata$ships[shipname] <-
          update_current_ship(shipname, input, mydata)
      } else if (input$cancel == "Retire") {
        mydata$ships[shipname] <- ceiling(c(rep(1, input$Yr - 2018),
                                            rep(0, 2046 - input$Yr + 1)) * mydata$navy_data$`Fleet Plan`[shipname])
        
        
      }
    })
    Popover.template(ns("b"),
                     shipname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

# --------------------------------------------------------------------------------
# LSUI

LSUI <- function(id, labelname) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelname),
      style = "basic",
      value = 1,
      type = "toggle",
      size = "small",
      block = TRUE,
      disabled = FALSE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 1"),
      
      br(),
      
      fluidRow(
        conditionalPanel(condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
                         column(3)),
        
        column(6,
               hidden(
                 radioButtons(
                   ns("cancel"),
                   "",
                   c("Keep", "Retire"),
                   selected = "Keep",
                   inline = TRUE
                 )
               )),
        
        conditionalPanel(condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
                         column(2)),
        
        conditionalPanel(
          condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
          column(1,
                 actionLink(
                   inputId = ns("c"),
                   label = strong(" "),
                   icon = icon("bars")
                 ))
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("cancel"), "'] == 'Keep'"),
        sliderInput(
          inputId = ns("slider"),
          label = " ",
          value = unlist(default_ships[id]),
          min = unlist(min_ships[id]),
          max = unlist(max_ships[id]),
          step = 1,
          width = '90%',
          ticks = FALSE
        )
      )
    ),
    
    bsTooltip(
      ns("slider"),
      "Set the inventory level for 2046",
      "top",
      options = list(container = "body")
    ),
    
    bsTooltip(
      ns("c"),
      "Change cost assumptions",
      "left",
      options = list(container = "body")
    )
  )
}


LS <-
  function(input,
           output,
           session,
           mydata,
           shipname,
           FullName,
           NickName,
           Purpose,
           imagename,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$slider
    }, {
      if (input$cancel == "Keep") {
        mydata$ships[shipname] <-
          update_current_ship(shipname, input, mydata)
      }
    })
    Popover.template(ns("b"),
                     shipname,
                     FullName,
                     NickName,
                     Purpose,
                     imagename,
                     session)
  }

# ================================================================================
# 3rd module type: NewShipModuleUI

# note: id, ship id, i.e."F-X"
# ...labelName: label name of ship, i.e."Build your own fighter"

NewShipModuleUI <- function(id,
                            labelName) {
  ns <- NS(id)
  
  tagList(
    bsButton(
      inputId = ns("b"),
      label = strong(labelName),
      style = "basic",
      value = 0,
      type = "toggle",
      size = "small",
      block = TRUE
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("b"), "'] == 1"),
      
      br(),
      
      fluidRow(
        column(3),
        column(6,
               
               radioButtons(
                 ns("whether_to_build"),
                 "",
                 c("Build", "Cancel"),
                 selected = "Cancel",
                 inline = TRUE
               )),
        
        column(2),
        
        column(1,
               actionLink(
                 inputId = ns("c"),
                 label = strong(" "),
                 icon = icon("bars")
               ))
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("whether_to_build"), "'] == 'Build'"),
        
        div(
          style = "display:inline-block",
          numericInput(
            ns("rd_start_num"),
            # label = "Milestone A Start (yr)",
            label = "",
            value = 2022,
            min = 2018,
            max = 2041,
            width = '150px'
          )
        ),
        bsTooltip(
          ns("rd_start_num"),
          "Set Milestone A (the start year) to begin RDT&E",
          "right",
          options = list(container = "body")
        ),
        
        
        div(
          style = "display:inline-block",
          numericInput(
            ns("rd_duration"),
            # label = "RDT&E Duration (yrs)",
            label = "",
            value = 5,
            min = 1,
            max = 27,
            width = '150px'
          )
        ),
        
        sliderInput(
          inputId = ns("slider"),
          label = "",
          value = 0,
          min = 0,
          max = 9,
          step = 1,
          width = '90%',
          ticks = FALSE
        ),
        
        bsTooltip(
          ns("slider"),
          "Set the inventory level for 2046",
          "top",
          options = list(container = "body")
        ),
        
        bsButton(
          ns("x_update"),
          label = strong("Update Acquisition Program"),
          style = "basic",
          disabled = FALSE,
          size = "small",
          width = '90%'
        ),
        br()
      ),
      
      bsTooltip(
        ns("rd_duration"),
        "Set the duration (number of years) of the RDT&E funding",
        "right",
        options = list(container = "body")
      ),
      
      br()
    )
  )
}


NewShipModule <-
  function(input,
           output,
           session,
           current,
           shipname,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, input$whether_to_build, {
      if (input$whether_to_build == "Build" & input$b) {
        current$navy_data %<>% apply_new_ship(
          new_name = shipname,
          stats_df = current$ship_stats_full,
          rd_start = input$rd_start_num,
          rd_duration = input$rd_duration
        )
        
        updateNumericInput(
          session,
          inputId =  "slider",
          min = 0,
          max = 9,
          value = input$slider
        )
        
        updateNumericInput(session,
                           inputId =  "rd_duration",
                           value = 5)
        
        updateNumericInput(session,
                           inputId = "rd_start_num",
                           value = 2022)
        
        updateButton(
          session,
          "x_update",
          label = "Update Acquisition Program",
          style = "primary",
          disabled = FALSE
        )
        
      } else if (input$whether_to_build == "Cancel") {
        if (shipname %in% names(current$ships)) {
          current %<>% remove_from_current(shipname, input)
          
          current$navy_data[["R&D Cost"]][shipname] <-
            current$ships[shipname] *
            c(
              rep(0, input$rd_start_num - navy_data[[1]]$FY[1]),
              rep(
                unlist(ship_stats_full[["R&D Cost"]][shipname]) / input$rd_duration,
                input$rd_duration
              ),
              rep(
                0,
                navy_data[[1]]$FY[nrow(navy_data[[1]])] -
                  input$rd_start_num - input$rd_duration + 1
              )
            )
          
          updateSliderInput(
            session,
            inputId = "slider",
            min = filter(current$navy_data[["Fleet Plan"]], FY == max(FY))[shipname],
            max = 35,
            value = 0
          )
          
          updateNumericInput(session,
                             "rd_duration",
                             value = 0)
          
          updateNumericInput(session,
                             "rd_start_num",
                             value = 2018)
          
          updateButton(
            session,
            "x_update",
            label = "Update Acquisition Program",
            style = "primary",
            disabled = FALSE
          )
        }
      }
    })
    
    observeEvent(ignoreInit = TRUE, input$x_update, {
      if (input$whether_to_build == "Build" & input$b) {
        current$ship_stats_full[["First Available"]][shipname] <-
          input$rd_start_num + input$rd_duration
        
        current$navy_data %<>% apply_new_ship(
          new_name = shipname,
          stats_df = current$ship_stats_full,
          rd_start = input$rd_start_num,
          rd_duration = input$rd_duration
        )
        
        temp <- input$slider
        
        updateSliderInput(
          session,
          inputId = "slider",
          min = as.numeric(filter(current$navy_data[["Fleet Plan"]], FY == max(FY))[shipname]),
          max = as.numeric(filter(current$navy_data[["Max New Builds"]], FY == max(FY))[shipname]),
          value = as.numeric(min(temp,
                                 current$navy_data[["Max New Builds"]][shipname][nrow(current$navy_data[["Max New Builds"]]), ]))
        )
        
        current$ships[shipname] <-
          update_current_ship(shipname,
                              min(input$slider,
                                  current$navy_data[["Max New Builds"]][shipname][nrow(current$navy_data[["Max New Builds"]]), ]),
                              current)
        
        updateButton(
          session,
          "x_update",
          label = "Update Acquisition Program",
          style = "primary",
          disabled = FALSE
        )
      }
    })
    
    observeEvent(ignoreInit = TRUE, input$rd_start_num, {
      if (current$navy_data[[1]]$FY[nrow(current$navy_data[[1]])] -
          input$rd_start_num < input$rd_duration + 2) {
        updateNumericInput(session,
                           "rd_duration",
                           value = current$navy_data[[1]]$FY[nrow(current$navy_data[[1]])] -
                             input$rd_start_num - 1)
      }
      
      updateButton(
        session,
        "x_update",
        label = "Update Acquisition Program",
        style = "primary",
        disabled = FALSE
      )
      
    })
    
    observeEvent(ignoreInit = TRUE, input$rd_duration, {
      if (current$navy_data[[1]]$FY[nrow(current$navy_data[[1]])] - input$rd_start_num <
          input$rd_duration + 2) {
        updateNumericInput(session,
                           "rd_start_num",
                           value = current$navy_data[[1]]$FY[nrow(current$navy_data[[1]])] -
                             input$rd_duration - 1)
      }
      
      updateButton(
        session,
        "x_update",
        label = "Update Acquisition Program",
        style = "primary",
        disabled = FALSE
      )
    })
    
  }

# ================================================================================
# 4th module type: OldShipbsModalUI

# note: id, i.e. Nimitz, Ford
# ...labelName: label name of ship, i.e. "Nimitz Class",

OldShipbsModalUI <- function(id,
                             labelName) {
  ns <- NS(id)
  
  bsModal(
    ns("modalExample"),
    labelName,
    ns("c"),
    size = "small",
    sliderInput(
      ns("acquisition_cost"),
      "Per Unit Acquisition Cost ($)",
      value = MasterFile[id][1, ],
      min = 0,
      max = 2 * MasterFile[id][1, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("br"),
      "Annual Max Build Rate",
      value = MasterFile[id][4, ],
      min = 0,
      max = 2 * MasterFile[id][4, ],
      step = 0.01,
      ticks = FALSE
    ),
    
    sliderInput(
      ns("d_os"),
      "Direct O&S Cost ($)",
      value = MasterFile[id][5, ],
      min = 0,
      max = 2 * MasterFile[id][5, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("i_os"),
      "Indirect O&S Cost ($)",
      value = MasterFile[id][6, ],
      min = 0,
      max = 2 * MasterFile[id][6, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("o_os"),
      "Overhead O&S Cost ($)",
      value = MasterFile[id][7, ],
      min = 0,
      max = 2 * MasterFile[id][7, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("d_p"),
      "Direct Personnel",
      value = MasterFile[id][9, ],
      min = 0,
      max = 2 * MasterFile[id][9, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("i_p"),
      "Indirect Personnel",
      value = MasterFile[id][10, ],
      min = 0,
      max = 2 * MasterFile[id][10, ],
      ticks = FALSE
    ),
    
    sliderInput(
      ns("o_p"),
      "Overhead Personnel",
      value = MasterFile[id][11, ],
      min = 0,
      max = 2 * MasterFile[id][11, ],
      ticks = FALSE
    )
  )
}

OldShipbsModal <-
  function(input,
           output,
           session,
           current,
           shipname,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$acquisition_cost
      input$br
      input$d_os
      input$i_os
      input$o_os
      input$d_p
      input$i_p
      input$o_p
    }, {
      if (input$c) {
        # update acquisition cost
        current$ship_stats_full$`Acquisition Cost`[shipname] <-
          input$acquisition_cost
        current$navy_data$`Acquisition Yearly`[shipname] <-
          input$acquisition_cost
        
        # max build per year
        current$ship_stats_full$`Max Build Per Year`[shipname] <-
          input$br
        
        # update planned Work
        current$navy_data$`Planned Work`[shipname] <-
          update_planned_work(shipname, current, input)
        
        # sum planned work
        current$navy_data$`Sum Planned Work`[shipname] <-
          add_up_all_previous(current$navy_data$`Planned Work`[[shipname]])
        
        # free capacity
        current$navy_data$`Free Capacity`[shipname] <-
          current$ship_stats_full$`Max Build Per Year`[[shipname]] - current$navy_data$`Planned Work`[[shipname]]
        
        # update sum free capacity
        current$navy_data$`Sum Free Capacity`[shipname] <-
          add_up_all_previous(current$navy_data$`Free Capacity`[[shipname]])
        
        # update max new builds
        current$navy_data$`Max New Builds`[shipname] <-
          floor(current$navy_data$`Sum Free Capacity`[shipname])
        
        # update slider accordingly
        temp <- input$slider
        
        updateSliderInput(
          session,
          inputId = "slider",
          min = filter(current$navy_data$`Fleet Plan`, FY == max(FY))[[shipname]] -
            filter(current$navy_data[["Max Cut Builds"]], FY == max(FY))[[shipname]],
          max = filter(current$navy_data$`Fleet Plan`, FY == max(FY))[[shipname]] +
            filter(current$navy_data[["Max New Builds"]], FY == max(FY))[[shipname]],
          value = min(
            temp,
            filter(current$navy_data$`Fleet Plan`, FY == max(FY))[[shipname]] +
              current$navy_data[["Max New Builds"]][[shipname]][nrow(current$navy_data[["Max New Builds"]])]
          )
        )
        
        # update max new acq
        current$navy_data$`Max New Acq`[[shipname]] <-
          (
            current$ship_stats_full$`Max Build Per Year`[[shipname]] - current$navy_data$`Planned Work`[[shipname]]
          ) *
          current$ship_stats_full$`Acquisition Cost`[[shipname]]
        
        # update max cut acq
        current$navy_data$`Max Cut Acq`[[shipname]] <-
          current$navy_data$`Planned Work`[[shipname]] *
          current$ship_stats_full$`Acquisition Cost`[[shipname]]
        
        # update all O&S cost-related stats
        current$ship_stats_full$`Direct Cost`[shipname] <-
          input$d_os
        current$ship_stats_full$`Indirect Cost`[shipname] <-
          input$i_os
        current$ship_stats_full$`Overhead Cost`[shipname] <-
          input$o_os
        current$ship_stats_full$`Total Cost`[shipname] <-
          input$d_os + input$i_os + input$o_os
        current$ship_stats_full$`Yearly O&S`[shipname] <-
          input$d_os + input$i_os + input$o_os
        current$navy_data$`O&S Yearly`[shipname] <-
          input$d_os + input$i_os + input$o_os
        
        # max new O&S
        current$airforce_data$`Max New O&S`[[shipname]] <-
          update_max_new_OS(current, shipname)
        
        # update all personnel-related stats
        current$ship_stats_full$`Direct Personnel`[shipname] <-
          input$d_p
        current$ship_stats_full$`Indirect Personnel`[shipname] <-
          input$i_p
        current$ship_stats_full$`Overhead Personnel`[shipname] <-
          input$o_p
        current$ship_stats_full$`Total Personnel`[shipname] <-
          input$d_p + input$i_p + input$o_p
      }
    })
  }

# ================================================================================
# 5th module type: newshipbsModalUI

newshipbsModalUI <- function(id, labelname) {
  ns <- NS(id)
  
  bsModal(ns("modalExample"),
          labelname,
          ns("c"),
          size = "small",
          
          tabsetPanel(
            tabPanel(
              "Cost",
              br(),
              
              sliderInput(
                ns("acquisition_cost"),
                "Per Unit Acquisition Cost ($)",
                value = newshipstats[id][1, ],
                min = 0,
                max = 2 * newshipstats[id][1, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("br"),
                "Annual Max Build Rate",
                value = newshipstats[id][6, ],
                min = 0,
                max = 2 * newshipstats[id][6, ],
                step = 0.01,
                ticks = FALSE
              ),
              sliderInput(
                ns("rd"),
                "R&D Cost",
                value = newshipstats[id][5, ],
                min = 0,
                max = 2 * newshipstats[id][5, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("d_os"),
                "Direct O&S Cost",
                value = newshipstats[id][7, ],
                min = 0,
                max = 2 * newshipstats[id][7, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("i_os"),
                "Indirect O&S Cost",
                value = newshipstats[id][8, ],
                min = 0,
                max = 2 * newshipstats[id][8, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("o_os"),
                "Overhead O&S Cost",
                value = newshipstats[id][9, ],
                min = 0,
                max = 2 * newshipstats[id][9, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("d_p"),
                "Direct Personnel",
                value = newshipstats[id][11, ],
                min = 0,
                max = 2 * newshipstats[id][11, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("i_p"),
                "Indirect Personnel",
                value = newshipstats[id][12, ],
                min = 0,
                max = 2 * newshipstats[id][12, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("o_p"),
                "Overhead Personnel",
                value = newshipstats[id][13, ],
                min = 0,
                max = 2 * newshipstats[id][13, ],
                ticks = FALSE
              )
            ),
            
            tabPanel(
              "Performance",
              br(),
              sliderInput(
                ns("aircraftcapacity"),
                "Aircraft Capacity",
                value = newshipstats[id][17, ],
                min = 0,
                max = 2 * newshipstats[id][17, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("vlstubes"),
                "VLS Tubes",
                value = newshipstats[id][18, ],
                min = 0,
                max = 2 * newshipstats[id][18, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("speed"),
                "Speed (knots)",
                value = newshipstats[id][15, ],
                min = 0,
                max = 2 * newshipstats[id][15, ],
                ticks = FALSE
              ),
              sliderInput(
                ns("tonnage"),
                "Tonnage (lbs)",
                value = newshipstats[id][16, ],
                min = 0,
                max = 2 * newshipstats[id][16, ],
                ticks = FALSE
              )
            )
          ))
}

newshipbsModal <-
  function(input,
           output,
           session,
           current,
           shipname,
           ...) {
    ns <- session$ns
    
    observeEvent(ignoreInit = TRUE, {
      input$acquisition_cost
      input$br
      input$rd
      input$d_os
      input$i_os
      input$o_os
      input$d_p
      input$i_p
      input$o_p
      input$aircraftcapacity
      input$vlstubes
      input$speed
      input$tonnage
    }, {
      if (input$c) {
        # update cost assumptions
        current$ship_stats_full$`Acquisition Cost`[shipname] <-
          input$acquisition_cost
        current$ship_stats_full$`Max Build Per Year`[shipname] <-
          input$br
        current$ship_stats_full$`R&D Cost`[shipname] <- input$rd
        current$ship_stats_full$`Direct Cost`[shipname] <-
          input$d_os
        current$ship_stats_full$`Indirect Cost`[shipname] <-
          input$i_os
        current$ship_stats_full$`Overhead Cost`[shipname] <-
          input$o_os
        current$ship_stats_full$`Total Cost`[shipname] <-
          input$d_os + input$i_os + input$o_os
        current$ship_stats_full$`Yearly O&S`[shipname] <-
          input$d_os + input$i_os + input$o_os
        current$ship_stats_full$`Direct Personnel`[shipname] <-
          input$d_p
        current$ship_stats_full$`Indirect Personnel`[shipname] <-
          input$i_p
        current$ship_stats_full$`Overhead Personnel`[shipname] <-
          input$o_p
        current$ship_stats_full$`Total Personnel`[shipname] <-
          input$d_p + input$i_p + input$o_p
        
        
        # update performance assumptions
        current$ship_stats_full$`Aircraft Capacity`[shipname] <-
          input$aircraftcapacity
        current$ship_stats_full$`VLS Tubes`[shipname] <-
          input$vlstubes
        current$ship_stats_full$`Speed (knots)`[shipname] <-
          input$speed
        current$ship_stats_full$`Tonnage (lbs)`[shipname] <-
          input$tonnage
        
      }
    })
  }

# ================================================================================
