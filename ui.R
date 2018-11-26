# ================================================================================
# Build your own Navy 2.0
# Designed and built by Gabriel Coll, Yuanjing Han, and Loren Lipsey
# --------------------------------------------------------------------------------
# user interface
# build your own Navy and see the implications of your shipbuilding plan
# ================================================================================

# --------------------------------------------------------------------------------
# load packages

library(shinyjs)
library(shinyBS)
library(tidyverse)
library(extrafont)
library(extrafontdb)

# --------------------------------------------------------------------------------
# load functions

source("module_functions.R")

# --------------------------------------------------------------------------------
# load terms

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

# --------------------------------------------------------------------------------
# load data

load("data/navy_data_full.Rda")
load("data/ship_stats_full.Rda")

# --------------------------------------------------------------------------------
# get the min, max, and default settings for sliders

default_ships <- filter(navy_data[["Fleet Plan"]], FY == max(FY))

min_ships <- default_ships -
  filter(navy_data[["Max Cut Builds"]], FY == max(FY))

max_ships <- default_ships +
  filter(navy_data[["Max New Builds"]], FY == max(FY))

ship_stats_full <- ship_stats_full[5:length(ship_stats_full)]

# --------------------------------------------------------------------------------
# begin user interface section

ui <- fluidPage(
  useShinyjs(),
  
  # --------------------------------------------------------------------------------
  # import Google Font "Open Sans"
  
  tags$style(
    HTML(
      "@import url('//fonts.googleapis.com/css?family=Open+Sans');
      body {
      font-family: 'Open Sans',  sans-serif;
      font-weight: 500;
      line-height: 1.1;
      color: #554449;
      }"
)
    ),

# --------------------------------------------------------------------------------
# app design style

tags$style(
  type = "text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
),

tags$style(HTML(
  ".tooltip {position: {my: 'left top', at: 'left bottom'}"
)),

tags$style(HTML(".fa-bars {
                color: #4D7FA3;
                }")),

tags$head(tags$style(
  HTML("body{background-color: #fcfcfc;}")
)),

(
  tags$style(
    ".help-block{color: #554449;
    font-size: 14px;
    font-style: normal;
    background-color: #fcfcfc;
    border-color: #C76363;
    border-style: solid;
    border-width: 4px;
    border-top: 4px #63c5b8;
    border-bottom: 4px #63c5b8;
    border-right: 4px #63c5b8;
    #border-left: 4px #63c5b8;
    border-radius: 1px
    }"
  )
  ),

# --------------------------------------------------------------------------------
# button style

tags$style(
  HTML(
    ".btn {background-color: #4D7FA3;
    color: white;
    border-color: #FCFCFC}"
  )
),
tags$style(
  HTML(
    ".btn:hover {border-color: #FCFCFC;
    background-color: #2F4D63;
    color: white;
    font-weight: normal}"
  )
  ),
tags$style(HTML(
  ".btn:active:hover {background-color: #6BC6B5}"
)),
tags$style(HTML(
  ".btn:dropdown-toggle {background-color: red}"
)),

tags$style(
  HTML(
    ".btn-primary {background-color: #BDD4DE;
    border-color: #FCFCFC;
    color: #554449}"
  )
),

tags$style(
  HTML(
    ".btn-primary:hover {background-color: #A5B9C2;
    color: #554449}"
  )
),

tags$style(
  HTML(".btn-primary:active:hover{background-color: #6BC6B5}")
),

tags$style(
  HTML(".btn-primary:active:focus{background-color: #90ABC2}")
),

tags$style(
  HTML(".btn-primary:dropdown-toggle {background-color: #BDD4DE}")
),

tags$style(
  HTML(
    ".btn-basic {background-color: #BDD4DE;
    border-color: #FCFCFC;
    color: #554449}"
  )
),

tags$style(
  HTML(
    ".btn-basic:hover {border-color: #FCFCFC;
    background-color: #A5B9C2;
    color: #FFFFFF}"
  )
),

tags$style(
  HTML(
    ".btn-basic:active,.open>.dropdown-toggle.btn-basic {background-color: #6BC6B5}"
  )
),

tags$style(
  HTML(".btn-basic:dropdown-toggle {background-color: #BDD4DE}")
),

# --------------------------------------------------------------------------------
# slider style

tags$style(
  HTML(
    ".irs-bar {background: #788ca8;
    border-top: 1px #566377;
    border-bottom: 1px #566377}"
  )
),

tags$style(HTML(
  ".irs-single, .irs-to, .irs-from {background: #788ca8}"
)),

tags$style(HTML(".irs-max {color: #554449}")),
tags$style(HTML(".irs-min {color: #554449}")),

tags$style(
  HTML(
    ".irs-bar-edge {border: 1px #566377;
    border-color: 1px #566377;
    border-color: 1px #566377}"
  )
),

tags$style(
  HTML(".irs .irs-bar-edge, .irs .irs-bar {
       background: #788ca8;
       }")
)
,

# --------------------------------------------------------------------------------
# code to prevent early app disconnection

tags$head(
  HTML(
    "
    <script>
    var socket_timeout_interval
    var n = 0
    $(document).on('shiny:connected', function(event) {
    socket_timeout_interval = setInterval(function(){
    Shiny.onInputChange('count', n++)
    }, 15000)
    });
    $(document).on('shiny:disconnected', function(event) {
    clearInterval(socket_timeout_interval)
    });
    </script>
    "
  )
  ),

# --------------------------------------------------------------------------------
# CSIS header

tags$div(
  HTML(
    "<div class='fusion-secondary-header'>
    <div class='fusion-row'>
    <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:15px;'><a href='https://defense360.csis.org/content-type/data/' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
    </div>
    </div>"
  )
  ),
tags$style(
  HTML(
    ".fusion-secondary-header {border-bottom: 2.5px solid #6F828F}"
  )
),
br(),

# --------------------------------------------------------------------------------
# begin sidebar panel

fluidRow(
  column(
    4,
    shinyjs::useShinyjs(),
    id = "side-panel",
    
    tags$head(tags$style(
      HTML("body{background-color: #FCFCFC;}")
    )),
    
    tags$head(tags$style(
      HTML(".well{
           background-color: #FCFCFC;
           border-color: #FCFCFC;
           }")
)),

tags$style(HTML(
  ".popover({delay: {show: 500, hide: 100}})"
)),

# --------------------------------------------------------------------------------
# Google analytics script

tags$script(
  HTML(
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-99363803-1', 'auto');
    ga('send', 'pageview')"
)
),

# --------------------------------------------------------------------------------

hidden(textOutput("keepAlive")),

# --------------------------------------------------------------------------------
# info button

bsButton(
  inputId = "info_btn",
  label = strong("Build your own Navy >"),
  style = "default",
  type = "action",
  size = "small",
  block = TRUE
),


br(),

# --------------------------------------------------------------------------------
# options button

bsButton(
  "options_top_bottom",
  label = strong("More Options"),
  style = "basic",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

br(),

# --------------------------------------------------------------------------------
# options panel

conditionalPanel(
  condition = "input.options_top_bottom == 1",
  radioButtons("top_chart", "",
               c("Area", "Line"),
               inline = TRUE),
  bsTooltip(
    "top_chart",
    "Select top chart type",
    "top",
    options = list(container = "body")
  ),
  bsTooltip(
    "top_y",
    "Select top chart y-axis",
    "top",
    options = list(container = "body")
  ),
  bsTooltip(
    "bottom_chart",
    "Select bottom chart type",
    "top",
    options = list(container = "body")
  ),
  
  selectizeInput(
    inputId = "top_y",
    label = "",
    choices = list(
      Ships = c("Ships"),
      "O&S Cost (CBO)" = c("Direct Cost",
                           "Indirect Cost",
                           "Overhead Cost",
                           "Total Cost"),
      "Personnel (CBO)" = c(
        "Direct Personnel",
        "Indirect Personnel",
        "Overhead Personnel",
        "Total Personnel"
      ),
      "Crew (USN)" = c("Crew",
                       "Officers",
                       "Enlisted"),
      "Performance / Specifications" = c(
        "Aircraft Capacity",
        "VLS Tubes",
        "Speed (knots)",
        "Tonnage (lbs)"
      )
    ),
    selected = "Ships"
  ),
  
  radioButtons("bottom_chart", "",
               c("Total", "Change"),
               inline = TRUE),
  
  br(),
  align = "center"
),

# --------------------------------------------------------------------------------
# cvn panel

bsButton(
  inputId = "AircraftCarriers",
  label = strong("Aircraft Carriers"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.AircraftCarriers == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("Nimitz", "Nimitz class"),
  OldShipSliderUI("Ford", "Ford class"),
  NewShipModuleUI("CVN-X", "Build your own carrier")
),

# --------------------------------------------------------------------------------
# lsc panel

bsButton(
  inputId = "LSC",
  label = strong("Large Surface Combatants"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.LSC == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("Ticonderoga", "Ticonderoga class"),
  OldShipSliderUI("Burke", "Burke class (Flight I, II)"),
  OldShipSliderUI("BurkeIII", "Burke class (Flight III)"),
  OldShipSliderUI("Zumwalt", "Zumwalt class"),
  OldShipSliderUI("FutureLSC", "Future LSC"),
  NewShipModuleUI("Cruiser-X", "Build your own cruiser"),
  NewShipModuleUI("Destroyer-X", "Build your own destroyer")
),

# --------------------------------------------------------------------------------
# ssc panel

bsButton(
  inputId = "SSC",
  label = strong("Small Surface Combatants"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.SSC == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("Avenger", "Avenger class"),
  OldShipSliderUI("LittoralCombatShip", "Littoral Combat Ship"),
  OldShipSliderUI("FastFrigate", "FFG(X)"),
  OldShipSliderUI("FutureSSC", "Future SSC"),
  NewShipModuleUI("Frigate-X", "Build your own frigate")
),

# --------------------------------------------------------------------------------
# ssn panel

bsButton(
  inputId = "SSN",
  label = strong("Attack Submarines"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.SSN == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("LosAngeles", "Los Angeles class"),
  OldShipNoSliderUI("LosAngelesi", "Los Angeles class (i)"),
  OldShipNoSliderUI("Seawolf", "Seawolf class"),
  OldShipSliderUI("Virginia", "Virginia class"),
  OldShipSliderUI("Virginiai", "Virginia class (i)"),
  NewShipModuleUI("SSN-X", "Build your own attack submarine")
),

# --------------------------------------------------------------------------------
# ssgn panel

bsButton(
  inputId = "SSGN",
  label = strong("Cruise Missile Submarines"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.SSGN == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("OhioSSGN", "Ohio class (SSGN)"),
  NewShipModuleUI("SSGN-X", "Build your own cruise missile submarine")
),

# --------------------------------------------------------------------------------
# ssbn panel

bsButton(
  inputId = "SSBN",
  label = strong("Ballistic Missile Submarines"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.SSBN == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("OhioSSBN", "Ohio class (SSBN)"),
  OldShipSliderUI("Columbia", "Columbia class"),
  NewShipModuleUI("SSBN-X", "Build your own ballistic missile submarine")
),

# --------------------------------------------------------------------------------
# aws panel

bsButton(
  inputId = "AWS",
  label = strong("Amphibious Warfare Ships"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.AWS == 1",
  
  # --------------------------------------------------------------------------------
  
  OldShipNoSliderUI("WhidbeyIsland", "Whidbey Island class"),
  OldShipNoSliderUI("Wasp", "Wasp class"),
  OldShipNoSliderUI("HarpersFerry", "Harpers Ferry class"),
  OldShipSliderUI("SanAntonio", "San Antonio class"),
  OldShipSliderUI("America", "America class"),
  OldShipSliderUI("LXR", "LX(R)"),
  NewShipModuleUI("AWS-X", "Build your own amphibious warfare ship")
),

# --------------------------------------------------------------------------------
# logistics and support panel

bsButton(
  inputId = "LogisticsSupport",
  label = strong("Logistics and Support"),
  style = "default",
  value = 0,
  type = "toggle",
  size = "small",
  block = TRUE
),

conditionalPanel(
  condition = "input.LogisticsSupport == 1",
  
  # --------------------------------------------------------------------------------
  
  LSUI("CLF", "Combat Logistics Force"),
  LSUI("SupportVessels", "Support Vessels")
),

br(),

# --------------------------------------------------------------------------------
# (currently hidden) user submission function

conditionalPanel(
  condition = "input.options_top_bottom == 3",
  textInput("text", label = NULL,
            placeholder = "Email (optional)"),
  textInput("text2", label = NULL,
            placeholder = "Comments (optional)")
),

bsButton(
  inputId = "reset_input",
  label = strong("Reset"),
  style = "basic",
  size = "small",
  width = '100%',
  block = TRUE
),

br(),

helpText(
  HTML(
    "<div align = 'left'>",
    "<h5> The default settings for this app are based on the <a href=https://www.cbo.gov/publication/52324>CBO's analysis</a>  of the Navy's fiscal year 2017 shipbuilding plan. As of the release of this app, CBO had not published a report on the 2018 plan.",
    "<br/>",
    "</div>"
  )
),

conditionalPanel(
  condition = "input.options_top_bottom == 3",
  tags$head(tags$script(src = "message-handler.js")),
  bsButton(
    inputId = "submit",
    label = strong("Submit"),
    style = "default",
    size = "small",
    width = '100%',
    block = TRUE
  ),
  
  bsTooltip(
    "submit",
    "CSIS is collecting data from users who are willing to share their USN aircraft inventory plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
    "right",
    options = list(container = "body")
  ),
  bsTooltip(
    "text",
    "CSIS is collecting data from users who are willing to share their USN aircraft inventory plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
    "right",
    options = list(container = "body")
  ),
  bsTooltip(
    "text2",
    "CSIS is collecting data from users who are willing to share their USN aircraft inventory plan. If you have any questions about this effort, please contact Gabriel Coll (gcoll@csis.org)",
    "right",
    options = list(container = "body")
  ),
  
  br(),
  br()
),

conditionalPanel(
  condition = "input.options_input == 3",
  div(
    style = "display:inline-block",
    
    checkboxInput("checkbox", label = "Dollars", value = FALSE)
  )
  
),

bsModal("modalExample", trigger = "info_btn", htmlOutput("description")),

align = "center"
  ),

# --------------------------------------------------------------------------------
# begin main panel

mainPanel(
  div(
    style = "position:relative",
    plotOutput(
      "ships",
      height = "320px",
      hover = hoverOpts(id = "plot_hover_ships", delay = 80)
    ),
    uiOutput("hover_info_ships")
  ),
  
  # --------------------------------------------------------------------------------
  
  shinyjs::useShinyjs(),
  id = "main-panel",
  
  # current ships
  OldShipbsModalUI("Nimitz", "Nimitz class"),
  OldShipbsModalUI("Ford", "Ford class"),
  
  OldShipbsModalUI("Ticonderoga", "Ticonderoga class"),
  OldShipbsModalUI("Burke", "Burke class (Flight I, II)"),
  OldShipbsModalUI("BurkeIII", "Burke class (Flight III)"),
  OldShipbsModalUI("Zumwalt", "Zumwalt class"),
  OldShipbsModalUI("FutureLSC", "Future Large Surface Combatant"),
  
  OldShipbsModalUI("Avenger", "Avenger class"),
  OldShipbsModalUI("LittoralCombatShip", "Littoral Combat Ship"),
  OldShipbsModalUI("FastFrigate", "Fast Frigate"),
  OldShipbsModalUI("FutureSSC", "Future Small Surface Combatant"),
  
  OldShipbsModalUI("LosAngeles", "Los Angeles class"),
  OldShipbsModalUI("LosAngelesi", "Los Angeles class (improved)"),
  OldShipbsModalUI("Seawolf", "Seawolf class"),
  OldShipbsModalUI("Virginia", "Virginia class"),
  OldShipbsModalUI("Virginiai", "Virginia class (improved)"),
  
  OldShipbsModalUI("OhioSSGN", "Ohio class (SSGN)"),
  
  OldShipbsModalUI("OhioSSBN", "Ohio class (SSBN)"),
  OldShipbsModalUI("Columbia", "Columbia class"),
  
  OldShipbsModalUI("WhidbeyIsland", "Whidbey Island class"),
  OldShipbsModalUI("Wasp", "Wasp class"),
  OldShipbsModalUI("HarpersFerry", "Harpers Ferry class"),
  OldShipbsModalUI("SanAntonio", "San Antonio class"),
  OldShipbsModalUI("America", "America class"),
  OldShipbsModalUI("LXR", "LX(R)"),
  
  OldShipbsModalUI("CLF", "Combat Logistics Force"),
  OldShipbsModalUI("SupportVessels", "Support Vessels"),
  
  # future ships
  newshipbsModalUI("CVN-X", "CVN-X: Build your own carrier"),
  newshipbsModalUI("Cruiser-X", "Cruiser-X: Build your own cruiser"),
  newshipbsModalUI("Destroyer-X", "Destroyer-X: Build your own destroyer"),
  newshipbsModalUI("Frigate-X", "Frigate-X: Build your own frigate"),
  newshipbsModalUI("SSN-X", "SSN-X: Build your own attack submarine"),
  newshipbsModalUI("SSGN-X", "SSGN-X: Build your own cruise missile submarine"),
  newshipbsModalUI("SSBN-X", "SSBN-X: Build your own ballistic missile submarine"),
  newshipbsModalUI("AWS-X", "AWS-X: Build your own amphibious warfare ship"),
  
  br(),
  
  div(
    style = "position:relative",
    plotOutput(
      "budget",
      height = "320px",
      hover = hoverOpts(id = "plot_hover_budget", delay = 80)
    ),
    uiOutput("hover_info_budget")
  ),
  
  align = 'center'
)
),

hr(),

fluidRow(
  tags$body(tags$style(HTML(
    ".col-sm-6 {border: 4}"
  ))),
  
  tags$style(
    ".selectize-control {
    color: #554449;
    font-size: 14px;
    font-style: normal;
    background-color: #EDECEB;
    border-color: #C76363;
    border-style: solid;
    border-width: 6px;
    border-top: 6px #63c5b8;
    border-bottom: 6px #63c5b8;
    border-right: 6px #63c5b8;
    #border-left: 6px #63c5b8;
    border-radius: 5px
    }"
    ),
  tags$style(HTML(".popover {background: #256A91};")),
  tags$style(
    HTML(
      ".popover-content {color: white; font-family: 'Open Sans',  sans-serif};"
    )
  ),
  
  tags$style(HTML(".arrow {visibility: hidden};")),
  tags$style(HTML(".tooltip-arrow {visibility: hidden};")),
  
  tags$style(
    HTML(
      ".popover.right > .arrow:after {visibility: hidden;
      border-right-color: white;
      border-color: white;
      };"
    )
    ),
  
  tags$style(HTML(".well {color: #554449}")),
  
  tags$style(
    HTML(
      "img {
      padding:1px;
      #border:1px solid #021a40;
      #background-color:#ff0;
      height: 'auto';
      max-width: '100%';
      border-radius: 5px
      }"
)
    ),

tags$style(HTML(
  ".img .shiny-image-output {border: 4px green}"
)),

tags$style(HTML(
  ".img .shiny-image-output {border-color: 4px purple}"
)),

tags$style(HTML(
  ".irs-bar-edge {border-color: 1px #63c5b8}"
))
    ),

uiOutput("bottom_line"),

br(),
br(),
br()

  )

# ================================================================================
