#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
dashboardPage(
    dashboardHeader(title = 'Wenner Wine',titleWidth = '19%'),
    dashboardSidebar(disable = T),
    dashboardBody(tags$head(
        tags$script('$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'),
        tags$head(tags$link(rel="shortcut icon", href="wine.ico")),
        tags$link(rel="stylesheet", type = "text/css",
        href="https://fonts.googleapis.com/css?family=Euphoria%20Script"),
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny-style.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny-style-small.css"),
        tags$style(".col-lg-1, .col-lg-10, .col-lg-11, .col-lg-12, .col-lg-2, .col-lg-3, .col-lg-4, .col-lg-5, .col-lg-6, .col-lg-7, .col-lg-8, .col-lg-9, .col-md-1, .col-md-10, .col-md-11, .col-md-12, .col-md-2, .col-md-3, .col-md-4, .col-md-5, .col-md-6, .col-md-7, .col-md-8, .col-md-9, .col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-xs-1, .col-xs-10, .col-xs-11, .col-xs-12, .col-xs-2, .col-xs-3, .col-xs-4, .col-xs-5, .col-xs-6, .col-xs-7, .col-xs-8, .col-xs-9{
                   padding-left:3px; padding-right:3px;")
    ),
    tabsetPanel(
        tabPanel('Wine List',
                 br(),
    bscols(
        widths = c(2, 10),device = 'sm',
        list(
        uiOutput('filters')),
        reactableOutput('tbl')
    ),br(),
    actionButton(inputId = 'refresh_list',
                 label = 'Refresh Data'
                 )),
    tabPanel('Plots',
             tabsetPanel(type = 'pills',
                 tabPanel('Varietals',
        plotlyOutput('var_plot',height = '800px')),
        tabPanel('Region',
               plotlyOutput('reg_plot', height = '500px')))
    ),
    tabPanel('Maps',
             # tabsetPanel(type = 'pills',
             #             tabPanel('World',
             #                      highchartOutput('map')),
             #             tabPanel('Europe',
             #                      highchartOutput('map_eur')),
             #             tabPanel('US',
             #                      highchartOutput('map_us'))
             #             )

             fluidRow(column(3,highchartOutput('map_us')),
                      column(6,highchartOutput('map')),
                      column(3,highchartOutput('map_eur'))
                      )
             ),
    tabPanel('Consumed',
             reactableOutput('consumed_tbl'))
    )
    )
)
