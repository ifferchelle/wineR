#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    observeEvent({input$refresh_list},
                 GET(url = update_url),
                 ignoreInit = T,
                 ignoreNULL = T,
                 once = T)

    output$map <- renderHighchart({
        hc
    })

    output$map_eur <- renderHighchart({
        hc_eur
    })

    output$map_us <- renderHighchart({
        hc_us
    })

    output$var_plot <- renderPlotly({
        ggplotly(gg) %>%
            layout(font = list(family = c('"Segeo UI","Open Sans", Arial'))) ##'"Segoe UI", Calibri, Tahoma, Arial, sans-serif'))
    })

    output$reg_plot <- renderPlotly({
        ggplotly(gg2) %>%
            layout(font = list(family = c('"Segeo UI","Open Sans", Arial'))) ##'"Segoe UI", Calibri, Tahoma, Arial, sans-serif'))
    })

    output$filters <- renderUI({
        if(length(input$GetScreenWidth)==0){
            is_small <- F
        } else if(is.na(input$GetScreenWidth)){
            is_small <- F
        } else is_small <- (input$GetScreenWidth<768)

        box(collapsible = T,
            title = 'Wine Filters',
            width = 12,
            collapsed = is_small,
        div(style = css(fontFamily = 'Segoe UI',
                        fontSize = '12.5px'),
            filter_checkbox("type", "Type", shared_data, ~Type,
                            inline = is_small),
            filter_checkbox("blend", "Blend", shared_data, ~Blend,
                            inline = is_small),
            filter_slider("price", "Price", shared_data, ~Price, width = "100%",pre = '$'),
            filter_slider("vintage", "Vintage", shared_data, ~Vnt, width = "100%",sep = ''),
            filter_slider("quant", "Quantity", shared_data, ~Qty, width = "100%"),
            filter_select("var", "Varietal", shared_data, ~Varietal)
        ))
    })

    output$tbl <- renderReactable({

        reactable(shared_data, 'wine',
                             # theme = reactableThemeWine(searchInputStyle = list(width = "100%")),
                  fontSize = '12px',
                  searchable = TRUE,
                  columns = list(
                      Wine = colDef(html = TRUE,
                                    minWidth = 100),
                      Vnt = colDef(sortNALast = T,
                                   minWidth = 50),
                      Qty = colDef(minWidth = 40),
                      Varietal = colDef(minWidth = 80),
                      Region = colDef(minWidth = 80),
                      SubRegion = colDef(minWidth = 90),
                      Location = colDef(minWidth = 80),

                      Begin= colDef(sortNALast = T,
                                    minWidth = 50),
                      End = colDef(sortNALast = T,
                                   minWidth = 50),
                      Rating = colDef(html = TRUE,
                                      sortNALast = T,
                                      minWidth = 40),
                      Price = colDef(format = colFormat(prefix = "$",digits = 0),
                                     sortNALast = T, minWidth = 50),
                      Blend = colDef(cell = function(value) {
                          # Render ✓
                          if (value == 'Blend') "\u2713" else ""
                      }, minWidth = 40),
                      Type = colDef(minWidth = 75),
                      Country = colDef(minWidth = 75),

                      Store = colDef(minWidth = 75),
                      Purchased = colDef(minWidth = 65)
                  ))

            })

    output$consumed_tbl <- renderReactable({

        reactable(shared_data_consumed, 'wine',
                  searchable = TRUE,
                  columns = list(
                      Wine = colDef(html = TRUE))
        )

    })


})
