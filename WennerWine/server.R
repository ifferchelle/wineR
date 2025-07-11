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


    price_gg <- reactive({
      price_graph(varietal = input$pp_varietal,
                  type = input$pp_type,
                  vint_low = input$pp_vint_low,
                  vint_high = input$pp_vint_high,
                  include_nv = input$pp_include_nv)
    })

    output$price_plot <- renderPlotly({
      gg <- price_gg()
      if(!is.null(gg)) ggplotly(gg)
    })


    output$price_ui <- renderUI({

      if(!is.null(price_gg())){
        plotlyOutput('price_plot')
      } else{
        h4('No wine found with filters provided.')
      }

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
            filter_checkbox("cold", "Cold", shared_data, ~AnyCold,
                            inline = is_small),
            filter_checkbox("type", "Type", shared_data, ~Type,
                            inline = is_small),
            filter_checkbox("blend", "Blend", shared_data, ~Blend,
                            inline = is_small),
            #filter_slider("price", "Price", shared_data, ~Price, width = "100%",pre = '$'),
            filter_checkbox("price2", "Price", shared_data, ~Price2,
                            inline = is_small),
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
                      AnyCold = colDef(show = F),
                      Cold = colDef(show = F),
                      Wine = colDef(html = TRUE,
                                    minWidth = 100),
                      Vnt = colDef(sortNALast = T,
                                   minWidth = 50),
                      Qty = colDef(minWidth = 40),
                      Varietal = colDef(show = T, minWidth = 80),
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
                      Price2 = colDef(show = FALSE),
                      Blend = colDef(show = FALSE,
                                     cell = function(value) {
                          # Render ✓
                          if (value == 'Blend') "\u2713" else ""
                      }, minWidth = 40),
                      Type = colDef(show = FALSE,minWidth = 75),
                      TypeColor = colDef(show = FALSE),
                      VarietalInspect = colDef(show = FALSE),
                      Color = colDef(show = FALSE),
                      Country = colDef(minWidth = 75),

                      Store = colDef(minWidth = 75),
                      Purchased = colDef(minWidth = 65)
                  ))

            })

    output$consumed_tbl <- renderReactable({

        reactable(shared_data_consumed, 'wine',
                  fontSize = '12px',
                  searchable = TRUE,
                  columns = list(
                      Wine = colDef(html = TRUE))
        )

    })




    output$inspect_tbl <- renderReactable({

      reactable(inspect_tbl, 'wine',
                fontSize = '12px',
                details = function(index){
                  dropdown <- wine_tbl |> filter(Type==inspect_tbl$Type[index],
                                                 VarietalInspect==inspect_tbl$VarietalInspect[index]) |>
select(Wine, Price, Vnt, Location, Country, Region, SubRegion, Store, Purchased)

                  htmltools::div(style='padding: 1 rem',
                                 reactable::reactable(dropdown,
                                           outlined = TRUE,
                                           columns = list(Wine = colDef(html = T),
                                                          Price = colDef(format = colFormat(prefix = "$",digits = 0)))))
                },

                searchable = TRUE,
                columns = list(
                  Type = colDef(show = FALSE),
                  VarietalInspect = colDef(name = 'Varietal'),
                  TypeColor = colDef(name = 'Type', html = TRUE),
                  `% Cold` = colDef(format = colFormat(perc = TRUE,digits = 0))),
                rowStyle = function(index) {
                  if (inspect_tbl[index, "% Cold"] < 0.1 | inspect_tbl[index,"Cold"]<=1) {
                    list(fontWeight = "bold",
                         color = "darkred")
                  }
                  }
      )

    })


    output$room_tbl <- renderReactable({

      reactable(room_tbl, 'wine',
                fontSize = '12px',
                details = function(index){
                  dropdown <- room_tbl_dropdown |> filter(Location==room_tbl$Location[index])

                  htmltools::div(style='padding: 1 rem',
                                 reactable::reactable(dropdown|>
                                                        select(-Location),
                                                      outlined = TRUE,
        details = function(index2){
          dropdown2 <- room_tbl_dropdown2 |>
                        filter(Location == dropdown$Location[index2],
                               Bin == dropdown$Bin[index2]) |>
                        select(-Location, -Bin) |> select(Wine, everything())

          htmltools::div(style='padding: 1 rem',
                         reactable::reactable(dropdown2,
                                              outlined = TRUE,
                                              columns = list(
                                                Wine = colDef(html = TRUE),
                                                Price = colDef(format = colFormat(prefix = "$",digits = 0)))))
          }
        )
        )
                },
                searchable = TRUE
      )

    })




})
