library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(reactable)
library(crosstalk)
library(htmltools)
library(httr)
library(countrycode)
library(plotly)
library(highcharter)
#library(iffer)

#' Concatenate All Unique Values
#' @description Concatenate all unique values in a list
#' @param x the list to uniquely concatenate
#' @param collapse how to separate the elements
#' @param sort whether the unique elements should be sorted
#' @param max maximum number of unique elements to allow in list. Mainly to stop
#'     an accidental concatenation of too many of strings.
#' @export
c_unique <- function(x, collapse = ', ',
                     sort = T, max = 100){
  if(sort) y <- sort(unique(x)) else y <- unique(x)
  if(length(y)>max) stop('More than ',max,' elements. Increase max or check list.')
  paste0(y, collapse = collapse)
}

#' reactable Updated Defaults
#' @description Reactable with striped rows, highlight on hover, and Cone Health
#'      color and font.
#' @param data the data
#' @param color color I want for the reactable. so far, one of 'wine', 'mint'
#' @param striped whether or not to stripe the columns, default \code{TRUE}
#' @param highlight whether or not to highlight entire row on hover, default \code{TRUE}
#' @param fontSize default \code{'14px'}
#' @param ... with other options
#' @export
reactable <- function(data,
                      color = 'mint',
                      striped = T,
                      highlight = T,
                      fontSize = '16px',
                      ...){

  requireNamespace("reactable")

  if(color == 'mint'){
    thm <- reactableThemeMint(fontSize = fontSize)
  } else if(color == 'wine'){
    thm <- reactableThemeWine(fontSize = fontSize)
  } else{
    stop('color must be one of: mint, wine')
  }

  reactable::reactable(data,
                       striped = T,
                       highlight = T,
                       theme = thm,
                       ...)
}


#' Reactable Theme with WINE Colors
#' @description Default theme for reactables. Applied automatically when calling
#'       \code{iffer::reactable(color = 'wine')}
#' @param borderColor border color
#' @param stripedColor striped columns color
#' @param highlightColor color of row on highlight
#' @param cellPadding table cell padding
#' @param headerBackgroundColor header background color
#' @param headerColor header font color
#' @param fontFamily table font
#' @param fontSize table font size
#' @param searchWidth width of search when reactable argument \code{searchable = TRUE}
#' @param ... other parameters passed to \code{reactableTheme}
#' @export
reactableThemeWine <- function(borderColor = "#dfe2e5",
                               stripedColor = "#f6f8fa",
                               highlightColor = "#f0f5f9",
                               cellPadding = "8px 12px",
                               headerBackgroundColor = "#722f37",
                               headerColor = "#FFFFFF",
                               fontFamily = "Segoe UI, Calibri, Tahoma, Arial, sans-serif",
                               fontSize = '16px',
                               searchWidth = "100%",
                               ...){

  requireNamespace("reactable")

  reactable::reactableTheme(
    borderColor = borderColor,
    stripedColor = stripedColor,
    highlightColor = highlightColor,
    cellPadding = cellPadding,
    headerStyle = list(backgroundColor = headerBackgroundColor,
                       color = headerColor),
    style = list(fontFamily = fontFamily,
                 fontSize = fontSize),
    searchInputStyle = list(width = searchWidth),
    ...
  )
}



#' Reactable Theme with MINT Colors
#' @description Default theme for reactables. Applied automatically when calling
#'       \code{iffer::reactable(color = 'mint')}
#' @param headerBackgroundColor header background color
#' @param headerColor header font color
#' @param ... other parameters passed to \code{reactableTheme}
#' @export
reactableThemeMint <- function(headerBackgroundColor = '#98ff98',
                               headerColor = '#444444',
                               ...){

  requireNamespace("reactable")

  reactableThemeWine(
    headerBackgroundColor = headerBackgroundColor,
    headerColor = headerColor,
    ...
  )
}



#local
#csv_file <- "C:\\Users\\iffer\\Nextcloud\\winecellar\\cellar.csv"
#wine <- readr::read_csv(file = csv_file)


# user <- 'winecellar'
# pw <- '3DAaj-SkLJe-j5FFp-jT3bN-CjWTN'
# url <- "https://nc.jenniferandadam.net/remote.php/dav/files/winecellar/winecellar/cellar.csv"
#
# req <- GET(url, authenticate(user, pw))

update_url <- "https://prod-24.eastus.logic.azure.com:443/workflows/53993553d4e6454b97493ed5e59c5126/triggers/manual/paths/invoke?api-version=2016-10-01&sp=%2Ftriggers%2Fmanual%2Frun&sv=1.0&sig=DSz-A8cKw7mrqL3n77H0_GKTOvHQpqwywSsdwL4XgQs"

url <- "https://awctstorageacct.blob.core.windows.net/ctblob/cellar.csv"
url_bottles <- "https://awctstorageacct.blob.core.windows.net/ctblob/bottles.csv"
req <- GET(url)
if(req$status_code==200){
  wine <- content(req, type = "text/csv", encoding = 'UTF-8')
} else stop('Bad request.')

req2 <- GET(url_bottles)
if(req2$status_code==200){
  wine_bottles <- content(req2, type = "text/csv", encoding = 'UTF-8')
} else stop('Bad request.')


# g(wine)
# table(wine$Varietal)
# table(wine$MasterVarietal)
# table(wine$Type)

wine_consumed <- wine_bottles |>
  filter(BottleState==0) |>
  rowwise() |>
  mutate(Note = toString(na.omit(c(ConsumptionType, ConsumptionNote)))) |>
  ungroup() |>
  transmute(iWine,
            Wine, Type, Locale,
            Store,
            `Consumption Date` = as.Date(ConsumptionDate,
                                         "%m/%d/%Y"),
            Price = scales::dollar(BottleCost, accuracy = 1),
            Note) |>
  arrange(desc(`Consumption Date`)) |>
  separate(col = Locale,sep = ', ',extra = 'merge',
           fill = 'right',remove = T,
           into = c('Country', 'Region', 'SubRegion',
                    'Appellation')) |>
  mutate(SubRegion = case_when(!is.na(SubRegion) & !is.na(Appellation)~
                                 paste0(SubRegion,' - ',Appellation),
                               !is.na(SubRegion)~SubRegion,
                               !is.na(Appellation)~Appellation,
                               TRUE~'')) |>
  select(-Appellation)

wine_purchased <- wine_bottles |> filter(BottleState!=0) |>
  transmute(iWine, Store,
            `Purchase Date` = as.Date(PurchaseDate,"%m/%d/%Y")
  ) |>
  arrange(Store, `Purchase Date`) |>
  group_by(iWine) |>
  summarise(Store = c_unique(Store),
            Purchased = max(`Purchase Date`))

wine_all <- wine

#CT = community tasting rating

wine <- wine_all |>
  mutate(Quantity = Quantity + ifelse(is.na(Pending),0,Pending),
         Location = ifelse(is.na(Location) & !is.na(Pending),
                           'Pending',
                           Location),
         Cold = case_when(Location %in% c('A Fridge', 'Garage') ~ Quantity,
                          TRUE ~ 0),
         Bin = ifelse(is.na(Bin), 'N/A', Bin)) |>
  select(iWine,
         Wine, Type, Varietal, MasterVarietal,
         Size, Price, Valuation, Producer,
         BeginConsume,
         EndConsume, Vintage,
         Country, Region, SubRegion, Appellation,
         Quantity,
         Location, Bin, Cold,
         CT) |>
  left_join(wine_purchased, by = 'iWine')
# Sort options
#what type of wine? ------
## Wine
## Type (red/white/rose, sparkling, dessert, fortified)
# make orange = rose
## Varietal/Master Varietal
#wine details -----
## Size
## Price / Valuation
##
#how old in the wine? ------
## BeginConsume/EndConsume
## Vintage
#where is the wine from? ------
## Country (map view?)
## Region
#how much do we have? ------
## Quantity
#where is the wine? ------
## Location / Bin


app_link_fmt <- c('cellartracker://load/m/wines/')
#desk_link_fmt <- c('https://www.cellartracker.com/wine.asp?iWine=')
desk_link_fmt <- c('https://www.cellartracker.com/barcode.asp?iWine=')

rating_link_fmt <- c('https://www.cellartracker.com/notes.asp?iWine=')

consumed_link_fmt <- c('https://www.cellartracker.com/myconsumed.asp?iWine=')

color_img <- function(color){
  img(src = 'glass.svg', class = paste0('filter-',color), height = '15px')
}

price_labels <- c('Under $15', '$15-$24', '$25-$34','$35-$49','$50-$74',
                  '$75-$99','$100-$149','$150+')

wine_tbl <- wine |> ungroup() |>
  mutate(Price = ifelse(Price==0 | is.na(Price),
                        Valuation,
                        Price),
         Type = case_when(grepl('Sparkling', Type)~'Sparkling',
                          grepl('Sweet|Dessert|Fortified|Port|Fruit',Type)~'Port/Sweet',
                          Type=='Orange'~'Rosé',
                          TRUE~Type),
         Color = case_when(Type=='Rosé'~'rose',
                           Type=='Port/Sweet'~'sweet',
                           TRUE~tolower(Type)),
         TypeColor = mapply(x = Type, y=Color,
                            function(x, y)paste0(color_img(y),
                                                 htmltools::tags$span(x),
                                                 collapse = '')),
         Wine = mapply(x = iWine, y = Wine, z=Color,
                       function(x, y, z)paste0(color_img(z),
                                               htmltools::tags$a(href = paste0(app_link_fmt,x),
                                                                 target = "_blank", y),
                                               htmltools::tags$a(href = paste0(desk_link_fmt,x),
                                                                 target = "_blank", y),
                                               collapse = '')),
         Blend = ifelse(grepl('Blend',MasterVarietal),T,F),
         Varietal = MasterVarietal,
         Location = paste0(ifelse(!is.na(Location),
                                  Location,
                                  ''),
                           ifelse(!is.na(Bin) & Bin!='N/A',
                                  paste0(' - ', Bin),
                                  '')),
         SubRegion = ifelse(SubRegion=='Unknown',NA,
                            SubRegion),
         Appellation = ifelse(Appellation=='Unknown',NA,
                              Appellation),
         SubRegion = case_when(!is.na(SubRegion) & !is.na(Appellation)~
                                 paste0(SubRegion,' - ',Appellation),
                               !is.na(SubRegion)~SubRegion,
                               !is.na(Appellation)~Appellation,
                               TRUE~''),
         Vnt = ifelse(Vintage == 1001, NA, Vintage)
  ) |>
  # select(-iWine, -Valuation,
  #        -Size, -MasterVarietal,
  #        -Bin, -Appellation) |>
  group_by(iWine, Wine, Vnt, Type, Color, TypeColor) |>
  summarise(Location = c_unique(paste0(Location,
                                       ifelse(Quantity > 1,
                                              paste0(' (',Quantity,')'),
                                              ''))),
            Qty = sum(Quantity),
            Cold = sum(Cold),
            # Type = c_unique(Type),
            Varietal = c_unique(Varietal),
            Blend = any(Blend, na.rm = T),
            Price = round(max(Price, na.rm = T)),
            Producer = c_unique(Producer),

            Country = c_unique(Country),
            Region = c_unique(Region),
            SubRegion = c_unique(SubRegion),

            Begin = min(BeginConsume, na.rm = T),
            End= max(EndConsume, na.rm = T),
            Rating = round(mean(CT, na.rm = T)),

            Store = c_unique(Store),
            Purchased = max(Purchased)

  ) |>

  mutate(VarietalInspect = if_else(Type %in% c('Red', 'White'),
                                   Varietal, 'All'),
         Price = ifelse(is.infinite(Price), NA, Price),
         Price2 = cut(ifelse(is.na(Price),0,Price),
                      c(-Inf,14,24,34,49,74,99,149,Inf),
                      price_labels),
         Begin = ifelse(is.infinite(Begin), NA, Begin),
         End = ifelse(is.infinite(End), NA, End),
         Rating = ifelse(is.infinite(Rating), NA, Rating),
         Rating = mapply(x = iWine, y = Rating,
                         function(x, y)ifelse(is.na(y),'',
                                              paste0(htmltools::tags$a(href = paste0(rating_link_fmt,x),
                                                                       target = "_blank", y),
                                                     collapse = ''))),
         Rating = ifelse(Rating!='', Rating, NA),
         Blend = ifelse(Blend, 'Blend', 'Single Varietal'),
         AnyCold = factor(if_else(Cold>0,'Yes', 'No'),
                          levels = c('Yes', 'No'))) |>
  ungroup() |>
  select(-iWine) |>
  arrange(desc(Qty),Vnt,End)

color_cnt <- function(color){
  sum(wine_tbl$Qty[wine_tbl$Color==color])
}

inspect_tbl <- wine_tbl |>
  mutate(VarietalInspect = if_else(Type %in% c('Red', 'White'),
                                   Varietal, 'All')) |>
  group_by(Type, TypeColor, VarietalInspect) |>
  summarise(across(c(Qty, Cold), sum), .groups = 'drop') |>
  filter(Qty!=Cold) |>
  mutate(`% Cold` = Cold/Qty) |>
  arrange(`% Cold`, Cold, desc(Qty))

# capacity <- tribble(~Location, ~Capacity,
#                     'A Fridge -  Top', 55,
#                     'A Fridge - Bottom', 99,
#                     'Rack A', 24,
#                     'Rack B', 24,
#                     'Rack C', 24,
#                     'Rack D', 24)

capacity <-bind_rows(tibble(Location = 'A Fridge', Bin = sprintf('%02.f',1:14)),
                     tibble(Location = 'Garage', Bin = 'N/A'),
                     expand.grid(Location = paste('Rack', c('A','B','C','D','E','F','G')),
                                 Bin = c(as.character(1:6),'Top'))) |>
  mutate(Location2 = case_when(Location=='A Fridge' & as.numeric(Bin) <=5 ~ 'A Fridge -  Top',
                               Location=='A Fridge' ~ 'A Fridge - Bottom',
                               TRUE~Location),
         Capacity = case_when(grepl('^Rack',Location) & Bin!='Top' ~ 4,
                              Location=='A Fridge' & Bin %in% c('01','10') ~ 11 + 9,
                              Location=='A Fridge' ~ 11,
                              TRUE ~ 0))

room_tbl <- wine_all |>
  mutate(Location2 = case_when(Location=='A Fridge' & as.numeric(Bin) <=5 ~ 'A Fridge -  Top',
                               Location=='A Fridge' ~ 'A Fridge - Bottom',
                               TRUE~Location)) |>
  group_by(Location, Location2) |>
  summarise(Filled = sum(Quantity)) |>
  ungroup() |>
  right_join(capacity |> group_by(Location2) |>
               summarise(Capacity = sum(Capacity)), by = c('Location2')) |>
  mutate(Filled = ifelse(is.na(Filled),0, Filled),
         Space = pmax(0,Capacity - Filled)) |>
  select(-Location) |> rename(Location = Location2)|>
  arrange(Location)

room_tbl_dropdown <- wine_all |>
  mutate(Location2 = case_when(Location=='A Fridge' & as.numeric(Bin) <=5 ~ 'A Fridge -  Top',
                               Location=='A Fridge' ~ 'A Fridge - Bottom',
                               TRUE~Location)) |>
  group_by(Location, Location2, Bin) |>
  summarise(Filled = sum(Quantity)) |>
  ungroup() |>
  mutate(Bin = ifelse(is.na(Bin), 'N/A', Bin)) |>
  right_join(capacity, by = c('Location', 'Location2', 'Bin')) |>
  mutate(Filled = ifelse(is.na(Filled),0, Filled),
         Space = pmax(0,Capacity - Filled)) |>
  select(-Location) |> rename(Location = Location2) |>
  filter(!(Filled==0 & Space==0 & Bin=='Top')) |>
  arrange(Location, Bin)

room_tbl_dropdown2 <- wine_bottles |>
  mutate(Price = BottleCost) |>
  filter(BottleState!=0) |> #consumed
  mutate(Type = case_when(grepl('Sparkling', Type)~'Sparkling',
                          grepl('Sweet|Dessert|Fortified|Port|Fruit',Type)~'Port/Sweet',
                          Type=='Orange'~'Rosé',
                          TRUE~Type),
         Color = case_when(Type=='Rosé'~'rose',
                           Type=='Port/Sweet'~'sweet',
                           TRUE~tolower(Type)),
         Wine = mapply(x = iWine, y = Wine, z=Color,
                       function(x, y, z)paste0(color_img(z),
                                               htmltools::tags$a(href = paste0(app_link_fmt,x),
                                                                 target = "_blank", y),
                                               htmltools::tags$a(href = paste0(desk_link_fmt,x),
                                                                 target = "_blank", y),
                                               collapse = '')),
         Location2 = case_when(Location=='A Fridge' & as.numeric(Bin) <=5 ~ 'A Fridge -  Top',
                               Location=='A Fridge' ~ 'A Fridge - Bottom',
                               TRUE~Location),
         Bin = ifelse(is.na(Bin), 'N/A', Bin)) |>

  select(Location = Location2, Price, Bin, Wine, Vnt = Vintage, Varietal, Country, Region, SubRegion, Store, Purchased = PurchaseDate)

consumed_tbl <- wine_consumed |>
  mutate(Type = case_when(grepl('Sparkling', Type)~'Sparkling',
                          grepl('Sweet|Dessert|Fortified|Port|Fruit',Type)~'Port/Sweet',
                          Type=='Orange'~'Rosé',
                          TRUE~Type),
         Color = case_when(Type=='Rosé'~'rose',
                           Type=='Port/Sweet'~'sweet',
                           TRUE~tolower(Type)),
         Wine = mapply(x = iWine, y = Wine, z=Color,
                       function(x, y, z)paste0(color_img(z),
                                               htmltools::tags$a(href = paste0(app_link_fmt,x),
                                                                 target = "_blank", y),
                                               htmltools::tags$a(href = paste0(desk_link_fmt,x),
                                                                 target = "_blank", y),
                                               collapse = '')),
         Date = `Consumption Date`) |>
  arrange(desc(Date)) |>
  select(-iWine, -Date,-Type,-Color)

shared_data <- SharedData$new(wine_tbl) #not reactive.

shared_data_consumed <- SharedData$new(consumed_tbl)

gg <- wine_tbl |> mutate(`Not Cold` = Qty-Cold) |>
  pivot_longer(cols = c(Cold, `Not Cold`)) |>
  arrange(desc(Varietal)) |>
  mutate(Varietal = fct_inorder(Varietal)) |>
  group_by(Varietal, Type, name) |>
  summarise(Qty = sum(value)) |>
  ggplot(aes(x = Varietal, y = Qty, fill = Type)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c('Red' = '#722f37',
                               'White' = '#F3DB74',
                               'Rosé' = '#EDAEC0',
                               'Sparkling' = '#C0C0C0',
                               'Port/Sweet' = '#000000')) +
  labs(fill = NULL, y = 'Quantity Available',
       x = NULL, title = 'Wine Varietals Available') +
  facet_grid(.~name) +
  theme_minimal()



gg2 <- wine_tbl |>
  mutate(`Not Cold` = Qty-Cold) |>
  pivot_longer(cols = c(Cold, `Not Cold`)) |>
  group_by(Country, Region, Type, name) |>
  summarise(Qty = sum(value)) |> ungroup() |>
  mutate(Region = paste(Country,'-',Region)) |>
  arrange(desc(Region)) |>
  mutate(Region = fct_inorder(Region)) |>
  ggplot(aes(x = Region, y = Qty, fill = Type)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c('Red' = '#722f37',
                               'White' = '#F3DB74',
                               'Rosé' = '#EDAEC0',
                               'Sparkling' = '#C0C0C0',
                               'Port/Sweet' = '#000000')) +
  labs(fill = NULL, y = 'Quantity Available',
       x = NULL, title = 'Wines Available by Region') +
  theme_minimal() +
  facet_grid(.~name)

price_graph <- function(varietal = NULL, type = NULL,
                        vint_low = NULL, vint_high = NULL,
                        include_nv = TRUE){

  plot_dat <- wine_tbl

  if(!is.null(varietal)) plot_dat <- plot_dat |> filter(Varietal %in% varietal)
  if(!is.null(type)) plot_dat <- plot_dat |> filter(Type %in% type)
  if(!is.null(vint_low)) plot_dat <- plot_dat |> filter(Vnt >= vint_low | is.na(Vnt))
  if(!is.null(vint_high)) plot_dat <- plot_dat |> filter(Vnt <= vint_high | is.na(Vnt))
  if(!include_nv) plot_dat <- plot_dat |> filter(!is.na(Vnt))

  if(nrow(plot_dat)<1) return(NULL)


  gg3 <- plot_dat |>
    mutate(`Not Cold` = Qty-Cold,
           Price = cut(ifelse(is.na(Price),0,Price),
                       c(-Inf,14,24,34,49,74,99,149,Inf),
                       price_labels) |> factor(levels = rev(price_labels))
    ) |>
    pivot_longer(cols = c(Cold, `Not Cold`)) |>
    group_by(Price, Type, name) |>
    reframe(Qty = sum(value)) |>
    complete(Price, Type, name, fill = list(Qty = 0)) |>
    ggplot(aes(x = Price, y = Qty, fill = Type)) +
    geom_col() + coord_flip() +
    scale_fill_manual(values = c('Red' = '#722f37',
                                 'White' = '#F3DB74',
                                 'Rosé' = '#EDAEC0',
                                 'Sparkling' = '#C0C0C0',
                                 'Port/Sweet' = '#000000')) +
    labs(fill = NULL, y = 'Quantity Available',
         x = NULL, title = 'Wines Available by Price') +
    theme_minimal() +
    facet_grid(.~name)
  return(gg3)
}

wine_cntry <- wine_tbl|>group_by(Country) |>
  summarise(Quantity = sum(Qty, na.rm = T)) |>
  ungroup() |>
  mutate(Country = case_when(Country%in%c('US', 'USA')~'United States of America',
                             TRUE~Country))

eur <- codelist |> filter(continent=='Europe') |>
  select(country.name.en)
wine_europe <- wine_tbl|>
  filter(Country %in% eur$country.name.en) |>
  group_by(Country) |>
  summarise(Quantity = sum(Qty, na.rm = T))

wine_us <- wine_tbl|>
  filter(Country %in% c('US', 'USA', 'United States of America')) |>
  group_by(Region) |>
  summarise(Quantity = sum(Qty, na.rm = T))

# Load the world Map data
data(worldgeojson, package = "highcharter")
data(usgeojson, package = "highcharter")
hc <- highchart() |>
  hc_add_series_map(
    worldgeojson, wine_cntry, value = "Quantity", joinBy = c('name','Country'),
    name = "Bottles of Wine"
  )  |>
  hc_colorAxis(stops = color_stops(), min = 0) |>
  hc_title(text = "World Map") |>
  hc_subtitle(text = "Wine Available by Country")

hc_eur <- hcmap(map = 'custom/europe', data = wine_europe,
                joinBy = c("name","Country"), value = "Quantity",
                name = "Bottles of Wine") |>
  hc_colorAxis(stops = color_stops(), min = 0) |>
  hc_title(text = "Europe Map") |>
  hc_subtitle(text = "Wine Available by Country")

hc_us <- highchart() |>
  hc_add_series_map(
    usgeojson, wine_us, value = "Quantity",
    joinBy = c('name','Region'),
    name = "Bottles of Wine"
  )  |>
  hc_colorAxis(stops = color_stops(), min = 0) |>
  hc_title(text = "US Map") |>
  hc_subtitle(text = "Wine Available by State")


# consumption trends
consume_sum <- wine_bottles |> filter(BottleState==0) |>
  mutate(Type = case_when(grepl('Sparkling', Type)~'Sparkling',
                          grepl('Sweet|Dessert|Fortified|Port|Fruit',Type)~'Port/Sweet',
                          Type=='Orange'~'Rosé',
                          grepl('^White', Type) ~ 'White',
                          TRUE~Type),
         Varietal = case_when(!Type %in% c('Red', 'White')~ Type,
                              TRUE ~ MasterVarietal),
         Type = case_when(!Type %in% c('Red', 'White')~ 'Other',
                          TRUE ~ Type),
         Month = lubridate::floor_date(as.Date(ConsumptionDate,
                                               "%m/%d/%Y"), 'month')) |>
  group_by(Month, Varietal, Type) |>
  count() |> ungroup() |>
  complete(nesting(Type, Varietal), Month, fill = list(n = 0)) |>
  group_by(Type, Varietal) |>
  mutate(n_tot = sum(n)) |> ungroup() |>
  mutate(DiffYr = as.numeric(difftime(max(Month), min(Month), units = 'days'))/365,
         Varietal = case_when(!Type %in% c('Red', 'White') ~ Varietal,
                              ceiling(n_tot/DiffYr) < 1.1 ~ 'Other',
                              TRUE~ Varietal)) |>
  group_by(Type, Varietal, Month) |>
  summarise(n = sum(n), .groups = 'drop')


gg3 <- consume_sum |>
  ggplot(aes(x = Month, y = n, color = Varietal)) +
  geom_line() +
  facet_grid(Type~., scales = 'free', space = 'free')
# plotly::ggplotly(gg3)
