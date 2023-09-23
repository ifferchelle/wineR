library(readr)
library(tidyverse)
library(reactable)
library(crosstalk)
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



library(htmltools)
library(shiny)
library(httr)

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

wine_consumed <- wine_bottles %>%
  filter(BottleState==0) %>%
  transmute(iWine,
            Wine, Locale,
            Store,
            `Consumption Date` = as.Date(ConsumptionDate,
                                      "%m/%d/%Y"),
            Note = ConsumptionType) %>%
    arrange(desc(`Consumption Date`)) %>%
    separate(col = Locale,sep = ', ',extra = 'merge',
             fill = 'right',remove = T,
             into = c('Country', 'Region', 'SubRegion',
                                    'Appellation')) %>%
  mutate(SubRegion = case_when(!is.na(SubRegion) & !is.na(Appellation)~
                                 paste0(SubRegion,' - ',Appellation),
                               !is.na(SubRegion)~SubRegion,
                               !is.na(Appellation)~Appellation,
                               TRUE~'')) %>%
  select(-Appellation)

wine_purchased <- wine_bottles %>% filter(BottleState!=0) %>%
  transmute(iWine, Store,
            `Purchase Date` = as.Date(PurchaseDate,"%m/%d/%Y")
            ) %>%
  arrange(Store, `Purchase Date`) %>%
  group_by(iWine) %>%
  summarise(Store = c_unique(Store),
            Purchased = max(`Purchase Date`))

wine_all <- wine

#CT = community tasting rating

wine <- wine_all %>%
  mutate(Quantity = Quantity + ifelse(is.na(Pending),0,Pending),
         Location = ifelse(is.na(Location) & !is.na(Pending),
                           'Pending',
                           Location)) %>%
  select(iWine,
         Wine, Type, Varietal, MasterVarietal,
         Size, Price, Valuation, Producer,
         BeginConsume,
         EndConsume, Vintage,
         Country, Region, SubRegion, Appellation,
         Quantity,
         Location, Bin,
         CT) %>%
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

wine_tbl <- wine %>%
  mutate(Price = ifelse(Price==0 | is.na(Price),
                        Valuation,
                        Price),
         Type = case_when(grepl('Sparkling', Type)~'Sparkling',
                          grepl('Sweet|Dessert|Fortified|Port|Fruit',Type)~'Port/Sweet',
                          Type=='Orange'~'Rosé',
                          TRUE~Type),
         Wine = mapply(x = iWine, y = Wine,
                       function(x, y)paste0(htmltools::tags$a(href = paste0(app_link_fmt,x),
                                     target = "_blank", y),
                                     htmltools::tags$a(href = paste0(desk_link_fmt,x),
                                                                  target = "_blank", y),
                                     collapse = '')),
         Blend = ifelse(grepl('Blend',MasterVarietal),T,F),
         Varietal = MasterVarietal,
         Location = paste0(ifelse(!is.na(Location),
                                  Location,
                                  ''),
                           ifelse(!is.na(Bin),
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
         ) %>%
  # select(-iWine, -Valuation,
  #        -Size, -MasterVarietal,
  #        -Bin, -Appellation) %>%
  group_by(iWine, Wine, Vnt) %>%
  summarise(Location = c_unique(paste0(Location,
                                       ifelse(Quantity > 1,
                                              paste0(' (',Quantity,')'),
                                              ''))),
            Qty = sum(Quantity),
            Type = c_unique(Type),
            Varietal = c_unique(Varietal),
            Blend = any(Blend, na.rm = T),
            Price = round(max(Price, na.rm = T)),
            #Producer = c_unique(Producer),

            Country = c_unique(Country),
            Region = c_unique(Region),
            SubRegion = c_unique(SubRegion),

            Begin = min(BeginConsume, na.rm = T),
            End= max(EndConsume, na.rm = T),
            Rating = round(mean(CT, na.rm = T)),

            Store = c_unique(Store),
            Purchased = max(Purchased)

            ) %>%
  mutate(Price = ifelse(is.infinite(Price), NA, Price),
         Begin = ifelse(is.infinite(Begin), NA, Begin),
         End = ifelse(is.infinite(End), NA, End),
         Rating = ifelse(is.infinite(Rating), NA, Rating),
         Rating = mapply(x = iWine, y = Rating,
                         function(x, y)ifelse(is.na(y),'',
                                              paste0(htmltools::tags$a(href = paste0(rating_link_fmt,x),
                                                                target = "_blank", y),
                                                     collapse = ''))),
         Rating = ifelse(Rating!='', Rating, NA),
         Blend = ifelse(Blend, 'Blend', 'Single Varietal')) %>%
  ungroup() %>%
  select(-iWine) %>%
  arrange(desc(Qty),Vnt,End)


consumed_tbl <- wine_consumed %>%
  mutate(Wine = mapply(x = iWine, y = Wine,
                          function(x, y)paste0(htmltools::tags$a(href = paste0(consumed_link_fmt,x),
                                                                 target = "_blank", y),
                                               collapse = '')),
        Date = `Consumption Date`) %>%
  arrange(desc(Date)) %>%
  select(-iWine, -Date)

shared_data <- SharedData$new(wine_tbl) #not reactive.

shared_data_consumed <- SharedData$new(consumed_tbl)

gg <- wine_tbl %>% group_by(Varietal, Type) %>%
  summarise(Qty = sum(Qty)) %>%
  ggplot(aes(x = Varietal, y = Qty, fill = Type)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c('Red' = '#722f37',
                               'White' = '#F3DB74',
                               'Rosé' = '#EDAEC0',
                               'Sparkling' = '#C0C0C0',
                               'Port/Sweet' = '#000000')) +
  labs(fill = NULL, y = 'Quantity Available',
       x = NULL, title = 'Wine Varietals Available') +
  theme_minimal()



gg2 <- wine_tbl %>% group_by(Country, Region, Type) %>%
  summarise(Qty = sum(Qty)) %>% ungroup() %>%
  mutate(Region = paste(Country,'-',Region)) %>%
  ggplot(aes(x = Region, y = Qty, fill = Type)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c('Red' = '#722f37',
                               'White' = '#F3DB74',
                               'Rosé' = '#EDAEC0',
                               'Sparkling' = '#C0C0C0',
                               'Port/Sweet' = '#000000')) +
  labs(fill = NULL, y = 'Quantity Available',
       x = NULL, title = 'Wines Available by Region') +
  theme_minimal()

wine_cntry <- wine_tbl%>%group_by(Country) %>%
  summarise(Quantity = sum(Qty, na.rm = T)) %>%
  ungroup() %>%
  mutate(Country = case_when(Country%in%c('US', 'USA')~'United States of America',
                             TRUE~Country))
library(countrycode)
eur <- codelist %>% filter(continent=='Europe') %>%
  select(country.name.en)
wine_europe <- wine_tbl%>%
  filter(Country %in% eur$country.name.en) %>%
  group_by(Country) %>%
  summarise(Quantity = sum(Qty, na.rm = T))

wine_us <- wine_tbl%>%
  filter(Country %in% c('US', 'USA', 'United States of America')) %>%
  group_by(Region) %>%
  summarise(Quantity = sum(Qty, na.rm = T))
library(plotly)
library(highcharter)
# Load the world Map data
data(worldgeojson, package = "highcharter")
data(usgeojson, package = "highcharter")
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, wine_cntry, value = "Quantity", joinBy = c('name','Country'),
    name = "Bottles of Wine"
  )  %>%
  hc_colorAxis(stops = color_stops(), min = 0) %>%
  hc_title(text = "World Map") %>%
  hc_subtitle(text = "Wine Available by Country")

hc_eur <- hcmap(map = 'custom/europe', data = wine_europe,
                joinBy = c("name","Country"), value = "Quantity",
                name = "Bottles of Wine") %>%
  hc_colorAxis(stops = color_stops(), min = 0) %>%
  hc_title(text = "Europe Map") %>%
  hc_subtitle(text = "Wine Available by Country")

hc_us <- highchart() %>%
  hc_add_series_map(
    usgeojson, wine_us, value = "Quantity",
    joinBy = c('name','Region'),
    name = "Bottles of Wine"
  )  %>%
  hc_colorAxis(stops = color_stops(), min = 0) %>%
  hc_title(text = "US Map") %>%
  hc_subtitle(text = "Wine Available by State")
