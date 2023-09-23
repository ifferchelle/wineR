library(dplyr)
library(httr)
library(tidyr)
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


wine_consumed <- wine_bottles %>%
  filter(BottleState==0) %>%
  transmute(iWine,
            Wine, Locale,
            Store,
            `Consumption Date` = as.Date(ConsumptionDate,
                                         "%m/%d/%Y"),
            Note = ConsumptionType,
            Quantity,
            Price = BottleCost) %>%
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
  summarise(Store = toString(sort(unique(Store))),
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
  summarise(Location = toString(sort(unique(paste0(Location,
                                       ifelse(Quantity > 1,
                                              paste0(' (',Quantity,')'),
                                              ''))))),
            Qty = sum(Quantity),
            Type = toString(sort(unique(Type))),
            Varietal = toString(sort(unique(Varietal))),
            Blend = any(Blend, na.rm = T),
            Price = round(max(Price, na.rm = T)),
            #Producer = toString(sort(unique(Producer))),

            Country = toString(sort(unique(Country))),
            Region = toString(sort(unique(Region))),
            SubRegion = toString(sort(unique(SubRegion))),

            Begin = min(BeginConsume, na.rm = T),
            End= max(EndConsume, na.rm = T),
            Rating = round(mean(CT, na.rm = T)),

            Store = toString(sort(unique(Store))),
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
  # select(-iWine) %>%
  arrange(desc(Qty),Vnt,End)


consumed_tbl <- wine_consumed %>%
  mutate(Wine = mapply(x = iWine, y = Wine,
                       function(x, y)paste0(htmltools::tags$a(href = paste0(consumed_link_fmt,x),
                                                              target = "_blank", y),
                                            collapse = '')),
         Date = `Consumption Date`) %>%
  arrange(desc(Date)) %>%
  select(#-iWine,
    -Date)

