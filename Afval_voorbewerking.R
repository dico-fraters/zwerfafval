#
# Afvalgegevens voorbewerking
# Versie: 10 januari 2024
#

# 1. Klaarmaken werkomgeving ----

# Opschonene geheugen
# Empty memory
rm(list=ls())
ls()

# Inladen of uitladen van pakketten:
# uitladen
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")

# inladen
if (!require("pacman")) install.packages("pacman")
pacman::p_load(Rmisc)
pacman::p_load(tidyverse, sf, stars, readxl, writexl)

# !!!! Geef datum en tijd bestand !!!! ----
datumbestand <- "9-1-2024"  # voer naam in zoals in .csv-bestand is gegeven
tijdbestand <- "12 16 09"

getwd()
#setwd("~/R_scripts/zwerfafval")

# Uit- of aanzetten afdrukken figuren tbv test (standaard "nee")
druk_figuur_af <- "nee"  # opties: "ja" of "nee"

#
# 2. Inlezen en voorbewerken gegevensbestanden (indien niet vernieuwd dit overslaan en naar 3.)  ----
# inlezen gemeenten / buurten
buurt <- st_read("Data/geo/WijkBuurtkaart_2023_v0/buurt_2023_v0.shp")
names(buurt)
buurt <- buurt %>% 
  select(BU_CODE, BU_NAAM, WK_CODE, WK_NAAM, GM_CODE, GM_NAAM, #POSTCODE,
                          Shape_Leng, Shape_Area, geometry) 

if(druk_figuur_af=="ja"){
  # plot plateau (inclusief afkorting namen)
  plot(buurt["BU_NAAM"])
  
  # Laat alle buurten zien
  buurt %>% distinct(GM_NAAM)
}


# Selecteer alleen De Bilt en omliggende gemeenten:
DeBilt_eo <- buurt[buurt$GM_NAAM %in% 
                     c("De Bilt", "Baarn", "Hilversum", "Maarssen", 
                       "Soest", "Stichtse Vecht", "Utrecht", 
                       "Wijdemeren", "Zeist"),]

if(druk_figuur_af=="ja") {
  par(mar=c(4,4,4,2), mai=c(0,0,0,0))
  plot(DeBilt_eo["GM_NAAM"])
  ggplot()+geom_sf(data = DeBilt_eo)+geom_sf(data = DeBilt_eo)
}


# Inlezen van afval
z <- read.csv(paste0("Data/zwerfafvalkompas-afvalstukken-",datumbestand, " ",tijdbestand, ".csv"),
              header=TRUE, na.strings = "")

#str(z)

# kortere namen (kleine letters)
names(z) <- c("afval", "uitdaging", "main", "sub", "cat", "object", "material", 
              "brand", "remark", "date", "loclati", "loclong", "url", "raper.id")

# komma in coordinaten vervangen door punt
z$loclati <- as.numeric(gsub(",", ".", z$loclati))
z$loclong <- as.numeric(gsub(",", ".", z$loclong))

# laad functie voor omzetten naar Amersfootse coord
source("Trans_WSG84-to-Amersfoort-vv.R")

# Omzetten WSG naar Amersfoortse coordinaten
rd.coord <- wgs84_to_rd(z$loclong, z$loclati)
#str(rd.coord)
z <- z %>% mutate(
  xcoord = rd.coord$x,
  ycoord = rd.coord$y
)

# Aanmaken parameters op basis van datum
#z$date
# datum en tijd splitsen
z <- z %>% separate(col=date, into=c("datum", "tijd"), sep=" ")
# dag, maand en jaar splitsen
z <- z %>% separate(col=datum, into=c("dag", "maand", "jaar"), sep="-", remove=FALSE)

# aanpassen modus van variabelen
z <- z %>% mutate(
  datum = as.Date(datum, format = "%d-%m-%Y"),
  loclati = as.numeric(loclati),
  loclong = as.numeric(loclong)
)
z <- z %>% mutate_if(is.character, as.factor)

#str(z)

# Toevoegen rapernummer (nr) (als vereenvoudiging van raper.id)
if("nr" %in% colnames(z)==FALSE) {
  # maak rapernummer
  z <- z %>% mutate(
    nr = as.character(NA)
  )
}
#str(z)

# Zoek bestaande raper.id
raper.id.uniek <- z %>% distinct(raper.id)
# Zoek bestaande rapernummers (nr)
if("nr" %in% colnames(z)) {
  nr.uniek <- z %>% filter(is.na(nr)==FALSE) %>% distinct(nr)
} else {
  z <- z %>% mutate(
    nr = as.numeric(NA)
  )
  nr.uniek <- z %>% filter(is.na(nr)==FALSE) %>% distinct(nr)
}


if(nrow(nr.uniek)==0) {
  raper.id.uniek <- raper.id.uniek %>% mutate(
    nr = paste0("r", as.character(c(1001:(1000+nrow(raper.id.uniek)))))
  )
  w <- left_join(z, raper.id.uniek, by="raper.id", copy=FALSE)
  (df_merged <- (structure(list(
    nr.x = as.vector(w$nr.x),
    nr.y = as.vector(w$nr.y)), 
   row.names = c(NA, -nrow(z)), class = c("tbl_df", "tbl", "data.frame"))))
  
  xn <- names(df_merged)[endsWith(names(df_merged),".x")]
  
  nms <- str_replace(string = xn,
                     pattern = ".x",
                     replacement="")
  test <- map_dfc(nms,
          ~ coalesce(df_merged[[paste0(.,".x")]],
                     df_merged[[paste0(.,".y")]]
          )) %>% setNames(nms)
  z$nr <- test$nr
  
} else {
  raper.id.uniek <- z %>% filter(is.na(nr)==TRUE) %>% distinct(raper.id)
  start <- max(as.integer(as.character(sub("r", "", z$nr))))
  raper.id.uniek <- raper.id.uniek %>% mutate(
    nr = paste0("r", as.character(c((start+1):((start+1)+nrow(raper.id.uniek)))))
  )
  w <- left_join(z, raper.id.uniek, by="raper.id", copy=FALSE)
  (df_merged <- (structure(list(
    nr.x = as.vector(w$nr.x),
    nr.y = as.vector(w$nr.y)), 
    row.names = c(NA, -nrow(z)), class = c("tbl_df", "tbl", "data.frame"))))
  
  xn <- names(df_merged)[endsWith(names(df_merged),".x")]
  
  nms <- str_replace(string = xn,
                     pattern = ".x",
                     replacement="")
  test <- map_dfc(nms,
                  ~ coalesce(df_merged[[paste0(.,".x")]],
                             df_merged[[paste0(.,".y")]]
                  )) %>% setNames(nms)
  z$nr <- test$nr
  
}

#z %>% distinct(nr)

# Aanmaken variabele met namen van rapers voor zover bekend ----
z <- z %>% mutate(
  raper.naam = as.factor(if_else(raper.id=="40CD23E3D355A9D43AE90C08DFC6876B", "Riemke.Visser",
               if_else(raper.id=="93C6A2DCE3FE497F437E893CDC0DB2B2", "Dico.Fraters",
               if_else(raper.id=="2AEF4A62F64F618CCADF444E255ADE77", "Marieke.Polder", 
               if_else(raper.id=="47088F219BFB494DA66AA2142D00AD72", "Annette.Smeets", "onbekend")))))
  # OOK 2, maar op 12 ipv 16 oktober "05E1B8E2DE4F54116FA7C710F1689C4A"
  # 7 stuks op 11 oktober "30D23EAEA26E770CF305240830C9E32B", 
  # echter maar één persoon extra vermeld in overzicht oktober op Zwerfafvalkompas
)


if(druk_figuur_af=="ja") {
  str(z)
  with(droplevels(z %>% filter(jaar=="2023")), table(raper.id, raper.naam))
  with(droplevels(z %>% filter(jaar=="2022")), table(raper.id, raper.naam))
  with(z, addmargins(table(jaar, raper.naam), FUN=sum))
}

# locatie van afval (x,y)
# afval_loc <- z %>% distinct(afval,jaar,loclati,loclong, .keep_all = F)
afval_loc <- z # %>% distinct(afval,jaar, raper.naam, xcoord,ycoord, .keep_all = F)
afval_loc_sf <- afval_loc %>%  st_as_sf(coords = c("xcoord","ycoord"),
                                       crs = 28992)
if(druk_figuur_af=="ja"){
  # plot afval
  ggplot()+
    #geom_stars(data=ahn5_s, downsample = 20)+
    #scale_fill_gradientn(colours = terrain.colors(6))+
    geom_sf(data=DeBilt_eo, fill = NA, lwd = 1, color = "black")+
    geom_sf(data= st_geometry(afval_loc_sf))+
    ggtitle("coordinaat per gebied")
  
}

# Combineer punten (afval) met de verschillende buurten (join)
loc_in_buurt <- st_join(afval_loc_sf, DeBilt_eo, join = st_intersects)
loc_in_buurt <- loc_in_buurt %>% mutate(in_buurt = ifelse(is.na(BU_NAAM), F, T))
#head(loc_in_buurt)
#str(loc_in_buurt)

if(druk_figuur_af=="ja") {
  # Check jaartallen
  with(z, table(raper.naam, jaar))
  with(loc_in_buurt, table(raper.naam, jaar))
}

# Opslaan van nuttige bestanden
saveRDS(loc_in_buurt, file="Data/afval_coordinaten.rda")
saveRDS(DeBilt_eo, file="Data/DeBilt_eo_poly.rda")
saveRDS(z, file=paste0("Data/afvalgegevens_", datumbestand, ".rda"))

write_xlsx(loc_in_buurt, path=paste0("Data/afvalgegevens_",datumbestand,".xlsx"))
saveRDS(loc_in_buurt, file=paste0("Data/afvalgegevens_geo_", datumbestand, ".rda"))
v <- readRDS(file=paste0("Data/afvalgegevens_geo_", datumbestand, ".rda"))
#str(v)
#with(v, table(raper.naam, jaar))


#
# 3. Verbeteren gegevensbestand ----
#

#
# 3.1 Inlezen en bekijken ---- 
z <- readRDS(file=paste0("Data/afvalgegevens_geo_", datumbestand, ".rda"))

if(druk_figuur_af=="ja") {
  names(z)
  str(z)
  with(z, table(raper.naam, jaar))
  
  levels(as.factor(z$uitdaging))
  levels(as.factor(z$main))        # Hoofdcategorie
  levels(as.factor(z$cat))         # Categorie
  levels(as.factor(z$sub))         # subcategorie
  levels(as.factor(z$object))      # Object
  levels(as.factor(z$material))    # Materiaal
  levels(as.factor(z$brand))       # Merk
  levels(as.factor(z$remark))      # Opmerking
  levels(as.factor(z$raper.naam))  # Naam van raper voor zover bekend
  levels(as.factor(z$nr))          # genummerd id van rappers in De Bilt
  levels(as.factor(z$GM_NAAM))     # Naam van gemeente
  levels(as.factor(z$WK_NAAM))     # Naam van wijk
  levels(as.factor(z$BU_NAAM))     # Naam van de buurt en de verschillende gemeenten
}

#
# 3.2 Verbeteren 'main' ----

if(druk_figuur_af=="ja"){
  levels(z$main)
  with(z, addmargins(table(main, jaar, useNA="ifany"), FUN=sum))
}


# Geuniformeerde 'main' maken (van 19 naar 8)
z <- z %>% mutate(
  main.uni = if_else(main %in% c("BlikOpStuk,Drinks", "checked by Joyce,Drinks", "Drinks"), "drinks", 
             if_else(main %in% c("checked by Joyce,Food", "Food"), "food",
             if_else(main %in% c("checked by Joyce,food & drinks", "Drinks,Food", "Drinks,food & drinks",
                                 "food & drinks", "Food,food & drinks"),"food&drinks", 
             if_else(main %in% c("BlikOpStuk,other mc", "checked by Joyce,other mc", "other mc"),
                     "other.mc", 
             if_else(main %in% c("Drinks,other mc"), "drinks&other.mc", 
             if_else(main %in% c("Food,other mc"), "food&other.mc", 
             if_else(main %in% c("food & drinks,other mc", "Food,food & drinks,other mc", 
                                 "Drinks,Food,other mc"), "food&drinks&other.mc", 
             if_else(str_detect(main, "handhaving"), "handhaving", "onbekend"))))))))
)

# Uitvoeren correctie
z <- z %>% mutate(
  main.uni = if_else(main%in%c("other mc", "checked by Joyce,other mc", "BlikOpStuk,other mc") & 
    cat%in%c("dairy", "dairy,packaging material"), "drink, other", main.uni))

if(druk_figuur_af=="ja") {
  # Checks
  levels(as.factor(z$main.uni))
  with(z, addmargins(table(main, main.uni, useNA="ifany"), FUN=sum))
  with(as.data.frame(z %>% filter(main.uni=="onbekend")) %>% droplevels(), 
       addmargins(table(main, main.uni, useNA="ifany"), FUN=sum))
  with(z, addmargins(table(main.uni, jaar, useNA="ifany"), FUN=sum))
  with(as.data.frame(z %>% filter(main.uni=="handhaving") )%>% droplevels(), 
       addmargins(table(main, main.uni, useNA="ifany"), FUN=sum))
  # zie bij 'cat'
  check <- z %>% filter(main%in%c("other mc", "checked by Joyce,other mc", "BlikOpStuk,other mc") & 
                          cat%in%c("dairy", "dairy,packaging material")) %>% 
    select(main, main.uni, cat, sub, object, material, brand, remark)  
}

# Verder vereenvoudiging i.v.m. reductie aantal klassen
reduc.gewenst <- "ja" # Kies "ja" of "nee"

if(reduc.gewenst %in% c("ja", "JA", "Ja", "jA")) {
  z <- z %>% mutate(
    main.uni = if_else(main.uni %in% c("drinks&other.mc", "food&drinks&other.mc", 
                                       "food&other.mc", "food&drinks"), 
                       "food, drinks & other.mc", main.uni),
    # Uitsplitsen handhaving
    main.uni = if_else(main %in% c("BlikOpStuk,Drinks,handhaving", "checked by Joyce,Drinks,handhaving",
                                       "Drinks,handhaving"), "drinks", 
               if_else(main %in% c("Food,handhaving"), "food", 
               if_else(main %in% c("Drinks,food & drinks,handhaving,other mc",
                                       "Drinks,handhaving,other mc"), "food, drinks & other.mc", 
               if_else(main %in% c("handhaving,other mc", "checked by Joyce,handhaving,other mc"), "other.mc", 
               if_else(main %in% c("handhaving"), "onbekend", main.uni)))))
  )
}

if(druk_figuur_af=="ja"){
  levels(as.factor(z$main.uni))
  with(z, addmargins(table(main, main.uni, useNA="ifany"), FUN=sum))
  with(z, addmargins(table(main.uni, jaar, useNA="ifany"), FUN=sum))
  z %>% filter(main=="checked by Joyce,handhaving,other mc") %>% select(main, sub, cat, object, material, brand, remark)  
  z %>% filter(main=="checked by Joyce,handhaving,other mc") %>% select(main, main.uni)
}


#
# 3.3 Verbeteren 'cat' ----

if(druk_figuur_af=="ja"){
  levels(z$cat)
  with(z, addmargins(table(cat, jaar, useNA="ifany"), FUN=sum))
}

# Geuniformeerde 'cat' maken (van 109 naar 26); cat.uni
z <- z %>% mutate(
  cat.uni = if_else(str_detect(cat, "alcohol"), "drinks, alcohol", 
            if_else(str_detect(cat, "energy drink"), "drinks, energy drink",
            if_else(str_detect(cat, "juice"), "drinks, juice", 
            if_else(str_detect(cat, "water"), "drinks, water", 
            if_else(str_detect(cat, "soft drink")|str_detect(cat, "soft.drink"), "drinks, soft drink", 
            if_else(str_detect(cat, "candy")&str_detect(cat, "chips", negate=TRUE), "food, candy", 
            if_else(str_detect(cat, "chips"), "food, chips", 
            if_else(str_detect(cat, "fastfood")|str_detect(cat, "energyfood"), "food, fastfood", 
            if_else(str_detect(cat, "ice.cream"), "food, ice.cream", 
            if_else(str_detect(cat, "cookie"), "food, cookie", 
            if_else(str_detect(cat, "smoking"), "smoking", 
            if_else(str_detect(cat, "medical")|str_detect(cat, "narcotics")|str_detect(cat, "chemicals")|
                      str_detect(cat, "cleaning"), "medical, nacotics & chemicals",
            if_else(str_detect(cat, "coffee"), "drinks, coffee & tea", 
            if_else(str_detect(cat, "drinks")|str_detect(cat, "dairy"), "drinks, other", 
            if_else(str_detect(cat, "parts")|str_detect(cat, "construction")|str_detect(cat, "accessories")|
                      str_detect(cat, "electronic"), "parts & construction", 
            if_else(str_detect(cat, "tickets and receipts"), "tickets and receipts", 
            if_else(str_detect(cat, "personal hygiene"), "personal hygiene", 
            if_else(str_detect(cat, "presswork"), "presswork", 
            if_else(str_detect(cat, "food other")|str_detect(cat, "food unknown")|str_detect(cat, "sauce"), "food, other",
            if_else(str_detect(cat, "fireworks")|str_detect(cat, "gambling")|str_detect(cat, "garden")|
                      str_detect(cat, "sex")|str_detect(cat, "animals")|str_detect(cat, "camping")|
                      str_detect(cat, "toys")|str_detect(cat, "party"), "leisure",
            if_else(str_detect(cat, "clothing"), "clothing", 
            if_else(str_detect(cat, "packaging")|str_detect(cat, "bags"), "packaging materials, other", 
            if_else(str_detect(cat, "unknown")|str_detect(cat, "nakijken"), "unknown", "other")))))))))))))))))))))))
)

# Verbeteren cat.uni
z <- z %>% mutate(
  cat.uni = if_else(main.uni=="food" & cat.uni %in% c("other", "unknown"), "food, other", cat.uni)
)

if(druk_figuur_af=="ja"){
  #check of alle cat in de gewenste 'cat.uni' zitten 
  levels(as.factor(z$cat.uni))
  with(z, addmargins(table(cat.uni, jaar, useNA="ifany"), FUN=sum))
  # laat zien welke cats in cat.uni zitten per onderdeel  
  with(as.data.frame(z %>% filter(cat.uni=="other")) %>% droplevels, table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, alcohol")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, energy drink")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, juice")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, water")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, soft drink")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, coffee & tea")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, other")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="food, candy")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="food, chips")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="food, fastfood")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="food, ice.cream")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="food, cookie")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="smoking")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="medical, nacotics & chemicals")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="parts & construction")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="personal hygiene")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="tickets and receipts")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="presswork")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="leisure")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="clothing")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="packaging materials, other")) %>% droplevels(), table(cat, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="unknown")) %>% droplevels(), table(cat, cat.uni))
  
  # Check of er overeenstemming is tussen main.uni en cat.uni
  with(z %>% filter(main.uni=="drinks") %>% distinct(), table(cat.uni, main.uni))
  with(z %>% filter(main.uni=="food") %>% distinct(), table(cat.uni, main.uni))
  z %>% filter(main.uni=="food" & cat.uni=="other") %>% select(main, sub, cat, object, material, brand, remark)
  z %>% filter(main.uni=="food" & cat.uni=="unknown") %>% select(main, sub, cat, object, material, brand, remark)
  with(z %>% filter(main.uni=="food, drinks & other.mc") %>% distinct(), table(cat.uni, main.uni))
  z %>% filter(main.uni=="food, drinks & other.mc" & cat.uni=="smoking") %>% select(main, sub, cat, object, material, brand, remark)  
  z %>% filter(main.uni=="food, drinks & other.mc" & cat.uni=="smoking") %>% select(cat, cat.uni, main, main.uni)  
  z %>% filter(main=="checked by Joyce,handhaving,other mc") %>% select(main, sub, cat, object, material, brand, remark)  
  with(z %>% filter(main.uni=="other.mc") %>% distinct(), table(cat.uni, main.uni))
  check.01 <- z %>% filter(main.uni=="other.mc" & cat.uni=="drinks, other") %>% select(main, sub, cat, object, material, brand, remark)
  z %>% filter(main.uni=="other.mc" & cat.uni=="drinks, other" & cat!="dairy") %>% select(main, sub, cat, object, material, brand, remark)
  check.02 <- z %>% filter(main%in%c("other mc", "checked by Joyce,other mc", "BlikOpStuk,other mc") & cat%in%c("dairy", "dairy,packaging material")) %>% select(main, main.uni, cat, cat.uni)  
  with(z %>% filter(main.uni=="onbekend") %>% distinct(), table(cat.uni, main.uni))
  
  # Check
  with(z, addmargins(table(cat.uni, raper.naam, useNA="ifany"), FUN=sum))
  tab.smok <- as.data.frame.matrix(with(droplevels(as.data.frame(z %>% filter(cat.uni=="smoking"))), 
                                        addmargins(table(brand, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.smok %>% arrange(desc(sum)), 15)
}


#
# 3.4 Verbeteren 'sub' ----
# nog verder uit te werken, veel werk, afhankelijk van vraag

if(druk_figuur_af=="ja") {
  levels(z$sub)
  with(z, addmargins(table(sub, jaar, useNA="ifany"), FUN=sum))
  
  # Geuniformeerde 'sub' maken (van 160 naar 26)
  levels(as.factor(z$cat.uni))
  
  # cat = medicijnen etal
  with(as.data.frame(z %>% filter(cat.uni=="medical, nacotics & chemicals")) %>% droplevels(), 
       addmargins(table(sub, jaar, useNA="ifany"), FUN=sum))
  
  # drinks
  with(as.data.frame(z %>% filter(str_detect(cat.uni, "drink")==TRUE)) %>% droplevels(), 
       addmargins(table(sub, jaar, useNA="ifany"), FUN=sum))
  # alcohol
  with(as.data.frame(z %>% filter(cat.uni=="drinks, alcohol")) %>% droplevels(), table(sub, cat.uni))
  with(as.data.frame(z %>% filter(main.uni=="drinks")) %>% droplevels(), table(sub, cat.uni))
  # Koffie en thee
  with(as.data.frame(z %>% filter(cat.uni=="drinks, coffee & tea")) %>% droplevels(), table(sub, cat.uni))
  
  # Foods
  with(as.data.frame(z %>% filter(str_detect(cat.uni, "food")==TRUE)) %>% droplevels(), 
       addmargins(table(sub, jaar, useNA="ifany"), FUN=sum))
  # smoking
  with(as.data.frame(z %>% filter(str_detect(cat.uni, "smoking")==TRUE)) %>% droplevels(), 
       addmargins(table(sub, jaar, useNA="ifany"), FUN=sum))
}


z <- z %>% mutate (
  sub.uni = if_else(str_detect(sub, "androgel"), "androgel", 
            if_else(sub=="medicine", "other medicine", 
            if_else(str_detect(sub, "medicine."), str_sub(sub, 10,1000), 
            if_else(str_detect(sub, ",medicine"), str_replace(sub, ",medicine", ""), 
            if_else(str_detect(sub, "bread,Geen peuk"), "bread", 
            if_else(str_detect(sub, "beer,")|str_detect(sub, "mix"), "mix_drank", 
            if_else(sub %in% c("jenever", "gin", "rum", "vodka", "whisky", "schnapps"), "sterke_drank", 
            if_else(sub%in%c("apfelkorn", "wine", "champagne"), "wijn_ea", 
            if_else(sub%in%c("cappuccino", "coffee,macchiato", "espresso", "ice coffee",
                             "macchiato"), "koffie_speciaal", 
            if_else(sub%in%c("sugar", "coffeemilk"), "koffie_toevoegingen", as.character(sub)))))))))))
)

if(druk_figuur_af=="ja") {
  levels(as.factor(z$sub.uni))
  with(z, addmargins(table(sub.uni, jaar, useNA="ifany"), FUN=sum))
  with(z %>% filter(sub.uni=="other"), table(sub, sub.uni))
  
  with(as.data.frame(z %>% filter(sub.uni=="androgel")) %>% droplevels(), table(sub, sub.uni))
  
  with(as.data.frame(z %>% filter(cat.uni=="drinks, alcohol")) %>% droplevels(), table(sub.uni, cat.uni))
  with(as.data.frame(z %>% filter(cat.uni=="drinks, coffee & tea")) %>% droplevels(), table(sub.uni, sub))
  
  with(as.data.frame(z %>% filter(main.uni=="drinks")) %>% droplevels(), table(sub.uni, sub))
  
  tab.sub <- as.data.frame.matrix(with(z, addmargins(table(sub, sub.uni, useNA="ifany"), FUN=sum)) )
  names(tab.sub)[ncol(tab.sub)-1] <- "geen.info"
  as_tibble(tab.sub, rownames = "NA") %>% print(n=Inf)
  
  # Check
  with(z, addmargins(table(sub.uni, raper.naam, useNA="ifany"), FUN=sum))
  tab.sub <- as.data.frame.matrix(with(z, addmargins(table(sub.uni, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.sub %>% arrange(desc(sum)), 15)
  
}


#
# 3.5 Verbeteren 'object' ----

if(druk_figuur_af=="ja") {
  levels(z$object)
  with(z, addmargins(table(object, jaar, useNA="ifany"), FUN=sum))
  
  tab.obj <- as.data.frame.matrix(with(z, addmargins(table(object, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.obj %>% arrange(desc(sum)), 20)
  
}

# Ordenen van 'wrappers' 
z <- z %>% mutate (
  object.uni = if_else(object%in%c("candy wrapper,wrapper", "chewinggum wrapper", "chewinggum package,wrapper",
                                   "lollipop stick,wrapper", "lollipop wrapper") | 
                         str_detect(object, "candy wrapper"), "wrapper, candy", 
               if_else(str_detect(object, "cookie"), "wrapper, cookie", 
               if_else(str_detect(object, "candy b")|str_detect(object, "candy p"), "package, candy", 
               if_else(object%in%c("wrapper", "wrapper piece", "piece,wrapper",
                                   "small piece,wrapper"), "wrapper, unknown", 
               if_else(object%in%c("cigarette butt,wrapper", "cigarette box,wrapper", "wrapper cigarette box" ,
                                   "vape package,wrapper", "shag pack,wrapper", "lighter,wrapper", 
                                   "cigarette box,wrapper cigarette box"), 
                       "wrapper, smoking", 
               if_else(object%in%c("straw,straw wrapper", "straw wrapper", "bottle,wrapper", "can,wrapper",
                                   "drink carton,wrapper", "milk carton,wrapper", "bottle wrapper",
                                   "tea bag wrapper", "cup lid,wrapper", "cup,package,wrapper", "glasses,wrapper",
                                   "bottle,package,wrapper", "straw wrapper,wrapper", "bag,straw wrapper",
                                   "bottle,bottlecap,can,cap,cigarette butt,cup,straw,wrapper",
                                   "bag,bottle,cigarette butt,cup,plastic bag,straw,wrapper", "straw,wrapper",
                                   "champagnecorkwrapper"),
                       "wrapper, drink", 
               if_else(object%in%c("ice cream stick,wrapper", "ice cream wrapper", "chopsticks wrapper", 
                                   "cake wrapper"),
                       "wrapper, food", 
               if_else(object %in% c("condom wrapper", "condom package", "condom,condom package", "condom,package", 
                                     "bag,container,wrapper", "bag plant feed,wrapper", "bag,wrapper", "bag,wrapper",
                                     "mouthcap package", "mouthcap,wrapper", "cocaine wrapper", "toy,wrapper",
                                     "pregnancy test,wrapper", "silicagelpackage,wrapper", "band aid wrapper"), 
                       "wrapper, other", 
               if_else(object %in% c("wet wipe package,wrapper", "wet wipe package", "dashboardwipes,wrapper",
                                     "sanitary napkin,wrapper"), 
                       "wrapper, wipes", as.character(object))))))))))
)

if(druk_figuur_af=="ja"){
  z%>%filter(str_detect(object.uni, "wrapper")) %>% distinct(object.uni)
  levels(as.factor(z$object.uni))
  levels(as.factor(z$object))
  
  with(z%>%filter(str_detect(object.uni, "wrapper")), addmargins(table(object.uni, jaar), FUN=sum))
  with(as.data.frame(z%>%filter(object.uni=="wrapper, other")) %>% droplevels(), 
       addmargins(table(object, jaar), FUN=sum))
  
  
  tab.obju <- as.data.frame.matrix(with(z, addmargins(table(object.uni, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.obju %>% arrange(desc(sum)), 20)
  
  z%>%filter(str_detect(object, "wrapper")) %>% distinct(object)
  
  z%>%filter(! str_detect(object, "wrapper")) %>% distinct(object)
  z%>%filter(str_detect(object, "bottlecap")) %>% distinct(object)
  
}


z <- z %>% mutate (
  object.uni = if_else(str_detect(object.uni, "bottlecap")==TRUE, "bottlecap", object.uni)
  )

if(druk_figuur_af=="ja"){
  with(as.data.frame(z%>%filter(str_detect(object.uni, "bottlecap"))) %>% droplevels(), table(object, object.uni)) 
}

#
# 3.6 Verbeteren 'materiaal' ----
if(druk_figuur_af=="ja"){
  levels(z$material)
  with(z, addmargins(table(material, jaar, useNA="ifany"), FUN=sum))
  
  tab.mat <- as.data.frame.matrix(with(z %>% filter(is.na(material)==FALSE), addmargins(table(material, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.mat %>% arrange(desc(sum)), 20)
}

z <- z %>% mutate (
  material.uni = if_else(str_detect(str_sub(material,1,7), "plastic") | str_detect(str_sub(material,1,7), "hard pl") |
                           material%in%c("nylon", "pvc", "zacht plastic", "granulaat,plastic") | 
                           str_detect(str_sub(material,1,10), "microfibre"), "plastic", 
                if_else(str_detect(str_sub(material,1,7), "alumin"), "aluminium", 
                if_else(str_detect(str_sub(material,1,7), "paper") | material%in%c("wax paper"), "paper", 
                if_else(str_detect(str_sub(material,1,7), "fabric") | str_detect(str_sub(material,1,7), "textile"), "fabric",
                if_else(str_detect(str_sub(material,1,7), "glass"), "glass", 
                if_else(str_detect(str_sub(material,1,7), "metal") | str_detect(str_sub(material,1,7), "iron"), "metal", 
                if_else(str_detect(str_sub(material,1,7), "cellulo"), "cellulose acetate", 
                if_else(str_detect(str_sub(material,1,7), "card") | str_detect(str_sub(material,1,7), "karton") |
                          material%in%c("hardboard"), "cardboard", 
                if_else(str_detect(str_sub(material,1,7), "foam") | str_detect(str_sub(material,1,7), "poly") | 
                          str_detect(str_sub(material,1,7), "piepsch") | str_detect(str_sub(material,1,7), "styrofo"), "foam", 
                if_else(str_detect(str_sub(material,1,7), "gelamin") | str_detect(str_sub(material,1,10), "papier/kar"), "laminated cardboard", 
                if_else(str_detect(str_sub(material,1,10), "latex") | str_detect(str_sub(material,1,10), "rubber") |
                          str_detect(str_sub(material,1,10), "elastic"), "latex", 
                if_else(str_detect(str_sub(material,1,7), "cork") | str_detect(str_sub(material,1,7), "cotton") |
                         str_detect(str_sub(material,1,7), "leather") | str_detect(str_sub(material,1,7), "organic") | 
                         material%in%c("wood", "wool", "vilt", "wax", "vilt,wood"), "natural", 
                if_else(material%in%c("porcelain", "beton", "stone", "gips"), "stone & clay", 
                if_else(material%in%c("stof nstax", "material unknown", "fibre"), "unknown", as.character(material)))))))))))))))
)

if(druk_figuur_af=="ja"){
  with(as.data.frame(z%>%filter(material.uni=="plastic")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="paper")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="fabric")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="glass")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="metal")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="cellulose acetate")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="cardboard")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="foam")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="laminated cardboard")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="latex")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="natural")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="unknown")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="stone & clay")) %>% droplevels(), table(material, jaar))
  with(as.data.frame(z%>%filter(material.uni=="aluminium")) %>% droplevels(), table(material, jaar))
  
  levels(as.factor(z$material.uni))
  with(z, addmargins(table(material.uni, jaar, useNA="ifany"), FUN=sum))
  tab.mat.uni <- as.data.frame.matrix(with(z %>% filter(is.na(material.uni)==FALSE), addmargins(table(material.uni, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.mat.uni %>% arrange(desc(sum)), 20)
  
  
  levels(as.factor(z$BU_NAAM))
  tab.mat.uni <- as.data.frame.matrix(with(z %>% filter(is.na(material.uni)==FALSE & 
                                                          BU_NAAM %in% c("Brandenburg", "Tuindorp", "De Bilt West", "De Bilt Oost")), 
                                           addmargins(table(material.uni,BU_NAAM, useNA="ifany"), FUN=sum)))
  head(tab.mat.uni %>% arrange(desc(sum)), 20)
}

#
# 3.7 Verbeteren 'merk' ----
# te doen, veel werk, afhankelijk van vraag

if(druk_figuur_af=="ja") {
  z %>% distinct(brand) 
  with(as.data.frame(z%>%filter(object.uni=="can"))%>%droplevels(), addmargins(table(brand, jaar, useNA="ifany"), FUN=sum))
  
  tab.mat <- as.data.frame.matrix(with(z %>% filter(is.na(brand)==FALSE), addmargins(table(brand, raper.naam, useNA="ifany"), FUN=sum)))
  head(tab.mat %>% arrange(desc(sum)), 20)
  
  str(z)
}

#
# 3.8 Samenvoegen buurten tot 'wijken' ----
levels(as.factor(z$BU_NAAM))

z <- z %>% mutate(
  wijk = if_else(BU_NAAM %in% c("De Bilt Oost","De Bilt West", "De Bilt Zuid", "Weltevreden",
                                "Beerschoten-Oostbroek"), "de bilt", 
         if_else(BU_NAAM %in% c("Brandenburg", "Tuindorp", "Bilthoven Centrum", "Noord Houdringe",
                                "Larenstein"), "bilthoven zuid",
         if_else(BU_NAAM %in% c("Bilthoven Noord I", "Bilthoven Noord II", "De Leijen", "Overbosch",
                                "Soestdijkerweg en omgeving", "Ridderoordsche Bossen"), "bilthoven noord", 
         if_else(BU_NAAM %in% c("Maartensdijk Buitengebied", "Maartensdijk Kern", "Industrieweg-Tolakkerweg"), "maartensdijk", 
         if_else(BU_NAAM %in% c("Hollandsche Rading Kern", "Hollandsche Rading Buitengebied"), "hollandsche rading",
         if_else(BU_NAAM %in% c("Groenekan Buitengebied", "Groenekan Kern"), "groenekan",
         if_else(BU_NAAM %in% c("Westbroek Buitengebied", "Westbroek Kern"), "westbroek","anders")))))))
)

# Checks
with(z, table(BU_NAAM, wijk))
with(z, table(wijk, GM_NAAM))
with(z, table(wijk, raper.naam))
with(z, table(wijk, jaar))

tab.can <- with(z %>% filter(object.uni=="can"), addmargins(table(wijk, jaar), FUN=sum))
tab.can
tab.mouthcap <- with(z %>% filter(object.uni=="mouthcap"),  addmargins(table(wijk, jaar), FUN=sum))
tab.mouthcap
with(z %>% filter(object.uni=="mouthcap"),  round(proportions(table(wijk, jaar))*100, digits=0))
with(z %>% filter(object.uni=="can"),  round(proportions(table(wijk, jaar))*100, digits=0))


# 3.8 Opslaan van nuttig bestand ----
saveRDS(z, file=paste0("c:/Users/Dico Fraters/Documents/Afval_verzamelen/data/afval_geo_aangevuld_", datumbestand, ".rda"))
write_xlsx(z, path=paste0("c:/Users/Dico Fraters/Documents/Afval_verzamelen/data/afval_geo_aangevuld_", datumbestand, ".xlsx"))



###### Vanaf hier enkele test om te checken of kaartenmaken werkt ########
if(druk_figuur_af=="ja"){
  # plot postcode bedrijven (incl kleur per plat)
  ggplot()+
    #geom_stars(data=ahn5_s, downsample = 20)+
    #scale_fill_gradientn(colours = terrain.colors(6))+
    geom_sf(data=DeBilt_eo %>% filter(jaar==2023), fill = NA, lwd = 1, color = "black")+
    geom_sf(data= loc_in_buurt, aes(color = GM_NAAM))+
    ggtitle("Afval geraapt per gemeente")
  
  
  # Inzoomend op gemeente De Bilt:
  bbox <- DeBilt_eo %>% filter(GM_NAAM == "De Bilt") %>% st_bbox()
  bdat <- loc_in_buurt%>% filter(GM_NAAM == "De Bilt") 
  
  # https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
  png("c:/Users/Dico Fraters/Documents/Afval_verzamelen/plaatjes/Afval_DeBilt_2023.png", 
      width = 5000, height = 4000, units = "px")
  par(mar=c(2,4,4,1), mai=c(0,0,0,0))
  ggplot()+
    theme_bw() + 
    geom_sf(data=DeBilt_eo%>%filter(GM_NAAM=="De Bilt"), 
            fill = NA, lwd = 3, color = "black") +
    geom_sf(data=bdat["BU_NAAM"], aes(color=BU_NAAM), size=10)+
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))+
    #geom_sf_label(data=DeBilt_eo, aes(label = GM_NAAM), alpha = 0.7)+
    ggtitle("Afval in De Bilt in 2023") +
    labs(color='Buurten in De Bilt') +
    theme(
      title=element_text(size=80, face="bold"),
      axis.text=element_text(size=60),
      legend.title= element_text(size=70),
      legend.text=element_text(size=60)) 
  dev.off()
  
  BiltBilthoven <- c("Bilthoven Centrum", "Bilthoven Noord I", "Bilthoven Noord II", 
                     "Brandenburg", "De Bilt Oost", "De Bilt West", "De Bilt Zuid",
                     "De Leijen", "Tuindorp")
  
  # Inzoomend op dorpen De Bilt - Bilthoven:
  bbox.1 <- DeBilt_eo %>% filter(BU_NAAM %in% BiltBilthoven) %>% st_bbox()
  bdat.1 <- loc_in_buurt%>% filter(BU_NAAM %in% BiltBilthoven) 
  
  png("c:/Users/Dico Fraters/Documents/Afval_verzamelen/plaatjes/Afval_DeBiltBilhoven_2019-2023april.png", 
      width = 5000, height = 4000, units = "px")
  par(mar=c(2,4,4,1), mai=c(0,0,0,0))
  ggplot()+
    theme_bw() + 
    geom_sf(data=DeBilt_eo%>%filter(BU_NAAM %in% BiltBilthoven), 
            fill = NA, lwd = 3, color = "black") +
    geom_sf(data=bdat.1["BU_NAAM"], aes(color=BU_NAAM), size=10)+
    coord_sf(xlim = c(bbox.1[1], bbox.1[3]), ylim = c(bbox.1[2], bbox.1[4]))+
    #geom_sf_label(data=DeBilt_eo, aes(label = GM_NAAM), alpha = 0.7)+
    ggtitle("Afval in De Bilt - Bilthoven 2019-2023 (deels)") +
    labs(color='Buurten in De Bilt') +
    theme(
      title=element_text(size=80, face="bold"),
      axis.text=element_text(size=60),
      legend.title= element_text(size=70),
      legend.text=element_text(size=60)) 
  dev.off()
  
  # Inzoomend op 2023 in dorpen De Bilt - Bilthoven:
  bbox.2 <- DeBilt_eo %>% filter(BU_NAAM %in% BiltBilthoven ) %>% st_bbox()
  bdat.2 <- loc_in_buurt%>% filter(BU_NAAM %in% BiltBilthoven & jaar==2023) 
  
  png("c:/Users/Dico Fraters/Documents/Afval_verzamelen/plaatjes/Afval_DeBiltBilhoven_2023april.png", 
      width = 5000, height = 4000, units = "px")
  par(mar=c(2,4,4,1), mai=c(0,0,0,0))
  ggplot()+
    theme_bw() + 
    geom_sf(data=DeBilt_eo%>%filter(BU_NAAM %in% BiltBilthoven), 
            fill = NA, lwd = 3, color = "black") +
    geom_sf(data=bdat.2["BU_NAAM"], aes(color=BU_NAAM), size=10)+
    coord_sf(xlim = c(bbox.2[1], bbox.2[3]), ylim = c(bbox.2[2], bbox.2[4]))+
    #geom_sf_label(data=DeBilt_eo, aes(label = GM_NAAM), alpha = 0.7)+
    ggtitle("Afval in De Bilt - Bilthoven 2023 (deels)") +
    labs(color='Rapers in De Bilt') +
    theme(
      title=element_text(size=80, face="bold"),
      axis.text=element_text(size=60),
      legend.title= element_text(size=70),
      legend.text=element_text(size=60)) 
  dev.off()
  
  # Inzoomend op 2023 in dorpen De Bilt - Bilthoven:
  bbox.3 <- DeBilt_eo %>% filter(BU_NAAM %in% BiltBilthoven ) %>% st_bbox()
  bdat.3 <- v%>% filter(BU_NAAM %in% BiltBilthoven & jaar==2023) 
  
  png("c:/Users/Dico Fraters/Documents/Afval_verzamelen/plaatjes/Afval_DeBiltBilhoven_raper_2023.png", 
      width = 5000, height = 4000, units = "px")
  par(mar=c(2,4,4,1), mai=c(0,0,0,0))
  ggplot()+
    theme_bw() + 
    geom_sf(data=DeBilt_eo%>%filter(BU_NAAM %in% BiltBilthoven), 
            fill = NA, lwd = 3, color = "black") +
    geom_sf(data=bdat.3["raper.naam"], aes(color=raper.naam), size=10) +
    coord_sf(xlim = c(bbox.3[1], bbox.3[3]), ylim = c(bbox.3[2], bbox.3[4]))+
    #geom_sf_label(data=DeBilt_eo, aes(label = GM_NAAM), alpha = 0.7)+
    ggtitle("Afval in De Bilt - Bilthoven 2023 (deels)") +
    labs(color='Rapers in De Bilt') +
    theme(
      title=element_text(size=80, face="bold"),
      axis.text=element_text(size=60),
      legend.title= element_text(size=70),
      legend.text=element_text(size=60)) 
  dev.off()
}

