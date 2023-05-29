podaci <- read.csv("C:/Users/MATE/Desktop/Hackathon/Oglasi.csv")

# triba instalirat rtools za koristit ggplot !!

library(rgdal)
library(ggplot2)
library(sf)

################################################################################
################################################################################

#---------------------------------------------------------------KARTA I PODACI (CRO)------------------
žup_cro <- data.frame(
  županija = c(
    "Virovitičko-podravska",
    "Koprivničko-križevačka",
    "Osječko-baranjska",
    "Istarska",
    "Dubrovačko-neretvanska",
    "Sisačko-moslovačka",
    "Brodsko-posavska",
    "Karlovačka",
    "Zadarska",
    "Vukovarsko-srijemska",
    "Splitsko-dalmatinska",
    "Varaždinska",
    "Krapinsko-zagorska",
    "Zagrebačka",
    "Primorsko-goranska",
    "Šibensko-kninska",
    "Ličko-senjska",
    "Bjelovarsko-bilogorska",
    "Grad Zagreb",
    "Međimurska",
    "Požeško-slavonska"   
  )
)


# prosjecni kvadrat po županiji
avg_cro <- aggregate(kvadrat ~ županija, data = podaci, FUN = mean)

# stand. devijacija
sd_cro <- aggregate(kvadrat ~ županija, data = podaci, FUN = sd)

# koeficijent varijacije po županiji
cv_cro <-
  data.frame(županija = avg_cro$županija,
             cv = sd_cro$kvadrat / avg_cro$kvadrat)

# broj oglasa po županiji (da vidite broj podataka i odlucimo koliko je "znacajno" za zeleno na karti)
cnt_cro <-
  aggregate(lokacija ~ županija, data = podaci, FUN = length)

# merge u fajl koji ima podatke o svakoj županiji
cro_data <-
  merge(merge(avg_cro, cv_cro, by = "županija"), cnt_cro, by = "županija")
cro_data <-
  merge(žup_cro, cro_data, by = "županija", all.x = TRUE)
names(cro_data) <-
  c("županija", "avgKvadrata", "koefVarKvadrata", "brojOglasa")  #stupce preimenovat

# karta
cro_map <-
  readOGR("C:/Users/MATE/Desktop/Hackathon/Croatia", "Croatia_map")

# spajanje tih podataka s kartom u zajednicki fajl
cro_data_map <-
  merge(
    cro_map,
    cro_data,
    by.x = "County",
    by.y = "županija",
    all.x = TRUE
  )
cro_data_map <- st_as_sf(cro_data_map)       #bolji format

#brisanje
rm(žup_cro,
   avg_cro,
   sd_cro,
   cv_cro,
   cnt_cro,
   cro_map)

#---------------------------------------------------------------PLOTIRANJE (CRO)----------------------
# avg cijena po kv CRO
ggplot(cro_data_map) +
  geom_sf(aes(fill = round(avgKvadrata, 2)),
          color = "gray",
          lwd = 1) +
  scale_fill_gradient(
    low = "#efedf5",
    high = "#3f007d",
    #raspon boja županija, opcionalno za boju onih bez podataka ide: na.value = "boja"
    breaks = c(1000, 1650, 2250, 2850, 3500),
    limits = c(1000, 3500)
  ) +
  theme_void() +       #tema da nema rešetki priko karte
  labs(title = "Prosječna cijena po kvadratu u Hrvatskoj",
       fill = "Cijena po metru kvadratnom (€)",
       caption = "Zelene županije - značajna količina podataka\nCrne županije - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(avgKvadrata) |
        is.null(avgKvadrata),
      County,
      paste0(County, "\n", "€", sprintf("%.2f", round(avgKvadrata, 2)))
    )),
    #ako nema podataka, onda samo ime županije
    size = 2.5,
    #za tekst županija je sve dalje
    fontface = "bold",
    color = ifelse(
      cro_data_map$brojOglasa < 40 |
        is.na(cro_data_map$brojOglasa) |
        is.null(cro_data_map$brojOglasa),
      "black",
      "darkgreen"
    ),
    #zeleni imaju znacajno podataka, crni ne
    fill = "#F2F3F4",
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0.001
  )

# koef var CRO
ggplot(cro_data_map) +
  geom_sf(aes(fill = round(koefVarKvadrata * 100, 2)),
          color = "gray",
          lwd = 1) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08306b",
    breaks = c(0, 10, 20, 30, 40, 50),
    limits = c(0, 50)
  ) +
  theme_void() +
  labs(title = "Koeficijent varijacije cijene kvadrata u Hrvatskoj",
       fill = "Koeficijent varijacije (%)",
       caption = "Zelene županije - značajna količina podataka\nCrne županije - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(koefVarKvadrata) |
        is.null(koefVarKvadrata) ,
      County,
      paste0(County, "\n", sprintf("%.2f", round(koefVarKvadrata, 2) * 100), "%")
    )),
    size = 2.5,
    fontface = "bold",
    color = ifelse(
      cro_data_map$brojOglasa < 40 |
        is.na(cro_data_map$brojOglasa) |
        is.null(cro_data_map$brojOglasa),
      "black",
      "darkgreen"
    ),
    fill = "#F2F3F4",
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0.001
  )

rm(cro_data_map)

#---------------------------------------------------------------KARTA I PODACI (ZG)-------------------
kvart_zg <-
  data.frame(
    lokacija = c(
      "Brezovica",
      "Črnomerec",
      "Donja Dubrava",
      "Donji Grad",
      "Gornja Dubrava",
      "Gornji Grad - Medveščak",
      "Maksimir",
      "Novi Zagreb - Istok",
      "Novi Zagreb - Zapad",
      "Pešćenica - Žitnjak",
      "Podsljeme",
      "Podsused - Vrapče",
      "Sesvete",
      "Stenjevec",
      "Trešnjevka - Jug",
      "Trešnjevka - Sjever",
      "Trnje",
      "Zagreb - Okolica"
    )
  )

# izdvojimo kvartove
zg_data <-
  subset(podaci,
         županija == "Grad Zagreb",
         select = c("lokacija", "kvadrat"))

# prosjecni kvadrat po kvartovima zga
avg_zg <- aggregate(kvadrat ~ lokacija, data = zg_data, FUN = mean)

# stand. devijacija
sd_zg <- aggregate(kvadrat ~ lokacija, data = zg_data, FUN = sd)

# koeficijent varijacije po kvartovima zga
cv_zg <-
  data.frame(lokacija = avg_zg$lokacija,
             cv = sd_zg$kvadrat / avg_zg$kvadrat)

# broj oglasa po kvartovima zga (da vidite broj podataka i odlucimo koliko je "znacajno" za zeleno na karti)
cnt_zg <-
  aggregate(kvadrat ~ lokacija, data = zg_data, FUN = length)

# merge u fajl koji ima podatke o svakoj županiji
zg_data <-
  merge(merge(avg_zg, cv_zg, by = "lokacija"), cnt_zg, by = "lokacija")
zg_data <-
  merge(kvart_zg, zg_data, by = "lokacija", all.x = TRUE)
names(zg_data) <-
  c("lokacija", "avgKvadrata", "koefVarKvadrata", "brojOglasa")  #stupce preimenovat

# karta
zg_map <-
  readOGR("C:/Users/MATE/Desktop/Hackathon/Zagreb", "Zagreb_map")

# spajanje tih podataka s kartom u zajednicki fajl
zg_data_map <-
  merge(zg_map,
        zg_data,
        by.x = "District",
        by.y = "lokacija",
        all.x = TRUE)
zg_data_map <- st_as_sf(zg_data_map)       #pogodniji format


#brisanje
rm(zg_data,
   kvart_zg,
   avg_zg,
   sd_zg,
   cv_zg,
   cnt_zg,
   zg_map)

#---------------------------------------------------------------PLOTIRANJE (ZG)-----------------------
# avg cijena po kv ZG
ggplot(zg_data_map) +
  geom_sf(aes(fill = round(avgKvadrata, 2)),
          color = "gray",
          lwd = 1) +
  scale_fill_gradient(
    low = "#fcfbfd",
    high = "#3f007d",
    #raspon boja kvartova
    breaks = c(2000, 2500, 3000, 3500),
    limits = c(2000, 3500)
  ) +
  theme_void() +       #tema da nema rešetki priko karte
  labs(title = "Prosječna cijena po kvadratu u Zagrebu",
       fill = "Cijena po metru kvadratnom (€)",
       caption = "Zeleni kvartovi - značajna količina podataka\nCrni kvartovi - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(avgKvadrata) |
        is.null(avgKvadrata),
      District,
      paste0(District, "\n", "€", sprintf("%.2f", round(avgKvadrata, 2)))
    )),
    #ako nema podataka, onda samo ime županije
    size = 2.5,
    #za tekst kvartova je sve dalje
    fontface = "bold",
    color = ifelse(
      zg_data_map$brojOglasa < 80 |
        is.na(zg_data_map$brojOglasa) |
        is.null(zg_data_map$brojOglasa),
      "black",
      "darkgreen"
    ),
    #zeleni imaju znacajno podataka, crni ne
    fill = "#F2F3F4",
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0.001
  )

# koef var ZG
ggplot(zg_data_map) +
  geom_sf(aes(fill = round(koefVarKvadrata * 100, 2)),
          color = "gray",
          lwd = 1) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08306b",
    breaks = c(0, 10, 20, 30, 40),
    limits = c(0, 40)
  ) +
  theme_void() +
  labs(title = "Koeficijent varijacije cijene kvadrata u Zagrebu",
       fill = "Koeficijent varijacije (%)",
       caption = "Zeleni kvartovi - značajna količina podataka\nCrni kvartovi - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(koefVarKvadrata) |
        is.null(koefVarKvadrata),
      District,
      paste0(District, "\n", sprintf("%.2f", round(koefVarKvadrata, 2) * 100), "%")
    )),
    size = 2.5,
    fontface = "bold",
    color = ifelse(
      zg_data_map$brojOglasa < 80 |
        is.na(zg_data_map$brojOglasa) |
        is.null(zg_data_map$brojOglasa),
      "black",
      "darkgreen"
    ),
    fill = "#F2F3F4",
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0.001
  )

rm(zg_data_map)