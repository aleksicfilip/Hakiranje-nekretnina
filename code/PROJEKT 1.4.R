library(rvest)
library(dplyr)
library(rgdal)
library(ggplot2)
library(sf)
library(stringr)
library(lubridate)
library(readr)
library(tm)
library(cowplot)
library(stargazer)


# briše sve objekte iz environment-a
rm(list = ls())

# u ovu liniju upišite adresu direktorija gdje su spremljeni shapefileovi za karte
folder = "C:/Users/xxx/Desktop/Karte" # ovo je primjer patha u windowsu (pazi da bude /, a ne \)

#---------------------------------------------------------------SCRAPING DATA----------------------------------

# skidam podatke dok ima oglasa
i =0
while(TRUE){
  i=i+1 #broj stranice
  #kreiram link kao kombinaciju pocetka i broja stranice i učitavam ga u R
  link1 = "https://besplatnioglasi.eu/nekretnine/stanovi?lang=hr&page="
  broj = as.character(i);
  LINK = paste(link1,broj,sep="",collapse=NULL)
  page = read_html(LINK)
  
  #---------------------------------------------------------------VADIM INFO SA STRANICE---------------------
  # vektori s naslovom i cjenom
  naslov = page %>% html_nodes(".titleLink") %>% html_text()
  if(length(naslov)==0) break;    # terministički uvjet
  cijena =  page %>% html_nodes(".actualValue") %>% html_text()
  
  #vađenje 4 puta većeg vektora koji sadrži: kvadraturu,objekt,sobe,lokaciju
  informacije = page %>% html_nodes(".dataHalfValue") %>% html_text()
  
  #sad moram srediti informacije jer točno svaki 4. daje istu informaciju
  kvadratura = subset(informacije,c(1:length(informacije))%%4 == 1)
  objekt = subset(informacije,c(1:length(informacije))%%4 == 2)
  sobe = subset(informacije,c(1:length(informacije))%%4 == 3)
  lokacija = subset(informacije,c(1:length(informacije))%%4 == 0)
  
  #---------------------------------------------------------------ULAZIM U SVAKI OGLAS I VADIM OPIS I DATUM---------------------  
  #1. preuzimam dio gdje su linkovi i spremam kao stringovi, 2. mičem prvih 10 slova,
  #3. uzimam sve do razmaka, 4. skidam zadnje slovo
  linkovi = page %>% html_nodes(".titleLink") %>% as.character()
  linkovi=substring(linkovi, first=10, last = 1000000L)
  linkovi=sub("\\ .*", "", linkovi)
  linkovi=gsub('.{1}$', '', linkovi)
  
  #definiram polje gdje ću spremati datume i opis
  datumi=c()
  opis=c()
  
  #for petlja koja za svaki link uđe u stranicu oglasa
  #nađe klasu koju tražim i da string koji sredim u datum (ili opis)
  for(j in 1:length(linkovi))
  {
    pagezadatum=read_html(linkovi[j]) #učita string
    datum = pagezadatum %>% html_nodes(".detailedTitleHolder") %>% html_text() #nađe element i da text
    datum=sub("\\Šifra oglasa:.*", "", datum) #da sve do šifre oglasa
    datum=gsub('.{3}$', '', datum) # izbriši zadnja 3 elementa
    datum = substr(datum, nchar(datum)-9, nchar(datum)) #da zadnjih 9 elemenata
    datumi = append(datumi,datum) #doda moj datum u listu datuma
    
    
    opis1 = pagezadatum %>% html_nodes(".detailDataDescription") %>% html_text() #skupljm opis
    opis = append(opis,opis1) #dodajem opis vektoru
  }
  
  #---------------------------------------------------------------SREĐIVANJE VARIJABLI---------------------  
  #prebacivanje datuma iz stringa u date(mjesec i godina)
  datumi = strptime(datumi, format = "%d.%m.%Y")
  datumi = as.Date(datumi) #datumi <- as.yearmon(datumi)
  
  #uređivanje cijene (micanje eura..)
  cijena = sub(".*~ ", "", cijena) 
  cijena = sub(" .*", "", cijena)
  cijena = gsub("[[:punct:]]", "", cijena) #micanje točke iz cijene(272.345 -> 272345) zbog racunanja
  cijena = as.numeric(cijena) #prebacivanje stringa u broj
  
  
  #micanje m2 i prebacivanje stringa u broj
  kvadratura = gsub('.{3}$', '', kvadratura)
  kvadratura = as.numeric(kvadratura)
  
  #definiranje cijene po kvadratu
  kvadrat = cijena/kvadratura
  kvadrat = round(kvadrat, 2)
  
  #---------------------------------------------------------------KRAJ GLAVNOG FOR-A I SPJANJE PODATAKA---------------------  
  
  #ako vadim s prve stranice onda kreiram data.frame, a ako sa stranica iza onda ono što izvadim spajam s prijašnjim data.frame
  if(i==1){podaci = data.frame(naslov,cijena,kvadratura ,objekt,sobe,lokacija,opis, kvadrat,datumi,stringsAsFactors = FALSE)}
  else {podaci2 = data.frame(naslov,cijena,kvadratura ,objekt,sobe,lokacija,opis, kvadrat,datumi, stringsAsFactors = FALSE); podaci=rbind(podaci,podaci2);}
}

#---------------------------------------------POPRAVAK NAZIVA I GRUPIRANJE PO ŽUPANIJAMA---------------------------------------

# popravak oštećenih lokacija
old_values <- c("Mali LoÅ¡inj", "KrÅ¡an", "VaraÅ¾din", "SukoÅ¡an", "TreÅ¡njevka - Sjever", 
                "PeÅ¡Ä\u0087enica - Å½itnjak", "Gornji Grad - MedveÅ¡Ä\u008dak", "ViÅ¡kovo", 
                "Ä\u008crnomerec", "ReÅ¡etari", "FaÅ¾ana", "PoreÄ\u008d", "Å½upa DubrovaÄ\u008dka",
                "ZapreÅ¡iÄ\u0087", "TreÅ¡njevka - Jug", "BaÅ¡ka", "OmiÅ¡alj", "LiÅ¾njan", 
                "Malinska-DubaÅ¡nica", "Podsused - VrapÄ\u008de", "Veliko TrgoviÅ¡Ä\u0087e", "Ä\u008cavle", 
                "Å\u00A0ibenik", "ViÅ¡njan", "PaÅ¡man", "Ä\u008cazma", "KaÅ¡tela", "Vinodolska OpÄ\u0087ina", 
                "FuÅ¾ine", "TuÄ\u008depi", "OrebiÄ\u0087", "MarÄ\u008dana", "KaÅ¡telir-Labinci")

new_values <- c("Mali Lošinj", "Kršan", "Varaždin", "Sukošan", "Trešnjevka - Sjever", 
                "Pešćenica - Žitnjak", "Gornji Grad - Medveščak", "Viškovo", 
                "Črnomerec", "Rešetari", "Fažana", "Poreč", "Župa Dubrovačka", 
                "Zaprešić", "Trešnjevka - Jug", "Baška", "Omišalj", "Ližnjan", 
                "Malinska-Dubašnica", "Podsused - Vrapče", "Veliko Trgovišće", "Čavle", 
                "Šibenik", "Višnjan", "Pašman", "Čazma", "Kaštela", "Vinodolska Općina", 
                "Fužine", "Tučepi", "Orebić", "Marčana", "Kaštelir-Labinci")

replacer <- setNames(new_values, old_values)
podaci$lokacija <- str_replace_all(podaci$lokacija, replacer)



# svakom gradu pridružiti njegovu županiju
lookup_table <- data.frame(grad = c(
      "Čačinci", "Čađavica", "Crnac", "Gradina", "Lukač", "Mikleuš", "Nova Bukovica", "Orahovica", "Pitomača", "Slatina", "Sopje", "Špišić Bukovica", "Suhopolje", "Virovitica", "Virovitica - Okolica", "Voćin", "Zdenci", 
      "Drnje", "Ferdinandovac", "Gola", "Gornja Rijeka", "Hlebine", "Kalinovac", "Kalnik", "Kloštar Podravski", "Koprivnica", "Koprivnica - Okolica", "Koprivnički Bregi", "Koprivnički Ivanec", "Križevci", "Legrad", "Molve", "Novigrad Podravski", "Novo Virje", "Peteranec", "Podravske Sesvete", "Rasinja", "Sokolovac", "Sveti Ivan Žabno", "Sveti Petar Orehovec", "Virje", "Đelekovac", "Đurđevac", 
      "Antunovac", "Beli Manastir", "Belišće", "Bilje", "Bizovac", "Čeminac", "Čepin", "Darda", "Donja Motičina", "Donji Miholjac", "Draž", "Drenje", "Erdut", "Ernestinovo", "Feričanci", "Gorjani", "Jagodnjak", "Kneževi Vinogradi", "Koška", "Levanjska Varoš", "Magadenovac", "Marijanci", "Našice", "Osijek", "Osijek - Okolica", "Petlovac", "Petrijevci", "Podgorač", "Podravska Moslavina", "Popovac", "Punitovci", "Satnica Đakovačka", "Semeljci", "Šodolovci", "Strizivojna", "Trnava", "Valpovo", "Viljevo", "Viškovci", "Vladislavci", "Vuka", "Đakovo", "Đurđenovac",
      "Bale", "Barban", "Brtonigla", "Buje", "Buzet", "Cerovlje", "Fažana", "Gračišće", "Grožnjan", "Kanfanar", "Karojba", "Kaštelir-Labinci", "Kršan", "Labin", "Lanišće", "Ližnjan", "Lupoglav", "Marčana", "Medulin", "Motovun", "Novigrad", "Oprtalj", "Pazin", "Pazin - Okolica", "Pićan", "Poreč", "Pula", "Raša", "Rovinj", "Sveta Nedelja", "Sveti Lovreč", "Sveti Petar U Šumi", "Svetvinčenat", "Tar-Vabriga", "Tinjan", "Umag", "Višnjan", "Vižinada", "Vodnjan", "Vrsar", "Žminj", 
      "Blato", "Dubrovačko Primorje", "Dubrovnik", "Dubrovnik - Okolica", "Janjina", "Konavle", "Korčula", "Kula Norinska", "Lastovo", "Lumbarda", "Metković", "Mljet", "Opuzen", "Orebić", "Ploče", "Pojezerje", "Slivno", "Smokvica", "Ston", "Trpanj", "Vela Luka", "Zažablje", "Župa Dubrovačka", 
      "Donji Kukuruzari", "Dvor", "Glina", "Gvozd", "Hrvatska Dubica", "Hrvatska Kostajnica", "Jasenovac", "Kutina", "Lekenik", "Lipovljani", "Majur", "Martinska Ves", "Novska", "Petrinja", "Popovača", "Sisak", "Sisak - Okolica", "Sunja", "Topusko", "Velika Ludina", 
      "Bebrina", "Brodski Stupnik", "Bukovlje", "Cernik", "Davor", "Donji Andrijevci", "Dragalić", "Garčin", "Gornja Vrba", "Gornji Bogićevci", "Gundinci", "Klakar", "Nova Gradiška", "Nova Kapela", "Okučani", "Oprisavci", "Oriovac", "Podcrkavlje", "Rešetari", "Sibinj", "Sikirevci", "Slavonski Brod", "Slavonski Brod - Okolica", "Slavonski Šamac", "Stara Gradiš¡ka", "Staro Petrovo Selo", "Velika Kopanica", "Vrbje", "Vrpolje", 
      "Barilović", "Bosiljevo", "Cetingrad", "Draganić", "Duga Resa", "Generalski Stol", "Josipdol", "Karlovac", "Karlovac - Okolica", "Krnjak", "Lasinja", "Netretić", "Ogulin", "Ozalj", "Plaški", "Rakovica", "Ribnik", "Saborsko", "Slunj", "Tounj", "Vojnić", "Žakanje", 
      "Benkovac", "Bibinje", "Biograd Na Moru", "Galovac", "Gračac", "Jasenice", "Kali", "Kukljica", "Lišane Ostrovičke", "Nin", "Obrovac", "Pag", "Pakoštane", "Pašman", "Polača", "Poličnik", "Posedarje", "Povljana", "Ugljan", "Ražanac", "Sali", "Škabrnja", "Stankovci", "Starigrad", "Sukošan", "Sveti Filip i Jakov", "Tkon", "Vir", "Zadar", "Zadar - Okolica", "Zemunik Donji", "Vrsi", "Privlaka (ZD)", "Novigrad (ZD)", 
      "Andrijaševci", "Babina Greda", "Bogdanovci", "Borovo", "Bošnjaci", "Cerna", "Drenovci", "Gradište", "Gunja", "Ilok", "Ivankovo", "Jarmina", "Lovas", "Markušica", "Negoslavci", "Nijemci", "Nuštar", "Privlaka", "Stari Jankovci", "Stari Mikanovci", "Tompojevci", "Tordinci", "Tovarnik", "Trpinja", "Vinkovci", "Vinkovci - Okolica", "Voćinci", "Vrbanja", "Vukovar", "Vukovar - Okolica", "Županja", "Otok (VU)", 
      "Baška Voda", "Bol", "Brela", "Cista Provo", "Dicmo", "Dugi Rat", "Dugopolje", "Gradac", "Hrvace", "Hvar", "Imotski", "Jelsa", "Kaštela", "Klis", "Komiža", "Lećevica", "Lokvičići", "Lovreč", "Makarska", "Marina", "Milna", "Muć", "Nerežišća", "Okrug", "Omiš", "Otok", "Podbablje", "Podgora", "Podstrana", "Postira", "Prgomet", "Primorski Dolac", "Proložac", "Pučišća", "Runovići", "Seget", "Selca", "Šestanovac", "Sinj", "Solin", "Solin - Okolica", "Šolta", "Split", "Split - Okolica", "Stari Grad", "Sućuraj", "Supetar", "Sutivan", "Trilj", "Trogir", "Trogir - Okolica", "Tučepi", "Vis", "Vrgorac", "Vrlika", "Zadvarje", "Zagvozd", "Zmijavci", 
      "Bednja", "Beretinec", "Breznica", "Breznički Hum", "Cestica", "Donja Voća", "Donji Martijanec", "Gornji Kneginec", "Ivanec", "Jalžabet", "Klenovnik", "Lepoglava", "Ljubešćica", "Ludbreg", "Mali Bukovec", "Maruševec", "Novi Marof", "Petrijanec", "Sračinec", "Sveti Ilija", "Sveti Đurđ", "Trnovec Bartolovečki", "Varaždin", "Varaždin - Okolica", "Varaždinske Toplice", "Veliki Bukovec", "Vidovec", "Vinica", "Visoko", 
      "Bedekovčina", "Budinščina", "Desinić", "Donja Stubica", "Gornja Stubica", "Hrašćina", "Hum Na Sutli", "Jesenje", "Klanjec", "Konjščina", "Kraljevec Na Sutli", "Krapina", "Krapina - Okolica", "Krapinske Toplice", "Kumrovec", "Lobor", "Mače", "Marija Bistrica", "Mihovljan", "Novi Golubovec", "Oroslavje", "Petrovsko", "Pregrada", "Radoboj", "Stubičke Toplice", "Sveti Križ Začretje", "Tuhelj", "Veliko Trgovišće", "Zabok", "Zagorska Sela", "Zlatar", "Zlatar-Bistrica", "Đurmanec", 
      "Bedenica", "Bistra", "Brckovljani", "Brdovec", "Dubrava", "Dubravica", "Dugo Selo", "Dugo Selo - Okolica", "Farkaševac", "Gradec", "Ivanić-Grad", "Ivanić-Grad - Okolica", "Jakovlje", "Jastrebarsko", "Jastrebarsko - Okolica", "Klinča Sela", "Kloštar Ivanić", "Krašić", "Kravarsko", "Križ", "Luka", "Marija Gorica", "Orle", "Pisarovina", "Pokupsko", "Preseka", "Pušća", "Rakovec", "Rugvica", "Samobor", "Samobor - Okolica", "Stupnik", "Sveti Ivan Zelina", "Velika Gorica", "Velika Gorica - Okolica", "Vrbovec", "Zaprešić", "Zaprešić - Okolica", "Žumberak", "Sveta Nedjelja", 
      "Bakar", "Baška", "Brod Moravice", "Čabar", "Čavle", "Cres", "Crikvenica", "Delnice", "Dobrinj", "Fužine", "Jelenje", "Kastav", "Klana", "Kostrena", "Kraljevica", "Krk", "Lokve", "Lovran", "Mali Lošinj", "Malinska-Dubašnica", "Matulji", "Mošćenička Draga", "Mrkopalj", "Novi Vinodolski", "Omišalj", "Opatija", "Opatija - Okolica", "Punat", "Rab", "Ravna Gora", "Rijeka", "Skrad", "Vinodolska Općina", "Viškovo", "Vrbnik", "Vrbovsko", 
      "Biskupija", "Civljane", "Drniš", "Ervenik", "Kijevo", "Kistanje", "Knin", "Murter", "Pirovac", "Primošten", "Promina", "Rogoznica", "Ružić", "Šibenik", "Šibenik - Okolica", "Skradin", "Tisno", "Unešić", "Vodice", 
      "Brinje", "Donji Lapac", "Gospić", "Gospić - Okolica", "Karlobag", "Lovinac", "Novalja", "Otočac", "Perušić", "Plitvička Jezera", "Senj", "Udbina", "Vrhovine", 
      "Berek", "Bjelovar", "Bjelovar - Okolica", "Čazma", "Daruvar", "Dežanovac", "Gareš¡nica", "Grubišno Polje", "Hercegovac", "Ivanska", "Kapela", "Končanica", "Nova Rača", "Rovišće", "Šandrovac", "Severin", "Sirač", "Štefanje", "Velika Pisanica", "Velika Trnovitica", "Veliki Grđevac", "Veliko Trojstvo", "Zrinski Topolovac", "Đulovac", 
      "Brezovica", "Črnomerec", "Donja Dubrava", "Donji Grad", "Gornja Dubrava", "Gornji Grad - Medveščak", "Maksimir", "Novi Zagreb - Istok", "Novi Zagreb - Zapad", "Pešćenica - Žitnjak", "Podsljeme", "Podsused - Vrapče", "Sesvete", "Stenjevec", "Trešnjevka - Jug", "Trešnjevka - Sjever", "Trnje", "Zagreb - Okolica", 
      "Belica", "Čakovec", "Čakovec - Okolica", "Dekanovec", "Domašinec", "Donji Kraljevec", "Donji Vidovec", "Goričan", "Gornji Mihaljevec", "Kotoriba", "Mala Subotica", "Mursko Središće", "Nedelišće", "Orehovica", "Podturen", "Prelog", "Pribislavec", "Selnica", "Šenkovec", "Strahoninec", "Štrigova", "Sveta Marija", "Sveti Juraj Na Bregu", "Sveti Martin Na Muri", "Vratišinec", 
      "Brestovac", "Čaglin", "Jakšić", "Kaptol", "Kutjevo", "Lipik", "Pakrac", "Pleternica", "Požega", "Požega - Okolica", "Velika"
    ),
    županija = c(
      rep("Virovitičko-podravska", 17),
      rep("Koprivničko-križevačka", 26),
      rep("Osječko-baranjska", 43),
      rep("Istarska", 41),
      rep("Dubrovačko-neretvanska", 23),
      rep("Sisačko-moslovačka", 20),
      rep("Brodsko-posavska", 29),
      rep("Karlovačka", 22),
      rep("Zadarska", 34),
      rep("Vukovarsko-srijemska", 32),
      rep("Splitsko-dalmatinska", 58),
      rep("Varaždinska", 29),
      rep("Krapinsko-zagorska", 33),
      rep("Zagrebačka", 40),
      rep("Primorsko-goranska", 36),
      rep("Šibensko-kninska", 19),
      rep("Ličko-senjska", 13),
      rep("Bjelovarsko-bilogorska", 24),
      rep("Grad Zagreb", 18),
      rep("Međimurska", 25),
      rep("Požeško-slavonska", 11)
    ))


# dodamo stupac sa županijama
podaci <- merge(podaci, lookup_table, by.x = "lokacija", by.y = "grad")

#---------------------------------------------------------------ČIŠĆENJE + BRISANJE VARIJABLI---------------------

#učitavanje i sređivanje podataka  
podaci = subset(podaci,podaci$kvadratura>15) #mičem nekretnine manje od 15 kvadrata
podaci = subset(podaci,podaci$kvadrat<13000) #mičem skuplje od 13k€ 
podaci = subset(podaci,podaci$kvadratura<1000) #mičem veće od 1000 kvadrata
podaci = subset(podaci, kvadrat > 500) #mičem jeftinije od 500€ po kvadratu

#brišemo koji nam više ne trebaju
rm(broj); rm(cijena); rm(datum); rm(datumi); rm(i); rm(informacije); rm(j); rm(kvadrat); rm(kvadratura); rm(LINK); rm(link1); rm(linkovi); rm(lokacija);
rm(naslov); rm(new_values); rm(objekt); rm(old_values); rm(opis); rm(opis1); rm(replacer); rm(sobe); rm(lookup_table); rm(page); rm(pagezadatum); rm(podaci2);


#---------------------------------------------------------------PODATCI PO ZUPANIJI---------------------
# sve županije
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


# prosječni kvadrat po županiji
avg_cro <- aggregate(kvadrat ~ županija, data = podaci, FUN = mean)

# standardna devijacija
sd_cro <- aggregate(kvadrat ~ županija, data = podaci, FUN = sd)

# koeficijent varijacije po županiji
cv_cro <-
  data.frame(županija = avg_cro$županija,
             cv = sd_cro$kvadrat / avg_cro$kvadrat)

# broj oglasa po županiji
cnt_cro <-
  aggregate(lokacija ~ županija, data = podaci, FUN = length)

# spajanje gore navedenih podataka
cro_data <-
  merge(merge(avg_cro, cv_cro, by = "županija"), cnt_cro, by = "županija")
cro_data <-
  merge(žup_cro, cro_data, by = "županija", all.x = TRUE)
names(cro_data) <-
  c("županija", "avgKvadrata", "koefVarKvadrata", "brojOglasa")

#brisanje
rm(žup_cro,
   avg_cro,
   sd_cro,
   cv_cro,
   cnt_cro)

#---------------------------------------------------------------KRUŽNI DIJAGRAM---------------------------------------------------------------

# županije sortirane silazno po broju oglasa
cro_data_sorted <- cro_data[order(-cro_data$brojOglasa),]

# top x županija po broju oglasa
x <- 5
top_x_zupanija <- data.frame(županija = cro_data_sorted$županija[1:x], 
                             brojOglasa = cro_data_sorted$brojOglasa[1:x])

# ostale županije i zbroj njihovih oglasa
ostali_brojOglasa <- sum(cro_data_sorted$brojOglasa[x:length(cro_data_sorted$brojOglasa)], na.rm = TRUE)
ostali_zupanija <- data.frame(županija = "Ostali", brojOglasa = ostali_brojOglasa)

# spajanje u jednu tablicu
top_x_ostali <- rbind(top_x_zupanija, ostali_zupanija)


# brojevi oglasa -> postotak
piepercent <- round(100*top_x_ostali$brojOglasa/sum(top_x_ostali$brojOglasa), 1)
piepercent[length(piepercent)] <- round(100 - sum(piepercent[-length(piepercent)]), 1)
# kružni dijagram
pie(piepercent,
    labels = paste(top_x_ostali$županija, paste0(piepercent, "%")),
    main = "Kružni dijagram", 
    col = rainbow(length(top_x_ostali$brojOglasa)))


rm(ostali_brojOglasa, ostali_zupanija, x, piepercent)


#---------------------------------------------------------------VIOLINSKI DIJAGRAM---------------------------------------------------------------

# sve županije
all_zupanije <- unique(cro_data_sorted$županija)

# županije koje nisu u top_x_zupanija i zamijeni ih s 'Ostali'
ostale_zupanije <- setdiff(all_zupanije, top_x_zupanija$županija)
podaci_renamed <-
  within(podaci, županija[županija %in% ostale_zupanije] <-
           'Ostali')

# željeni poredak
podaci_renamed$županija <-
  factor(podaci_renamed$županija, levels = top_x_ostali$županija)

# violinski dijagram
ggplot(data = podaci_renamed, aes(x = županija, y = kvadrat, color = županija)) +
  geom_violin() +
  labs(title = "Violinski dijagram",
       x = "Županija",
       y = "Kvadratura") +
  geom_boxplot(width = 0.1) +
  guides(color = "none")


rm(cro_data_sorted, all_zupanije, ostale_zupanije, podaci_renamed, top_x_ostali, top_x_zupanija)


#---------------------------------------------------------------KARTA I PODACI (HRVATSKA)------------------

# karta
cro_map <-
  readOGR(folder, "Croatia_map")

# spajanje prijašnjih podataka s kartom
cro_data_map <-
  merge(
    cro_map,
    cro_data,
    by.x = "County",
    by.y = "županija",
    all.x = TRUE
  )
# pogodniji format
cro_data_map <- st_as_sf(cro_data_map)   

#brisanje
rm(cro_map)

#---------------------------------------------------------------PLOTIRANJE (HRVATSKA)----------------------
# prosječna cijena po kvadratu (Hrvatska)
ggplot(cro_data_map) +
  geom_sf(aes(fill = round(avgKvadrata, 2)),
          color = "gray",
          lwd = 1) +
  scale_fill_gradient(
    low = "#efedf5",
    high = "#3f007d",
    # raspon boja županija
    # opcionalno za boju onih bez podataka ide: na.value = "boja"
    breaks = c(1000, 1650, 2250, 2850, 3500),
    limits = c(1000, 3500)
  ) +
  theme_void() +       # makne rešetke preko karte
  labs(title = "Prosječna cijena po kvadratu u Hrvatskoj",
       fill = "Cijena po metru kvadratnom (€)",
       caption = "Zelene županije - značajna količina podataka\n
       Crne županije - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(avgKvadrata) |
        is.null(avgKvadrata),
      County,  # ako nema podataka, onda samo ime županije
      paste0(County, "\n", "€", sprintf("%.2f", round(avgKvadrata, 2)))
    )),
    size = 2.5,
    fontface = "bold",
    color = ifelse(
      cro_data_map$brojOglasa < 40 |
        is.na(cro_data_map$brojOglasa) |
        is.null(cro_data_map$brojOglasa),
      "black",
      "darkgreen"
    ),  # zeleni imaju značajno podataka, crni ne
    fill = "#F2F3F4",
    label.padding = unit(0.065, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0,
    alpha = 0.5,
    fill.alpha = 0.5, color.alpha = 1
  ) +
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.caption = element_text(size = 14))


# koeficijent varijacije kvadrature (Hrvatska)
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
      paste0(County, "\n", sprintf("%.2f", round(koefVarKvadrata, 4) * 100), "%")
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
    label.padding = unit(0.065, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0,
    alpha = 0.5,
    fill.alpha = 0.5, color.alpha = 1
  ) +
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.caption = element_text(size = 14))

rm(cro_data_map)
#---------------------------------------------------------------KARTA I PODACI (ZAGREB)-------------------
# svi kvartovi
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

# prosječni kvadrat po kvartovima
avg_zg <- aggregate(kvadrat ~ lokacija, data = zg_data, FUN = mean)

# standardna devijacija
sd_zg <- aggregate(kvadrat ~ lokacija, data = zg_data, FUN = sd)

# koeficijent varijacije po kvartovima
cv_zg <-
  data.frame(lokacija = avg_zg$lokacija,
             cv = sd_zg$kvadrat / avg_zg$kvadrat)

# broj oglasa po kvartovima
cnt_zg <-
  aggregate(kvadrat ~ lokacija, data = zg_data, FUN = length)

# spajanje gore navedenih podataka
zg_data <-
  merge(merge(avg_zg, cv_zg, by = "lokacija"), cnt_zg, by = "lokacija")
zg_data <-
  merge(kvart_zg, zg_data, by = "lokacija", all.x = TRUE)
names(zg_data) <-
  c("lokacija", "avgKvadrata", "koefVarKvadrata", "brojOglasa")  #stupce preimenovat

# karta
zg_map <-
  readOGR(folder, "Zagreb_map")

# spajanje prijašnjih podataka s kartom
zg_data_map <-
  merge(zg_map,
        zg_data,
        by.x = "District",
        by.y = "lokacija",
        all.x = TRUE)
# pogodniji format
zg_data_map <- st_as_sf(zg_data_map)  


#brisanje
rm(zg_data,
   kvart_zg,
   avg_zg,
   sd_zg,
   cv_zg,
   cnt_zg,
   zg_map)

#---------------------------------------------------------------PLOTIRANJE (ZAGREB)-----------------------
# prosječna cijena po kvadratu (Zagreb)
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
    fill = "#F2F3F4",
    label.padding = unit(0.065, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0,
    alpha = 0.5,
    fill.alpha = 0.5, color.alpha = 1
  ) +
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.caption = element_text(size = 14))

# koeficijent varijacije kvadrature (Zagreb)
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
       caption = "Zeleni kvartovi - značajna količina podataka\n
       Crni kvartovi - manja količina podataka") +
  geom_sf_label(
    aes(label = ifelse(
      is.na(koefVarKvadrata) |
        is.null(koefVarKvadrata),
      District,
      paste0(District, "\n", sprintf("%.2f", round(koefVarKvadrata, 4) * 100), "%")
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
    label.padding = unit(0.065, "lines"),
    label.r = unit(0.2, "lines"),
    label.size = 0,
    alpha = 0.5,
    fill.alpha = 0.5, color.alpha = 1
  ) +
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.caption = element_text(size = 14))

rm(zg_data_map,cro_data)

#---------------------------------------------------------------REGRESIJA-----------------------------------------------------------

### graf cijenje i kvadrature svih oglasa (kontrolni graf) ###
p3<-ggplot(podaci, aes(y = cijena, x = kvadratura )) + 
  geom_point(shape=18, color="deepskyblue3")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  coord_cartesian(ylim = c(20000, 250000),
                  xlim = c(15,120), expand = FALSE) +
  ylab("Cijena stana") +
  xlab("Kvadratura") +
  ggtitle("Cijena po m2") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p3 #prokaz grafa
model <- lm(cijena ~ kvadratura, data = podaci) #definiram regresiju
summary(model) #prokaz koeficjenata
rm(model,p3) #brisanje varijabli

#---------------------------------------------------------------INDEXI-----------------------------------------------------------

B=1 #B je koji ces bazni index da ti bude

#definiram vektore za spremanje
cjene = c()
velicina = c()
mjesec = c()

#za cjene iz 2022
filter=subset(podaci, year(podaci$datumi)==2022)
for(i in 4:12){
  mjesec = append(mjesec,paste(as.numeric(i),"/","2022",sep="",collapse=NULL))
  filter2=subset(filter,month(filter$datumi)==i)
  velicina=append(velicina,length(filter2$kvadrat))
  cjene = append(cjene,mean(filter2$kvadrat))
}
# za cjene iz 2023
filter=subset(podaci, year(podaci$datumi)==2023)
for(i in 1:3){
  mjesec = append(mjesec,paste(as.numeric(i),"/","2023",sep="",collapse=NULL))
  filter2=subset(filter,month(filter$datumi)==i)
  velicina=append(velicina,length(filter2$kvadrat))
  cjene = append(cjene,mean(filter2$kvadrat))
}

#racunam indekse
cjeneB=(cjene/cjene[B])*100

#kreiram tablicu
tablica=data.frame(cjene,cjeneB,velicina,mjesec)
Q=tablica #za tablicu

#samo mjsece probacijen u tip datuma
tablica$mjesec <- as.Date(paste0("01/", tablica$mjesec), format = "%d/%m/%Y")

# Prikaz indexa (komentirat s deckima ocemo maknit ili ostavit ovu mracnu liniju)
index <- ggplot(tablica, aes(x=mjesec, y=cjene)) +
  geom_line(size = 1.5, color = "red") + 
  geom_point(color = "black") + 
  #geom_smooth(method = "lm", se = TRUE, level=0.95)+
  scale_y_continuous(breaks = seq(2800,3500,100)) +
  scale_x_date(date_breaks = "2 months") +
  ylab("Cijena kvadrata u €") +
  xlab("") + 
  ggtitle("Cijene stanova dataseta po datumu") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
theme_linedraw()
index #prikaz grfa

#koef korelacije
paste("Koeficjent koorelacije je:")
cor(cjene,c(1:12))

#moja regresija
model <- lm(cjene ~ c(1:12), data = tablica)
paste("Koeficjenti regresije su:")   
summary(model)

Q #prikaz tablice

#rjesavanje varijabli
rm(B,cjeneB,cjene,i,mjesec,velicina,filter,filter2,index,model,tablica,Q)

#---------------------------------------------------------------RIJEČI-----------------------------------------------------------



X3_3_Puni_opis = podaci
options(scipen=0)

#------------------------------- Analiza broja rijeci u oglasima ---------------------------------

# Stvaranje novog stupca gdje ce pisati broj rijeci u oglasu za svaki opis
X3_3_Puni_opis$br_rijeci = NA 
opis=X3_3_Puni_opis$opis
for (x in 1:length(podaci$naslov)) {
  my_string <- opis[x]
  X3_3_Puni_opis$br_rijeci[x] <- length(strsplit(as.character(my_string), "\\w+")[[1]])
}
rm(x,my_string)

# Prikaz korelacije 
p1<-ggplot(X3_3_Puni_opis, aes(y = kvadrat, x = br_rijeci )) + 
  geom_point(shape=18, color="deepskyblue3")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  ylab("Cijena po m2") +
  xlab("Broj riječi u oglasu") +
  ggtitle("Utjecaj broja riječi na cijenu kvadrata stana") +
  coord_cartesian(xlim = c(0,600), ylim = c(0,7500)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p1
rm(p1)

# Linearna regresija za analizu
summary(lm(kvadrat~ br_rijeci, X3_3_Puni_opis))
# Test znacajnosti korelacije
cor.test(X3_3_Puni_opis$br_rijeci, X3_3_Puni_opis$kvadrat, alternative = "greater")


#--------------------------------------- Analiza određenih riječi ----------------------------------------

#------ Trazenje najfrekventnijih rijeci --------

# Novi vektor gdje ce se nalaziti sve rijeci iz svih opisa
v = character() 
for (y in 1:length(podaci$naslov)) {
  a=strsplit(str_squish(gsub("[^[:alpha:]]"," ",opis[y]))," ")
  v=c(v,a)
}
rm(y,a)

# Stvaranje Corpusa
text_podaci<-Corpus(VectorSource(v)) 
rm(v)

# Čišćenje Corpusa
text_1<-tm_map(text_podaci, removeNumbers)
text_1<-tm_map(text_podaci, removePunctuation)
rm(text_podaci)

# Stvaranje matrice za analizu
dtm<- TermDocumentMatrix(text_1)
m<-as.matrix(dtm)
k<-sort(rowSums(m), decreasing = TRUE)
word_freq <- data.frame(word = names(k), frequency = k)

# Prikaz tablice frekvencija rijeci
View(word_freq)


rm(dtm,m,k,text_1)


#----------- Stvaranje grafova za analizu određenih riječi ------------


#--- Stvaranje stpaca koji sadrze 1 ako se u oglasu spominje odredena rijec, a 0 ako se ne spominje ---

# Stavranje stupaca
X3_3_Puni_opis$more = NA
X3_3_Puni_opis$parkirno = NA
X3_3_Puni_opis$prizemlje = NA
X3_3_Puni_opis$terasa = NA

# Stvaranje vektora gdje dijelim na oglase s odredenom rijeci i bez te rijeci
da_more = vector()
ne_more = vector()
da_parking = vector()
ne_parking = vector()
da_prizemlje = vector()
ne_prizemlje = vector()
da_terasa = vector()
ne_terasa = vector()

for (i in 1:length(podaci$naslov)) {
  string_opis = opis[i]
  string_opis = tolower(string_opis)
  
  if (grepl("\\bmor[[:alpha:]]\\b",string_opis)) {
    X3_3_Puni_opis$more[i] = 1
    da_more=c(da_more,X3_3_Puni_opis$kvadrat[i])
  } else {
    X3_3_Puni_opis$more[i] = 0
    ne_more=c(ne_more,X3_3_Puni_opis$kvadrat[i])
  }
  
  if (grepl("\\bparkir[[:alpha:]]*\\b",string_opis)) {
    X3_3_Puni_opis$parkirno[i] = 1
    da_parking=c(da_parking,X3_3_Puni_opis$kvadrat[i])
  } else {
    X3_3_Puni_opis$parkirno[i] = 0
    ne_parking=c(ne_parking,X3_3_Puni_opis$kvadrat[i])
  }
  
  if (grepl("\\bprizem[[:alpha:]]*\\b",string_opis)) {
    X3_3_Puni_opis$prizemlje[i] = 1
    da_prizemlje=c(da_prizemlje,X3_3_Puni_opis$kvadrat[i])
  } else {
    X3_3_Puni_opis$prizemlje[i] = 0
    ne_prizemlje=c(ne_prizemlje,X3_3_Puni_opis$kvadrat[i])
  }
  
  if (grepl("\\bteras[[:alpha:]]*\\b",string_opis)) {
    X3_3_Puni_opis$terasa[i] = 1
    da_terasa=c(da_terasa,X3_3_Puni_opis$kvadrat[i])
  } else {
    X3_3_Puni_opis$terasa[i] = 0
    ne_terasa=c(ne_terasa,X3_3_Puni_opis$kvadrat[i])
  }
}
rm(i,string_opis,opis)


#-- Filtriranje data seta ---

df.1 <- X3_3_Puni_opis  %>%
  mutate(more_1 = case_when(more == "1" ~ "Stan je blizu mora",
                            more == "0" ~ "Stan nije blizu mora")) %>% 
  mutate(prizemlje_1 = case_when(prizemlje == "1" ~ "Stan je u prizemlju",
                                 prizemlje == "0" ~ "Stan nije u prizemlju")) %>% 
  mutate(parkirno_1 = case_when(parkirno == "1" ~ "Stan ima parking",
                                parkirno == "0" ~ "Stan nema parking")) %>% 
  mutate(terasa_1 = case_when(terasa == "1" ~ "Stan ima terasu",
                              terasa == "0" ~ "Stan nema terasu")) 


#-- Regresijski grafovi kada u opisu je rijec more i kada nije ---

p4<-ggplot(df.1, aes(y = cijena, x = kvadratura )) + 
  geom_point(shape=18, color="deepskyblue3")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  facet_grid(~more_1) +
  ylab("Cijena stana") +
  xlab("Kvadratura") +
  ggtitle("Cijena po m2") +
  coord_cartesian(xlim = c(0,300), ylim = c(0,1000000)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p4
rm(p4)
# Test razlike sredina kod rijeci more
t.test(da_more, ne_more)

#-- Regresijski grafovi kada u opisu je riječ parkirno i kada nije ---

p5<-ggplot(df.1, aes(y = cijena, x = kvadratura )) + 
  geom_point(shape=18, color="darkgreen")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  facet_grid(~parkirno_1) +
  ylab("Cijena stana") +
  xlab("Kvadratura") +
  ggtitle("Cijena po m2") +
  coord_cartesian(xlim = c(0,300), ylim = c(0,1000000)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p5
rm(p5)
# Test razlike sredina kod rijeci parkirno
t.test(da_parking, ne_parking)

#-- Regresijski grafovi kada u opisu je riječ prizemlje i kada nije ---

p6<-ggplot(df.1, aes(y = cijena, x = kvadratura )) + 
  geom_point(shape=18, color="firebrick")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  facet_grid(~prizemlje_1) +
  ylab("Cijena stana") +
  xlab("Kvadratura") +
  ggtitle("Cijena po m2") +
  coord_cartesian(xlim = c(0,300), ylim = c(0,1000000)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p6
rm(p6)
# Test razlike sredina kod rijeci prizemlje
t.test(da_prizemlje, ne_prizemlje)

#-- Regresijski grafovi kada u opisu je riječ terasa i kada nije ---

p7<-ggplot(df.1, aes(y = cijena, x = kvadratura )) + 
  geom_point(shape=18, color="darkturquoise")+
  geom_smooth(method=lm, se=FALSE, 
              color="black") +
  facet_grid(~terasa_1) +
  ylab("Cijena stana") +
  xlab("Kvadratura") +
  ggtitle("Cijena po m2") +
  coord_cartesian(xlim = c(0,300), ylim = c(0,1000000)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))
p7
rm(p7)
# Test razlike sredina kod riječi terasa
t.test(da_terasa, ne_terasa)

rm(list = ls.str(mode = 'numeric'))

# Regresijska analiza
m3<-lm(cijena~kvadratura + more,df.1)
m4<-lm(cijena~kvadratura + prizemlje,df.1)
m5<-lm(cijena~kvadratura + terasa,df.1)
m6<-lm(cijena~kvadratura + parkirno,df.1)
stargazer(list(m3,m4,m5,m6), type = "text")
rm(m3,m4,m5,m6,df.1)
