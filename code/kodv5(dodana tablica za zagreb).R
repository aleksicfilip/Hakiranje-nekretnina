library(rvest)
library(dplyr)




#vrtim prvih x stranica oglasa u foru
for(i in 1:10){
  #kreiram link kao kombinaciju pocetka i broja stranice i ucitavam ga u r
  link1 = "https://besplatnioglasi.eu/nekretnine/stanovi?lang=hr&page="
  broj = as.character(i);
  LINK = paste(link1,broj,sep="",collapse=NULL)
  page = read_html(LINK)
  
  
  #1. preuzimam dio di su linkovi i spremam kao stringovi, 2. micem prvih 10 slova,
  #3. uzimam sve do razmaka, 4. skidam zadnje slovo
  linkovi = page %>% html_nodes(".titleLink") %>% as.character()
  linkovi=substring(linkovi, first=10, last = 1000000L)
  linkovi=sub("\\ .*", "", linkovi)
  linkovi=gsub('.{1}$', '', linkovi)
  
  #definiram polje gdje cu spremat datume i opis
  datumi=c()
  opis=c()
  #for petlja koja za svaki link ude u stranicu oglasa
  #nade klasu koju trazim i dade string koji sredim u datum (ili opis)
  for(j in 1:length(linkovi))
  {
    pagezadatum=read_html(linkovi[j]) #ucita str
    datum = pagezadatum %>% html_nodes(".detailedTitleHolder") %>% html_text() #nade element i pljune text
    datum=sub("\\Šifra oglasa:.*", "", datum) #daj sve do sifre oglasa
    datum=gsub('.{3}$', '', datum) # izbrisi zadnja 3 elementa
    datum = substr(datum, nchar(datum)-9, nchar(datum)) #daj zadnjih 9 elemenata
    datumi = append(datumi,datum) #dodaj moj datum u listu datuma
    
    opis1 = pagezadatum %>% html_nodes(".detailDataDescription") %>% html_text() #skupljm opis
    opis = append(opis,opis1) #dodajem opis vektoru
  }
  
  
  #prebacivanje datuma iz stringa u date(mjesec i godina)
  datumi = strptime(datumi, format = "%d.%m.%Y")
  datumi = as.Date(datumi)
  #datumi <- as.yearmon(datumi)
  
  
  #vadenje vektora sa naslovom i cjenom
  naslov = page %>% html_nodes(".titleLink") %>% html_text()
  cijena =  page %>% html_nodes(".actualValue") %>% html_text()
  
  #vađenje 4 puta veceg vektora koji sadrzi: kvadraturu,objekt,sobe,lokaciju
  informacije = page %>% html_nodes(".dataHalfValue") %>% html_text()
  
  #sad moran sredit informacije jer tocno svaki 4. daje istu informaciju
  kvadratura = subset(informacije,c(1:length(informacije))%%4 == 1)
  objekt = subset(informacije,c(1:length(informacije))%%4 == 2)
  sobe = subset(informacije,c(1:length(informacije))%%4 == 3)
  lokacija = subset(informacije,c(1:length(informacije))%%4 == 0)
  
  #uredivanje cijene
  cijena = sub(".*~ ", "", cijena) 
  cijena = sub(" .*", "", cijena)
  
  #micanje m2
  kvadratura = gsub('.{3}$', '', kvadratura)
  
  #micanje tocke iz cijene(272.345 -> 272345) zbog racunanja
  cijena = gsub("[[:punct:]]", "", cijena)
  
  #pribacivanje stringa u broj
  cijena = as.numeric(cijena)
  kvadratura = as.numeric(kvadratura)
  
  
  kvadrat = cijena/kvadratura
  kvadrat = round(kvadrat, 2)
  
  #ako vadim s prve stranice onda kreiram data.frame, a ako s stranica iza onda ono što izvadim spajam s prijasnjim data.frame
  if(i==1){podaci = data.frame(naslov,cijena,kvadratura ,objekt,sobe,lokacija,opis, kvadrat,datumi,stringsAsFactors = FALSE)}
  else {podaci2 = data.frame(naslov,cijena,kvadratura ,objekt,sobe,lokacija,opis, kvadrat,datumi, stringsAsFactors = FALSE); podaci=rbind(podaci,podaci2);}
}

#ovo je subset onih n-torki koje su u Gradu Zagrebu
samo_zagreb = subset(podaci, lokacija == "Brezovica" |
                       lokacija == "Črnomerec" |
                       lokacija == "Donja Dubrava" |
                       lokacija == "Donji Grad" |
                       lokacija == "Gornja Dubrava" |
                       lokacija == "Gornji Grad - Medveščak" |
                       lokacija == "Maksimir" |
                       lokacija == "Novi Zagreb - Istok" |
                       lokacija == "Novi Zagreb - Zapad" |
                       lokacija == "Pešćenica - Žitnjak" |
                       lokacija == "Podsljeme" |
                       lokacija == "Podsused - Vrapče" |
                       lokacija == "Sesvete" |
                       lokacija == "Stenjevec" |
                       lokacija == "Trešnjevka - Jug" |
                       lokacija == "Trešnjevka - Sjever" |
                       lokacija == "Trnje" |
                       lokacija == "Zagreb - Okolica" 
)

#ovo je subset svih lokacija koje nisu u Gradu Zagrebu
osim_zagreba = anti_join(podaci, samo_zagreb)

#novi filtrirani dataset
df = subset(podaci, kvadrat > 500)
#kremiram csv tamo gdje mu kazem
write.csv(df, "/Users/filip/OneDrive/Radna površina/stanovi/analiza.csv")
write.csv(samo_zagreb, "/Users/filip/OneDrive/Radna površina/stanovi/analiza_zg.csv")
write.csv(osim_zagreba, "/Users/filip/OneDrive/Radna površina/stanovi/analiza_osim_zg.csv")

