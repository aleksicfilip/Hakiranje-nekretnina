podaci <- read.csv("C:/Users/MATE/Desktop/Hackathon/Oglasi.csv")
#CRO_DATA SE MORA NAPRAVITI PRIJE OVOGA
################################################################################
################################################################################

# županije sortirane silazno po broju oglasa
cro_data_sorted <- cro_data[order(-cro_data$brojOglasa),]

# Create a new data frame with the top x županija and their corresponding 'brojOglasa' values
x <- 5
top_x_zupanija <- data.frame(županija = cro_data_sorted$županija[1:x], 
                             brojOglasa = cro_data_sorted$brojOglasa[1:x])

# Calculate the total number of 'brojOglasa' for the remaining županija
ostali_brojOglasa <- sum(cro_data_sorted$brojOglasa[x:length(cro_data_sorted$brojOglasa)], na.rm = TRUE)

# Create a new row for the remaining županija with a label 'ostali'
ostali_zupanija <- data.frame(županija = "Ostali", brojOglasa = ostali_brojOglasa)

#skupa
top_x_ostali <- rbind(top_x_zupanija, ostali_zupanija)

#######################################################################################################################################

piepercent <- round(100*top_x_ostali$brojOglasa/sum(top_x_ostali$brojOglasa), 1)
piepercent[length(piepercent)] <- round(100 - sum(piepercent[-length(piepercent)]), 1)

pie(piepercent,
    labels = paste(top_x_ostali$županija, paste0(piepercent, "%")),
    main = "PITA KRUMPIRUŠA", 
    col = rainbow(length(top_x_ostali$brojOglasa)))


rm(cro_data_sorted, top_x_zupanija, ostali_brojOglasa, ostali_zupanija, top_x_ostali, x, piepercent)