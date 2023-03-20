#CRO_DATA I TOP_X_OSTALI SE MORA NAPRAVITI PRIJE OVOGA
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


library(ggplot2)
####################################################################################
####################################################################################


# Create a vector of all županija names
all_zupanije <- unique(cro_data_sorted$županija)

# Get the names of the županije not in top_x_županija
ostale_zupanije <- setdiff(all_zupanije, top_x_zupanija$županija)


# Replace the names of the županije not in top_x_županija with 'Ostali'
podaci_renamed <-
  within(podaci, županija[županija %in% ostale_zupanije] <-
           'Ostali')

# Convert županija variable to a factor with the desired order
podaci_renamed$županija <-
  factor(podaci_renamed$županija, levels = top_x_ostali$županija)

#######################################################################
# Create the violin plot
ggplot(data = podaci_renamed, aes(x = županija, y = kvadrat, color = županija)) +
  geom_violin() +
  labs(title = "Violinice",
       x = "Županija",
       y = "Kvadrat") +
  geom_boxplot(width = 0.1) +
  guides(color = FALSE)


rm(cro_data_sorted, ostali_brojOglasa, all_zupanije, ostale_zupanije, x, ostali_zupanija, podaci_renamed, top_x_ostali, top_x_zupanija)