palmerpenguins::penguins
view(penguins)
levels(penguins$species)
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species, shape=species) ,na.rm = TRUE) + 
  geom_smooth(method = "lm",na.rm = TRUE) +
  labs(
    title = "Body mass and flipper length",
    caption = "Data come from the palmerpenguins package",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  )


# Dans le dataset penguins il y à 344 lignes et 8 colonnes

# Relation entre deux autre variables
ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Bill length adn depth",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    color = "Species", shape = "Species"
  )
  
ggplot(data = penguins, 
       mapping = aes(x = species, y = bill_depth_mm, color = species, shape = species)) +
  geom_boxplot()

#Relation non- linéaire
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = bill_depth_mm, shape=species) ,na.rm = TRUE) + 
  geom_smooth(na.rm = TRUE) +
  labs(
    title = "Body mass and flipper length",
    caption = "Data come from the palmerpenguins package",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  )

#Couleurs différentes en fonction de l'île
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(penguins, aes(x = fct_infreq(species), fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, mapping = aes(fill=species)) 

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()


ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, mapping = aes(fill = species), color = "white", alpha = 0.7) +
  labs(title = "Histogramme avec courbe de densité",
       x = "Poids corporel (g)", y = "Fréquence") +
  theme_minimal()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

ggplot(penguins, aes(y = fct_infreq(species), fill = species)) +
  geom_bar()
breast_cancer <- read_csv("../../Data/ISTIC/breast_cancer.csv")

ggplot(data = breast_cancer, mapping = aes(x = diagnosis, y = radius_mean, fill = diagnosis)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boîtes à moustaches du rayon moyen en fonction du diagnostic",
       x = "Diagnostic",
       y = "Rayon moyen") +
  theme_minimal()

ggplot(breast_cancer, aes(x = radius_mean, fill = diagnosis, color = diagnosis)) + geom_density(alpha = 0.5)
ggsave(filename = "breastc-plot.png")
ggplot(breast_cancer,aes(x = radius_mean, y = compactness_mean)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~ diagnosis)
