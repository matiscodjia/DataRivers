###########
# 1- Installation des packages, à ne faire qu'une seule fois
install.packages("keras")
install_keras()

# 2 - Chargement du package
library(keras)

# 3 - Chargement du jeu de données et extraction des jeux de tests et d'entrainement
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Mise en forme des données et visualisation
# 4.1 - Mise sous forme d'un vecteur de chaque image
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# 4.2 Mise à l'échelle des niveaux de gris
x_train <- x_train / 255
x_test <- x_test / 255

# 4.3 Conversion en "dummy variables" des sorties
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# 4.4 Visualiser une image du jeu d'entrainement
library(imager)
ind_train = sample(1:nrow(x_train), 1)
x1 = x_train[ind_train,]
x1 = matrix(x1, nrow = 28, ncol = 28)
x1=as.cimg(x1) # Conversion en format 'image'
plot(x1)
cat("Vrai label : ", max.col(y_train)[ind_train]-1)

# 5 - Définition du réseau de neurones
# 5.1 - Construction couche par couche
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

# 5.2- Précision de paramètres pour le modèle (fonction de coût, façon d'optimiser les coefficients, évaluation du modèle)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# 6- Entrainement du modèle et graphiques illustratifs
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

# 7- Performances sur le jeu de test

# 8- Visualiser les erreurs de prédiction

# 8.1- Trouver toutes les erreurs de prédiction dans le jeu de test

# 8.2- Selection d'une erreur de classification pour visualisation
