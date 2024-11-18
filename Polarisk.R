# Charger les packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(mclust)
library(ggplot2)
library(stats)
library(gridExtra) 
library(jsonlite)
library(rstudioapi)

####################             Set parameters              ####################

base_path <- "/home/ubuntu/Projects/Java/API-Code-R/src/main/resources/r_scripts"

###############       FONCTION 1 : Multiplication dataframe par vecteur      ################

"Le but est de multiplier le CA à l'hectare par culture en fonction de l'assolement"

multiply_columns <- function(df, vector) {
  # Vérifier si le nombre de colonnes du dataframe correspond à la longueur du vecteur
  if (ncol(df) != length(vector)) {
    stop("Le nombre de colonnes dans le dataframe doit correspondre à la longueur du vecteur.")
  }
  
  # Appliquer la multiplication à chaque colonne
  df[] <- Map(`*`, df, vector)
  
  # Retourner le dataframe modifié
  return(df)
}

#############      FONCTION 2 : Fitter une distribution gaussienne      ####################

fit_and_sample_gaussian_mixture <- function(data, n_samples = 100000) {
  # Vérifier si les données sont suffisantes
  if (length(data) < 20) {
    stop("Il faut au moins 20 données pour ajuster le modèle.")
  }
  
  # Ajuster un modèle de mélange gaussien
  model <- Mclust(data, G = 2)
  summary(model)
  plot(model, what="density")
  summary(model)
  print(model$parameters)
  
  "paramètres modèle"
  n1 <- length(model$parameters$mean)
  n2 <- length(model$parameters$variance$sigmasq)
  
  if (n1 == 2) {
    mu1 <- as.numeric(model$parameters$mean[1])
    mu2 <- as.numeric(model$parameters$mean[2])
  } else {
    mu1 <- as.numeric(model$parameters$mean)
    mu2 <- as.numeric(model$parameters$mean)
  }
  if (n2 == 2) {
    sigma1=sqrt(as.numeric(model$parameters$variance$sigmasq[1]))
    sigma2=sqrt(as.numeric(model$parameters$variance$sigmasq[2]))
  } else {
    sigma1=sqrt(as.numeric(model$parameters$variance$sigmasq))
    sigma2=sqrt(as.numeric(model$parameters$variance$sigmasq))
  }
  
  proportions <- model$parameters$pro
  
  sample1 <- rnorm(n_samples * proportions[1], mean = mu1, sd = sigma1)
  sample2 <- rnorm(n_samples * proportions[2], mean = mu2, sd = sigma2)
  
  # Mélanger les échantillons pour simuler un échantillonnage aléatoire du mélange
  samples <- c(sample1, sample2)
  clean_samples <- samples[!is.nan(samples) & !is.infinite(samples)]
  return(clean_samples)
}


#############      FONCTION 3 : BOOTSTRAP      ####################

"Fonction qui génère les échantillons"
generate_bootstrap_samples <- function(data, num_samples) {
  n <- length(data)
  bootstrap_samples <- list()
  for (i in 1:num_samples) {
    sample_indices <- sample(1:n, size = n, replace = TRUE)
    bootstrap_samples[[i]] <- data[sample_indices]
  }
  return(bootstrap_samples)
}

"Fonction qui créer la distribution finale et apppelle la fonction au dessus"
bootstrap <- function(data) {
  data_bootstrap=sort(data)
  data_a <- data_bootstrap[1:1000]
  data_b <- data_bootstrap[1001:2000]
  data_c <- data_bootstrap[2001:5000]
  data_d <- data_bootstrap[5001:15000]
  data_e <- data_bootstrap[15001:100000]
  
  num_bootstrap_samples_a <- 16
  num_bootstrap_samples_b <- 8
  num_bootstrap_samples_c <- 4
  num_bootstrap_samples_d <- 2
  num_bootstrap_samples_e <- 1
  
  bootstrap_samples_a <- generate_bootstrap_samples(data_a, num_bootstrap_samples_a)
  bootstrap_samples_b <- generate_bootstrap_samples(data_b, num_bootstrap_samples_b)
  bootstrap_samples_c <- generate_bootstrap_samples(data_c, num_bootstrap_samples_c)
  bootstrap_samples_d <- generate_bootstrap_samples(data_d, num_bootstrap_samples_d)
  bootstrap_samples_e <- generate_bootstrap_samples(data_e, num_bootstrap_samples_e)
  
  
  # Ajouter les échantillons bootstrap aux données initiales
  extended_data <- c(data) # Commencer avec l'échantillon original
  for (sample in bootstrap_samples_a) {
    extended_data <- c(extended_data, sample) # Ajouter chaque échantillon bootstrap
  }
  for (sample in bootstrap_samples_b) {
    extended_data <- c(extended_data, sample) # Ajouter chaque échantillon bootstrap
  }
  for (sample in bootstrap_samples_c) {
    extended_data <- c(extended_data, sample) # Ajouter chaque échantillon bootstrap
  }
  for (sample in bootstrap_samples_d) {
    extended_data <- c(extended_data, sample) # Ajouter chaque échantillon bootstrap
  }
  for (sample in bootstrap_samples_e) {
    extended_data <- c(extended_data, sample) # Ajouter chaque échantillon bootstrap
  }
  return(extended_data)
}

#############      FONCTION 4 : STATISTIQUES      ####################

calcul_risque <- function(data1, data2) {
  mean_finale=mean(data1)
  q_courant=quantile(data2,0.14)
  q_exceptionnel=quantile(data2,0.05)
  q_cata=quantile(data2,0.02)
  quantile_modele=list(q_courant,q_exceptionnel,q_cata)
  quantile_modele
  r_courant=(mean_finale-q_courant[[1]])/mean_finale
  r_exceptionnel=(mean_finale-q_exceptionnel[[1]])/mean_finale
  r_cata=(mean_finale-q_cata[[1]])/mean_finale
  r_bootstap=list(r_courant,r_exceptionnel,r_cata)
  risque <- data.frame("Risque Courant" = r_bootstap[[1]],
                       "Risque Exceptionnel" = r_bootstap[[2]],
                       "Risque Catastrophique" = r_bootstap[[3]])
  return(risque)
}

################              TRAITEMENT RENDEMENT            ###################


# Lire la feuille Excel
# Partie fixe du chemin (en dur)
file_name <- "rendements_input.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

data_rendement <- read_excel(file_path, sheet = 1)

# Remplacer les valeurs manquantes par 0 dans toutes les colonnes
data_rendement[is.na(data_rendement)] <- 0

# Obtenir la liste des départements
departments <- unique(data_rendement$Département)
departments
# Obtenir la liste des années et des cultures
years <- unique(data_rendement$Année)
cultures <- unique(data_rendement$Culture)
years
cultures
# Créer un tableau pour chaque département
for (dep in departments) {
  # Filtrer les données pour le département courant
  dep_data <- data_rendement %>% filter(Département == dep)
  
  # Initialiser un tableau vide avec les années en ligne et les cultures en colonnes
  dep_table <- matrix(0, nrow = length(years), ncol = length(cultures))
  rownames(dep_table) <- years
  colnames(dep_table) <- cultures
  
  # Remplir le tableau avec les rendements
  for (i in 1:nrow(dep_data)) {
    year <- dep_data$Année[i]
    culture <- dep_data$Culture[i]
    rendement <- dep_data$Rendement[i]
    dep_table[as.character(year), as.character(culture)] <- rendement
  }
  
  # Convertir le tableau en data frame pour un affichage plus convivial
  dep_table_df <- as.data.frame(dep_table)
  dep_table_df <- cbind(Année = rownames(dep_table_df), dep_table_df)
  
  # Nommer le tableau en fonction du département
  assign(paste0("tableau_", dep), dep_table_df)
}

tableau_20=tableau_94
tableau_75=tableau_95

# Fonction pour remplacer les NaN par 0 et les zéros par la moyenne de la colonne (excluant les zéros et les nuls),
# ou laisser 0 si la moyenne est 0
remplacer_na_zero_par_moyenne <- function(tableau) {
  for (i in seq_along(tableau)) {
    # Remplacer les NaN par 0
    tableau[[i]][is.nan(tableau[[i]])] <- 0
    
    # Calculer la moyenne en excluant les zéros et les valeurs nulles
    moyenne <- mean(tableau[[i]][tableau[[i]] != 0], na.rm = TRUE)
    
    # Vérifier si la moyenne est non-zéro ou si la colonne contient des zéros
    if (!is.nan(moyenne) && moyenne != 0 && any(tableau[[i]] == 0, na.rm = TRUE)) {
      # Remplacer les zéros par la moyenne
      tableau[[i]][tableau[[i]] == 0] <- moyenne
    }
    # Si la moyenne est 0 ou NaN (pas de valeurs non-nulles non-zéro), les zéros restent 0
  }
  return(tableau)
}

# Boucle pour traiter chaque tableau de tableau_1 à tableau_93
for (j in 1:93) {
  # Construire le nom du tableau
  nom_tableau <- paste("tableau", j, sep = "_")
  # Récupérer le tableau par son nom
  tableau_actuel <- get(nom_tableau)
  # Appliquer la fonction pour remplacer les NaN par 0 et ajuster les zéros
  tableau_modifie <- remplacer_na_zero_par_moyenne(tableau_actuel)
  # Sauvegarder les modifications dans le même tableau
  assign(nom_tableau, tableau_modifie)
}

# Pour vérifier un tableau créé, vous pouvez utiliser :
print(tableau_1)
tableau_20=tableau_94
tableau_75=tableau_95


###################         RENDEMENT PRIX A L HECTARE PAR CULTURE ET PAR DEPARTEMLENT      ###########


data_prix <- read_excel(file_path, sheet = 2)
data_prix

for (i in 1:93) {
  # Création dynamique des noms de data frame
  tableau_name <- paste("tableau_", i, sep = "")
  prix_tableau_name <- paste("tableau_prix_", i, sep = "")
  
  # Obtenir le data frame actuel dans la boucle
  current_tableau <- get(tableau_name, envir = .GlobalEnv)
  
  # Identifier les noms de colonnes communs
  common_names <- intersect(names(current_tableau), names(data_prix))
  
  # Initialiser result avec le nombre approprié de lignes
  result <- data.frame(matrix(ncol = length(common_names), nrow = nrow(current_tableau)))
  names(result) <- common_names
  
  # Multiplication des colonnes communes
  for (name in common_names) {
    result[[name]] <- (current_tableau[[name]] * data_prix[[name]]) / 10
  }
  
  # Assigner le résultat dans le nouvel objet data frame avec le nom dynamique
  assign(prix_tableau_name, result, envir = .GlobalEnv)
}



tableau_94=tableau_20
tableau_95=tableau_75
tableau_prix_94=tableau_prix_20
tableau_prix_95=tableau_prix_75
print(tableau_1)

###################### SELECTION DPT ET ASSOLEMENT  ###################

"Département"

# Demander à l'utilisateur d'entrer un numéro de département
#dpt <- readline(prompt = "Entrez le numéro de département : ")

# Partie fixe du chemin (en dur)
file_name <- "assolement_client.json"

# Construction du chemin complet
json_file <- file.path(base_path, file_name)
data <- fromJSON(json_file)

# Vérifier que le département correspond
dpt <- data$dpt
tableau_name1 <- paste("tableau_", dpt, sep = "")
tableau_name2 <- paste("tableau_prix_", dpt, sep = "")

# Accéder aux tableaux dans l'environnement global
tableau_rendement <- get(tableau_name1, envir = .GlobalEnv)
tableau_prix <- get(tableau_name2, envir = .GlobalEnv)

# Afficher les tableaux
print(tableau_rendement)
print(tableau_prix)

# ASSOLEMENT
assolement <- numeric(34)  # Pré-allouer un vecteur de taille 34 avec des zéros

# Remplacer les valeurs des surfaces agricoles par celles du fichier JSON
v1 <- data$v1
v2 <- data$v2
v3 <- data$v3
v4 <- data$v4
v5 <- data$v5
v6 <- data$v6
v7 <- data$v7
v8 <- data$v8
v9 <- data$v9
v10 <- data$v10
v11 <- data$v11
v12 <- data$v12
v13 <- data$v13
v14 <- data$v14
v15 <- data$v15
v16 <- data$v16
v17 <- data$v17
v18 <- data$v18
v19 <- data$v19
v20 <- data$v20
v21 <- data$v21
v22 <- data$v22
v23 <- data$v23
v24 <- data$v24
v25 <- data$v25
v26 <- data$v26
v27 <- data$v27
v28 <- data$v28
v29 <- data$v29
v30 <- data$v30
v31 <- data$v31
v32 <- data$v32
v33 <- data$v33
v34 <- data$v34


# Utiliser sapply pour collecter les valeurs de v1 à v34 dans un vecteur
nom_variables_assolement <- paste("v", 1:34, sep="")
assolement <- sapply(nom_variables_assolement, function(x) get(x))

# Afficher le vecteur de valeurs
print(assolement)
assolement=as.numeric(assolement)
class(assolement)
assolement

"CHIFFRE D'AFFAIRE ATTENDU PAR CULTURE"
tableau_ca_culture=multiply_columns(tableau_prix,assolement)
tableau_ca_culture$CA=rowSums(tableau_ca_culture)
tableau_ca_culture
ca_tot_exploitation <- mean(tableau_ca_culture$CA)
ca_tot_exploitation




"MODELISATION DE GAUSSIENNES"
generated_samples <- fit_and_sample_gaussian_mixture(tableau_ca_culture$CA)



"BOOTSTRAP"
extended_data=bootstrap(generated_samples)


# Visualisation des données étendues
coef_centrage=mean(generated_samples)/mean(extended_data)
mean(generated_samples)
mean(extended_data)
coef_centrage


##############################     STATISTIQUES  ##########################


# Afficher la taille des données étendues
cat("Taille des données étendues:", length(extended_data), "\n")
data_finale=extended_data[!is.na(extended_data)]
length(data_finale)
summary(data_finale)
quantile(data_finale)
r=calcul_risque(generated_samples,data_finale)
r


##################      COMPARAISON DEPARTEMENTALE ############################

#chargement des données départementales
# Partie fixe du chemin (en dur)
file_name <- "sortie_departement.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

total_risque_departement<- read_excel(file_path, sheet = 1)

# Partie fixe du chemin (en dur)
file_name <- "moyenne_dep.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

data_departementale <- read_excel(file_path, sheet = 1)
ligne_departement <- data_departementale[data_departementale$Département == dpt, ]
l=ligne_departement[, (ncol(ligne_departement)-2):ncol(ligne_departement)]
rownames(r) <- "Exploitation"
rownames(l) <- "Moyenne départementale"
# Combiner les deux dataframes en un seul
risque <- rbind(setNames(r, names(l)), l)
risque["Risque Courant"] <- lapply(risque["Risque Courant"], function(x) round(x, 2))
risque["Risque Exceptionnel"] <- lapply(risque["Risque Exceptionnel"], function(x) round(x, 2))
risque["Risque Catastrophique"] <- lapply(risque["Risque Catastrophique"], function(x) round(x, 2))
#output

donnees_dpt <- total_risque_departement[total_risque_departement$Département == dpt, ]

#########################   SIMULATION AVEC ASSURANCE  ###############################

names(assolement) <- paste0("v", 1:34)

####################      Franchise à 15%      ##########################
# Partie fixe du chemin (en dur)
file_name <- "Bilan annee 2022 v3.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

f15<- read_excel(file_path, sheet = 7)
resultat_coef_f15<- assolement
for (i in names(assolement)) {
  index <- match(i, f15$`Code culture`)
  if (!is.na(index)) {
    # Si un index est trouvé et donc un coefficient correspondant existe
    resultat_coef_f15[i] <- assolement[i] * f15$Coefficient[index]
  }
  else{
    resultat_coef_f15[i]=0
  }
}

indices_non_nuls_f15 <- which(resultat_coef_f15 != 0)
somme_resultat_coef_f15 <- sum(resultat_coef_f15[indices_non_nuls_f15])
somme_assolement_f15 <- sum(assolement[indices_non_nuls_f15])
coef_f15 <- somme_resultat_coef_f15 / somme_assolement_f15
coef_f15
####################      Franchise à 20%      ##########################
# Partie fixe du chemin (en dur)
file_name <- "Bilan annee 2022 v3.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)
f20<- read_excel(file_path, sheet = 8)
resultat_coef_f20<- assolement
for (i in names(assolement)) {
  index <- match(i, f20$`Code culture`)
  if (!is.na(index)) {
    # Si un index est trouvé et donc un coefficient correspondant existe
    resultat_coef_f20[i] <- assolement[i] * f20$Coefficient[index]
  }
  else{
    resultat_coef_f20[i]=0
  }
}

indices_non_nuls_f20 <- which(resultat_coef_f20 != 0)
somme_resultat_coef_f20 <- sum(resultat_coef_f20[indices_non_nuls_f20])
somme_assolement_f20 <- sum(assolement[indices_non_nuls_f20])
coef_f20 <- somme_resultat_coef_f20 / somme_assolement_f20

####################      Franchise à 25%      ##########################
# Partie fixe du chemin (en dur)
file_name <- "Bilan annee 2022 v3.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)
f25<- read_excel(file_path, sheet = 9)
resultat_coef_f25<- assolement
for (i in names(assolement)) {
  index <- match(i, f25$`Code culture`)
  if (!is.na(index)) {
    # Si un index est trouvé et donc un coefficient correspondant existe
    resultat_coef_f25[i] <- assolement[i] * f25$Coefficient[index]
  }
  else{
    resultat_coef_f25[i]=0
  }
}

indices_non_nuls_f25 <- which(resultat_coef_f25 != 0)
somme_resultat_coef_f25 <- sum(resultat_coef_f25[indices_non_nuls_f25])
somme_assolement_f25 <- sum(assolement[indices_non_nuls_f25])
coef_f25 <- somme_resultat_coef_f25 / somme_assolement_f25

####################      Franchise à 30%      ##########################
# Partie fixe du chemin (en dur)
file_name <- "Bilan annee 2022 v3.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)
f30<- read_excel(file_path, sheet = 10)
resultat_coef_f30<- assolement
for (i in names(assolement)) {
  index <- match(i, f30$`Code culture`)
  if (!is.na(index)) {
    # Si un index est trouvé et donc un coefficient correspondant existe
    resultat_coef_f30[i] <- assolement[i] * f30$Coefficient[index]
  }
  else{
    resultat_coef_f30[i]=0
  }
}

indices_non_nuls_f30 <- which(resultat_coef_f30 != 0)
somme_resultat_coef_f30 <- sum(resultat_coef_f30[indices_non_nuls_f30])
somme_assolement_f30 <- sum(assolement[indices_non_nuls_f30])
coef_f30 <- somme_resultat_coef_f30 / somme_assolement_f30

if (is.nan(coef_f15)) {
  coef_f15=0
}
if (is.nan(coef_f20)) {
  coef_f20=0
}
if (is.nan(coef_f25)) {
  coef_f25=0
}
if (is.nan(coef_f30)) {
  coef_f30=0
}

coefficients=list(coef_f15,coef_f20,coef_f25,coef_f30)
coefficients
seuil <- quantile(data_finale, 0.3)

data_finalef15=ifelse(data_finale <= seuil, data_finale * (1+coef_f15), data_finale)
data_finalef15=data_finalef15[!is.na(data_finalef15)]
r15=calcul_risque(generated_samples,data_finalef15)

data_finalef20=ifelse(data_finale <= seuil, data_finale * (1+coef_f20), data_finale)
data_finalef20=data_finalef20[!is.na(data_finalef20)]
r20=calcul_risque(generated_samples,data_finalef20)

data_finalef25=ifelse(data_finale <= seuil, data_finale * (1+coef_f25), data_finale)
data_finalef25=data_finalef25[!is.na(data_finalef25)]
r25=calcul_risque(generated_samples,data_finalef25)

data_finalef30=ifelse(data_finale <= seuil, data_finale * (1+coef_f30), data_finale)
data_finalef30=data_finalef30[!is.na(data_finalef30)]
r30=calcul_risque(generated_samples,data_finalef30)


risque_franchise=rbind(r15,r20,r25,r30)
rownames(risque_franchise) <- c("Risque avec franchise à 15%", "Risque avec franchise à 20%", "Risque avec franchise à 25%","Risque avec franchise à 30%")
risque_franchise <- setNames(risque_franchise, c("Risque Courant", "Risque Exceptionnel", "Risque Catastrophique"))
risque_final=rbind(risque,risque_franchise)
risque_final=round(risque_final,2)

###########################             ENSEMBLE ASSOLEMENT           #########################
# Partie fixe du chemin (en dur)
file_name <- "sortie_assolement_programme_franchise.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)
ensemble_assolement<- read_excel(file_path)
ensemble_assolement_dep=ensemble_assolement[ensemble_assolement$departement==dpt,]
ensemble_assolement_dep

colonnes_a_multiplier <- c(paste("v", 1:34, sep=""), "CA")

# Multiplier les colonnes sélectionnées par 110
ensemble_assolement_dep[colonnes_a_multiplier] <- ensemble_assolement_dep[colonnes_a_multiplier] * sum(assolement)
ensemble_assolement_dep



#######################      Différence avec assurance

ca=mean(generated_samples)
ca_risque_courant=ca*(1-r$Risque.Courant)
ca_risque_courant_f15=ca*(1-r15$Risque.Courant)
ca_risque_courant_f20=ca*(1-r20$Risque.Courant)
ca_risque_courant_f25=ca*(1-r25$Risque.Courant)
ca_risque_courant_f30=ca*(1-r30$Risque.Courant)
ca_risque_exceptionnel=ca*(1-r$Risque.Exceptionnel)
ca_risque_catastrophique=ca*(1-r$Risque.Catastrophique)
print(ca_risque_catastrophique)
print(ca_risque_exceptionnel)
print(ca_risque_courant)
print(ca)

output_ca_assurance <- data.frame(
  Col1 = c("Franchise à 15%", ca_risque_courant_f15),
  Col2 = c("Franchise à 20%", ca_risque_courant_f20),
  Col3 = c("Franchise à 25%", ca_risque_courant_f25),
  Col4 = c("Franchise à 30%", ca_risque_courant_f30),
  Col5 = c("Sans assurance", ca_risque_courant)
)


output_ca_pertes_risque <- data.frame(
  Col1 = c("CA Total de l'exploitation", ca_tot_exploitation),
  Col2 = c("Pertes Risque Courant", ca_tot_exploitation - ca_risque_courant),
  Col3 = c("Pertes Risque Exceptionnel", ca_tot_exploitation - ca_risque_exceptionnel),
  Col4 = c("Pertes Risque Catastrophique", ca_tot_exploitation - ca_risque_catastrophique)
)

# Fonction pour calculer la somme des moindres carrés pour une ligne pour savoir l'erreur
sum_of_least_squares <- function(row, assolement) {
  return(sum((row - assolement)^2))
}
cols_to_select <- grep("^v[0-9]+$", colnames(ensemble_assolement_dep), value = TRUE)
# Calculer la somme des moindres carrés pour chaque ligne du dataframe
sum_of_least_squares_per_row <- apply(ensemble_assolement_dep[, cols_to_select], 1, function(row) {
  sum_of_least_squares(row, assolement)
})

# Ajouter les résultats comme une nouvelle colonne appelée 'erreur'
ensemble_assolement_dep$erreur <- sum_of_least_squares_per_row
min_erreur_index <- which.min(ensemble_assolement_dep$erreur)
assolement_propose=ensemble_assolement_dep[min_erreur_index, ]


#############.     Prix du marché       #################
#Partie moyenne anuelle
file_name <- "/analyse risque de marche.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

# Lire l'onglet contenant les données
data_annuelle <- read_excel(file_path, sheet = "Moyenne annuelle JSON", col_names = TRUE)
 print (data_annuelle)
# Convertir en JSON avec une mise en forme lisible
json_data <- toJSON(data_annuelle, pretty = TRUE)

# Afficher le JSON
cat(json_data)

# Partie fixe du chemin (en dur)
file_name <- "Moyenne_anuelle_5_ans.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

write(json_data, file = file_path)

#Partie Protelis
file_name <- "/analyse risque de marche.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

# Lire l'onglet contenant les données
data_annuelle_protelis <- read_excel(file_path, sheet = "Moyenne Protelis JSON", col_names = TRUE)

# Convertir en JSON avec une mise en forme lisible
json_data <- toJSON(data_annuelle_protelis, pretty = TRUE)

print(data_annuelle_protelis)

# Afficher le JSON
cat(json_data)

# Partie fixe du chemin (en dur)
file_name <- "Moyenne_protelis_5_ans.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)


write(json_data, file = file_path)

#############.     Risque de marché       #################
# Partie fixe du chemin (en dur)
file_name <- "/analyse risque de marche.xlsx"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)
risque_marche<- read_excel(file_path,sheet = "Coef")
resultat_coef_risque_marche=assolement

for (i in names(assolement)) {
  index <- match(i, risque_marche$`Code culture`)
  if (!is.na(index)) {
    # Si un index est trouvé et donc un coefficient correspondant existe
    resultat_coef_risque_marche[i] <- assolement[i] * risque_marche$Coefficients[index]
  }
  else{
    resultat_coef_risque_marche[i]=0
  }
}
resultat_coef_risque_marche

indices_non_nuls_r_m <- which(resultat_coef_risque_marche != 0)
somme_resultat_r_m <- sum(resultat_coef_risque_marche[indices_non_nuls_r_m])
somme_assolement_r_m <- sum(assolement[indices_non_nuls_r_m])
coef_risque_marche <- somme_resultat_r_m / somme_assolement_r_m
coef_risque_marche
if (is.nan(coef_risque_marche)) {
  coef_risque_marche=mean(risque_marche$Coefficients)
}
coef_risque_marche

gains_marché <- ca_tot_exploitation*coef_risque_marche

marche <- data.frame(
  Col1 = c("coefficient de risque marché", coef_risque_marche),
  Col2 = c("gains potentiel", gains_marché)
)

###############   Moyene des prix Protelis Exploitation ##############

# Définir le chemin complet du fichier
file_name <- "/analyse risque de marche.xlsx"
file_path <- file.path(base_path, file_name)

# Lecture des données de la feuille "Liste Cultures"

liste_marche <- read_excel(file_path, sheet = "Liste Cultures")


# Calculer les résultats du risque de marché par culture
resultat_culture_risque_marche <- assolement
resultat_culture_risque_marche <- sapply(names(assolement), function(i) {
  index <- match(i, liste_marche$`Code culture`)
  if (!is.na(index)) {
    
    assolement[i] * liste_marche$Coefficients[index]
  } else {
    0
  }
})
resultat_culture_risque_marche <- as.numeric(resultat_culture_risque_marche)

# Suppression de la colonne "Année" de tableau_rendement pour le calcul des hectares

tableau_rendement <- tableau_rendement %>% select(-Année)																						 
tableau_hectare_culture=multiply_columns(tableau_rendement,resultat_culture_risque_marche)

# Calcul de la somme par année
somme_par_annee <- rowSums(tableau_hectare_culture)

# Remplacer les 0 par NA temporairement pour éviter la division par 0
tableau_hectare_culture[tableau_hectare_culture == 0] <- NA

# Calculer les pourcentages
Coeff_culture_risque_marche <- sweep(tableau_hectare_culture, 1, somme_par_annee, FUN = "/")
Coeff_culture_risque_marche[is.na(Coeff_culture_risque_marche)] <- 0 # Remettre les NA en 0

#extraction des années

annees <- colnames(data_annuelle)[grepl("^\\d{4}$", colnames(data_annuelle))]
print(annees)
# 1.1 Sélectionner les années du tableau data_annuelle
data_annuelle[is.na(data_annuelle)] <- 0								   
data_annuelle_filtered <- data_annuelle %>% 
  select(Produit, all_of(annees))

# 1.2 Mettre les produits en index dans les deux tableaux
Coeff_culture_risque_marche_ma <- Coeff_culture_risque_marche
Coeff_culture_risque_marche_ma$Produit <- rownames(Coeff_culture_risque_marche_ma)
rownames(Coeff_culture_risque_marche_ma) <- NULL
colnames(Coeff_culture_risque_marche_ma)[colnames(Coeff_culture_risque_marche_ma) == "Produit"] <- "Année"

# 1.3 Conversion explicite des colonnes des années en numériques
data_annuelle_filtered <- data_annuelle_filtered %>%
  mutate(across(c(all_of(annees)), as.numeric))
data_annuelle_filtered[is.na(data_annuelle_filtered)] <- 0

# Inverser les années et les produits
data_annuelle_filtered <- data_annuelle_filtered %>%
  pivot_longer(cols = all_of(annees), names_to = "Année", values_to = "Valeur")
data_annuelle_filtered <- data_annuelle_filtered %>%
  pivot_wider(names_from = Produit, values_from = Valeur)

data_moyenne_annuelle <- merge(Coeff_culture_risque_marche_ma, data_annuelle_filtered, by = "Année", suffixes = c("_coeff", "_marche"))

# Multiplier les colonnes correspondantes pour chaque culture en fonction des années
for (col in colnames(Coeff_culture_risque_marche_ma)[-1]) {  # Exclure la colonne "Année"
  col_coeff <- paste(col, "_coeff", sep = "")
  col_marche <- paste(col, "_marche", sep = "")
  
  if (col_coeff %in% colnames(data_moyenne_annuelle) && col_marche %in% colnames(data_moyenne_annuelle)) {
    data_moyenne_annuelle[[paste("resultat", col, sep = "_")]] <- data_moyenne_annuelle[[col_coeff]] * data_moyenne_annuelle[[col_marche]]
  }
}

# Supprimer les colonnes intermédiaires des coefficients et des prix
cols_to_remove <- grep("_coeff|_marche", colnames(data_moyenne_annuelle), value = TRUE)
data_moyenne_annuelle <- data_moyenne_annuelle[, !(colnames(data_moyenne_annuelle) %in% cols_to_remove)]

# Supprimer les colonnes qui contiennent uniquement des zéros
cols_with_nonzero_values <- colSums(data_moyenne_annuelle != 0) > 0
data_moyenne_annuelle <- data_moyenne_annuelle[, cols_with_nonzero_values]

# Ajouter une colonne 'total' qui contient la somme de toutes les colonnes de résultats pour chaque ligne
data_moyenne_annuelle$total <- rowSums(data_moyenne_annuelle[ , -1])  # Exclure la colonne "Année" de la somme

# 2.1 Sélectionner les années du tableau data_annuelle
data_annuelle_protelis[is.na(data_annuelle_protelis)] <- 0								   
data_annuelle_protelis_filtered <- data_annuelle_protelis %>% 
  select(Produit, all_of(annees))

# 2.2 Mettre les produits en index dans les deux tableaux
Coeff_culture_risque_marche_mp <- Coeff_culture_risque_marche
Coeff_culture_risque_marche_mp$Produit <- rownames(Coeff_culture_risque_marche_mp)
rownames(Coeff_culture_risque_marche_mp) <- NULL
colnames(Coeff_culture_risque_marche_mp)[colnames(Coeff_culture_risque_marche_mp) == "Produit"] <- "Année"
print(Coeff_culture_risque_marche_mp)
# 2.3 Conversion explicite des colonnes des années en numériques
data_annuelle_protelis_filtered <- data_annuelle_protelis_filtered %>%
  mutate(across(c(all_of(annees)), as.numeric))
data_annuelle_protelis_filtered[is.na(data_annuelle_protelis_filtered)] <- 0

# Inverser les années et les produits
data_annuelle_protelis_filtered <- data_annuelle_protelis_filtered %>%
  pivot_longer(cols = all_of(annees), names_to = "Année", values_to = "Valeur")
data_annuelle_protelis_filtered <- data_annuelle_protelis_filtered %>%
  pivot_wider(names_from = Produit, values_from = Valeur)

data_moyenne_protelis <- merge(Coeff_culture_risque_marche_mp, data_annuelle_protelis_filtered, by = "Année", suffixes = c("_coeff", "_marche"))

# Multiplier les colonnes correspondantes pour chaque culture en fonction des années
for (col in colnames(Coeff_culture_risque_marche_mp)[-1]) {  # Exclure la colonne "Année"
  col_coeff <- paste(col, "_coeff", sep = "")
  col_marche <- paste(col, "_marche", sep = "")
  
  if (col_coeff %in% colnames(data_moyenne_protelis) && col_marche %in% colnames(data_moyenne_protelis)) {
    data_moyenne_protelis[[paste("resultat", col, sep = "_")]] <- data_moyenne_protelis[[col_coeff]] * data_moyenne_protelis[[col_marche]]
  }
}

# Supprimer les colonnes intermédiaires des coefficients et des prix
cols_to_remove <- grep("_coeff|_marche", colnames(data_moyenne_protelis), value = TRUE)
data_moyenne_protelis <- data_moyenne_protelis[, !(colnames(data_moyenne_protelis) %in% cols_to_remove)]

# Supprimer les colonnes qui contiennent uniquement des zéros
cols_with_nonzero_values <- colSums(data_moyenne_protelis != 0) > 0
data_moyenne_protelis <- data_moyenne_protelis[, cols_with_nonzero_values]

# Ajouter une colonne 'total' qui contient la somme de toutes les colonnes de résultats pour chaque ligne
data_moyenne_protelis$total <- rowSums(data_moyenne_protelis[ , -1])  # Exclure la colonne "Année" de la somme


data_moyenne_prix_exploit <- list(
  "Data_Moyenne_Annuelle" = data_moyenne_annuelle,
  "Data_Moyenne_Protelis" = data_moyenne_protelis,
  "Risque marché" = marche
)



###############       DATA VISUALISATION      ####################

#################### Positonnement de l'exploitation

calc_ecdf <- function(data) {
  n <- length(data)
  sort_data <- sort(data)
  return(data.frame(Valeur = sort_data, ECDF = (1:n) / n))
}

# Calculer l'ECDF pour le Risque Courant
ecdf_courant <- calc_ecdf(donnees_dpt$Risque_Courant)
ecdf_exceptionnel <- calc_ecdf(donnees_dpt$Risque_Exceptionnel)
ecdf_cata <- calc_ecdf(donnees_dpt$Risque_Catastrophique)

ecdf_data <- rbind(
  data.frame(Valeur = ecdf_courant$Valeur, ECDF = ecdf_courant$ECDF, Risque = "Courant"),
  data.frame(Valeur = ecdf_exceptionnel$Valeur, ECDF = ecdf_exceptionnel$ECDF, Risque = "Exceptionnel"),
  data.frame(Valeur = ecdf_cata$Valeur, ECDF = ecdf_cata$ECDF, Risque = "Catastrophique")
)
valeur_abscisse=min(ecdf_courant$ECDF[ecdf_courant$Valeur >= r$Risque.Courant])





# liste des pertes potentielles
pertes <- data.frame(
  Col1 = c("Franchise à 15%", (ca_tot_exploitation - ca_risque_courant_f15) + gains_marché),
  Col2 = c("Franchise à 20%", (ca_tot_exploitation - ca_risque_courant_f20) + gains_marché),
  Col3 = c("Franchise à 25%", (ca_tot_exploitation - ca_risque_courant_f25) + gains_marché),
  Col4 = c("Franchise à 30%", (ca_tot_exploitation - ca_risque_courant_f30) + gains_marché),
  Col5 = c("Sans assurance", (ca_tot_exploitation - ca_risque_courant) + gains_marché)
)


ca_risque_courant

assolement_propose

risque_final



############################            OUTPUT.                  #############################
# Partie fixe du chemin (en dur)
file_name <- "output_risques.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_1 <- toJSON(risque_final, pretty = TRUE)
write(json_data_1, file = file_path)

# Partie fixe du chemin (en dur)
file_name <- "assolement_differents.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_2 <- toJSON(assolement_propose, pretty = TRUE)
write(json_data_2, file = file_path)

# Partie fixe du chemin (en dur)
file_name <- "output_pertes.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_3 <- toJSON(pertes, pretty = TRUE)
write(json_data_3, file = file_path)

# Partie fixe du chemin (en dur)
file_name <- "output_ca_assurance.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_4 <- toJSON(output_ca_assurance, pretty = TRUE)
write(json_data_4, file = file_path)

# Partie fixe du chemin (en dur)
file_name <- "output_risque_marche.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_5 <- toJSON(data_moyenne_prix_exploit, pretty = TRUE)
write(json_data_5, file = file_path)

# Partie fixe du chemin (en dur)
file_name <- "output_ca_brut_et_risques.json"

# Construction du chemin complet
file_path <- file.path(base_path, file_name)

json_data_6 <- toJSON(output_ca_pertes_risque, pretty = TRUE)
write(json_data_6, file = file_path)


