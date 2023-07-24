library(shiny)
library(leaflet)
library(sf)
library(shinydashboard)
library(rnaturalearth)
library(rnaturalearthdata)

# Chargement des données géographiques de l'Afrique de l'Ouest
west_africa <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")
donnes <- read.csv("ACLED-Western_Africa.csv")
#Définition de l'interface graphique
ui <- dashboardPage(
  # Définition de l'apparence du tableau de bord
  dashboardHeader(title = "Actes de violence"),
  dashboardSidebar(
    #Réservation de l'emplacement pour la sélection du Type des événements
    selectInput("evenement", "Sélectionnez un évènement", 
                choices = c(unique(donnes$type)),
                selected = unique(donnes$type)[sample(1:length(unique(donnes$type)), size = 1)],
                multiple = TRUE,),
    #Réservation de l'emplacement pour la sélection des pays
    selectInput("pays", "Sélectionnez un pays ", choices = c(unique(donnes$pays)),
                selected = unique(donnes$pays)[sample(1:length(unique(donnes$pays)),size = 1)],
                multiple = TRUE),
    #Réservation de l'emplacement pour la sélection des années
    selectInput("annee", "Sélectionnez vos années", choices = c(unique(donnes$annee)), 
                selected = unique(donnes$annee)[sample(1:length(unique(donnes$annee)), size = 1)], 
                multiple = TRUE)
  ),
  #Configuration du Body
  dashboardBody(
    #Réservation de l'emplacement pour la carte interactive
    tabItem(tabName = "carte",
            leafletOutput("map", width = "100%", height = "720px"),
            tags$style(".legend { position: bottomright; background-color: white; padding: 10px; }")
    )
  )
)

# Définition du serveur
server <- function(input, output, session) {
  # Réaction pour détecter les changements dans input$pays
  observeEvent(input$pays, {
    # Si input$pays est NULL (sélection effacée), réaffectation de la valeur par défaut pour éviter les erreurs
    if (is.null(input$pays)) {
      updateSelectInput(session, "pays", selected = "Senegal")
    }
  })
  observeEvent(input$pays, {
    # Si input$pays est NULL (sélection effacée), réaffectation de la valeur par défaut pour éviter les erreurs
    if (is.null(input$pays)) {
      updateSelectInput(session, "pays", selected = "Senegal")
    }
  })
  # Filtrage des données
  filtered_data <- reactive({
    donnes[donnes$pays %in% input$pays &
             donnes$type %in% input$evenement & 
             donnes$annee %in% input$annee, ]
  })
  
  # Paramétrage des éléments visibles sur la carte
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Création de la variable "color" en fonction du type d'événement
    data$color <- ifelse(data$type == unique(donnes$type)[1], "red",
                         ifelse(data$type == unique(donnes$type)[2], "blue",
                                ifelse(data$type == unique(donnes$type)[3], "green",
                                       ifelse(data$type == unique(donnes$type)[4], "orange",
                                              ifelse(data$type == unique(donnes$type)[5], "purple",
                                                     "yellow")))))
    
    #définition des coordonnées du point de départ de l'affichage sur la carte interactive
    if (length(input$pays) == 1 || mean(data$latitude) != 0 || mean(data$longitude) != 0) {
      # Si un seul pays est sélectionné, on visualise la carte à partir du centre de ce pays
      center_lat <- mean(data$latitude)
      center_lng <- mean(data$longitude)
      zoom_level <- 6
    } else {
      # Sinon, on utilise des coordonnées par défaut pour permettre d'avoir une vue d'ensemble sur la carte
      center_lat <- 12.6
      center_lng <- -5
      zoom_level <- 5
    }
    # Calcul du nombre de violences pour chaque couleur
    violences_par_couleur <- reactive({
      data <- filtered_data()
      
      data$color <- ifelse(data$type == unique(donnes$type)[1], "red",
                           ifelse(data$type == unique(donnes$type)[2], "blue",
                                  ifelse(data$type == unique(donnes$type)[3], "green",
                                         ifelse(data$type == unique(donnes$type)[4], "orange",
                                                ifelse(data$type == unique(donnes$type)[5], "purple",
                                                       "yellow")))))
      res <- table(data$color)
      noms <- table(data$type)
      df <- data.frame(couleur = names(res), Type = names(noms), Nombre_de_violences = as.numeric(res))
      # Ajout de la somme de tous les événements pour permettre le décompte par pays
      total_violences <- sum(df$Nombre_de_violences)
      df <- rbind(df, c("#FFFFFF", "Total", total_violences))
      return(df)
    })
    
    #Affectation du centre de visualisation
    leaflet() %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
      addTiles() %>% #Affichage des autres pays avec leur noms
      addPolygons(
        #sélection et coloriage en bleu clair avec contours en noir des pays sélectionnés
        data = if (length(input$pays) >= 1 & length(input$pays) <= 15) {
          ne_countries(scale = "medium", country = unique(input$pays))
        } else {
          west_africa
        },
        #si aucun pays n'est sélectionné, on affiche l'afrique de l'ouest
        fillColor = "lightblue",
        color = "black",
        fillOpacity = 0.3
      ) %>%
      #Définition des emplacements sur la carte 
      addCircleMarkers(data = data,
                       lng = ~longitude, 
                       lat = ~latitude, 
                       popup = ~type,# quand on clique sur le point, il affiche le type d'évènement
                       radius = 3, #rayon des points qui peut être modifié en fonction de du type d'évenement ou d'une autre variable
                       #Définiion des couleurs du contour des cercles associés à chaque point déssiné suivant son type
                       color = data$color,
                       #définition des couleurs d'intérieur du point représenté
                       fillColor = data$color,
                       fillOpacity = 1,#Réglage de l'oppacité(clareté)
                       #label = ~paste("Total :", "tets"), au cas où on veut mettre du texte quand la souris survole un point représenté
                       labelOptions = labelOptions(style = list("font-weight" = "bold"))#Mise en gras du texte
      ) %>%
      addLegend(position = "topright", colors = violences_par_couleur()$couleur, labels = paste(violences_par_couleur()$Type,violences_par_couleur()$Nombre_de_violences), title = "Types des événements")#ajout de la légende
  })
}

shinyApp(ui = ui, server = server)
