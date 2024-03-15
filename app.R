# Shinyapp de cartographie de la BD sols
#CANTONAL


#Packages ----
library(shiny)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(ggridges)
library(sf)
library(RODBC)
library(viridis)
library(RColorBrewer)
library(maptools)
library(maps)
library(ggspatial)
library(magrittr)
library(plotly)

#Data ----
#dataframe brut : sortie directe d'access 
bd <- read.csv("data/BD.csv", sep = ";", dec = ",")

colnames(bd) %<>% tolower()

bd$codepostal_parcelle <- as.numeric(as.character(bd$codepostal_parcelle))

bd <- bd %>%
    filter(codepostal_parcelle >= 11000 & codepostal_parcelle < 12000) %>% 
    #on selectionne les analyses dans l'aude uniquement
    rename(commune = commune_parcelle)

#fichier contenant le code insee, nom de la commune, nom du canton et culture dominante
villes <- read.csv("data/villes.csv")

bd <- villes %>%
    right_join(bd, by = "commune") %>%
    rename(CANTON = canton)

shp <- read_sf("data/CANTONS.shp") %>% st_transform(2154)

# Definir l'interface utilisateur -----------
ui <- fluidPage(
    
    # Titre du logiciel
    titlePanel(paste("Cartographie de la BD du laboratoire sols de la Chambre
                     d'Agriculture de l'Aude. Decoupage cantonal.")),
    
    # Barre laterale : selection des parametres par l'utilisateur
    sidebarLayout(
        sidebarPanel(
            
            #Input ----
            selectInput(inputId = "var",
                        label = "Choisissez la variable d'interet",
                        choices = c("Matiere Organique",
                                    "Carbone",
                                    "Azote total",
                                    "C/N",
                                    "CEC",
                                    "Argile",
                                    "Limons fins",
                                    "Limons grossiers",
                                    "Sables fins",
                                    "Sables grossiers",
                                    "pH",
                                    "Potentiel de stockage de MO (Hassink et al, 97)" = "Potentiel",
                                    "Remplissage du potentiel de stockage (Hassink et al, 97)" = "Remplissage",
                                    "k2 selon la formule de Girard et al (2011)" = "k2",
                                    "Indice de pouvoir chlorosant" = "IPC",
                                    "Phosphate",
                                    "Magnesie",
                                    "Potasse",
                                    "Oxyde de Calcium",
                                    "Oxyde de Sodium",
                                    "Fer",
                                    "Zinc",
                                    "Cuivre",
                                    "Manganese"
                        )),
            
            br(),
            
            sliderInput(inputId = "periode",
                        label = "Choisissez la periode qui vous interesse (bornes incluses)",
                        min = 1998, max = 2030,
                        value = c(1998,2004),
                        sep = ""),
            
            br(),
            
            sliderInput(inputId = "profondeur",
                        label = "Choisissez la profondeur d'echantillonage en cm (bornes incluses)",
                        min = 0, max = 100, value = c(0,30)),
                     
                     
            checkboxInput(inputId = "NAprof",
                          label = "Inclure les donnees sans profondeur indiquee"),
            
            br(),
            
            radioButtons(inputId = "graphtype",
                         label = "Comment souhaitez vous representer le nombre d'analyses par canton ?",
                         choices = c("Afficher sur une carte a part" = "nw_map",
                                     "Ne pas representer (deconseille)" = "none")),
            
            actionButton(inputId = "go",
                         label = "Generer la carte"),
            
            br(),
            br(),
            
            htmlOutput("CC")
        ),
        
        #outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Carte simple",
                                 plotOutput("carte1"),
                                 verbatimTextOutput("warning1")
                                 ),
                        
                        
                        tabPanel("Carte interactive",
                                 plotlyOutput("carte2"),
                                 verbatimTextOutput("warning2")
                                 )
            )
        )
        
    )
)

# Definir le serveur du programme
server <- function(input, output) {

    #output : licence CC ----
    output$CC <- renderUI({
        HTML("<a rel='license' href='http://creativecommons.org/licenses/by/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by/4.0/88x31.png' /></a><br />Ce logiciel est soumis a une licence <a rel='license' href='http://creativecommons.org/licenses/by/4.0/'>Creative Commons Attribution 4.0 International License</a>")
    })
    
    
    
    #on demande de demarrer uniquement si le bouton go est active
    v <- reactiveValues(doPlot = FALSE)
    
    #le bouton go compte le nombre de fois ou il est clique. On le transforme
    #en une valeur reactive booleenne : 
    #des qu'il est clique, sa valeur augmente d'un, v$doPlot prendra la valeur TRUE
    #et tout ce qui depend de v$doPlot est refait
    observeEvent(input$go, {
        # 0 sera compris comme FALSE
        # 1+ sera compris comme TRUE
        v$doPlot <- input$go
    })
    
    data <- reactive({
        
        #filtrer la profondeur
        if(input$NAprof == FALSE) {
            bd <- bd %>% filter(!is.na(profondeur_prelevement)) }
        
        #creer le df en fonction de la variable selectionnee ----
        switch(input$var, 
               "Matiere Organique" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, mo) %>%
                   filter(mo!=0 & !is.na(mo)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(med = median(mo)/10, n = n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,1,1.25,1.5,1.75,2,100),
                                    labels = c("<1%", "1-1.25%", "1.25-1.5%",
                                               "1.5-1.75%", "1.75-2%", ">2%"))),
               
               "Carbone" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, carbone) %>%
                   filter(carbone!=0 & !is.na(carbone)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(med = median(carbone), n = n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,5,7.5,9,10.5,12,600),
                                    labels = c("<5","5-7.5","7.5-9","9-10.5",
                                               "10.5-12",">12"))),
               
               "Azote total" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, azote_total) %>%
                   filter(azote_total!=0 & !is.na(azote_total)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(med = median(azote_total), n = n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,0.25,0.5,0.75,1,1.5,70),
                                    labels = c("<0.25","0.25-0.5","0.5-0.75",
                                               "0.75-1","1-1.5",">1.5"))),
               
               "C/N" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, carbone, azote_total) %>%
                   filter(carbone != 0 & azote_total != 0 & !is.na(carbone)
                          & !is.na(azote_total)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(c = median(carbone), az = median(azote_total), n = n()) %>%
                   
                   mutate(med = c/az,
                          
                          var = cut(med,
                                    breaks = c(0,6,8,10,12,14,1000),
                                    labels = c("tres faible (<6)", "faible (6-8)", 
                                               "normal (8-10)", 
                                               "legerement eleve (10-12)", 
                                               "eleve (12-14)", "tres eleve (>14)"))),
               
               "Argile" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, argile) %>%
                   filter(argile != 0 & !is.na(argile)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(med = median(argile)/10, n=n()) %>%
                   mutate(var = cut(med*10,
                                    breaks = c(0,100,200,250,300,400,1000),
                                    labels = c("<10%", "10-20%", "20-25%",
                                               "25-30%","30-40%", ">40%"))),
               
               "Limons fins" =  bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement, limons_fins) %>%
                 filter(limons_fins != 0 & !is.na(limons_fins)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 summarise(med = median(limons_fins)/10, n=n()) %>%
                 mutate(var = cut(med*10,
                                  breaks = c(0,100,200,250,300,400,1000),
                                  labels = c("<10%", "10-20%", "20-25%",
                                             "25-30%","30-40%", ">40%"))),
               
               "Limons grossiers" =  bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement, limons_grossiers) %>%
                 filter(limons_grossiers != 0 & !is.na(limons_grossiers)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 summarise(med = median(limons_grossiers)/10, n=n()) %>%
                 mutate(var = cut(med*10,
                                  breaks = c(0,100,200,250,300,400,1000),
                                  labels = c("<10%", "10-20%", "20-25%",
                                             "25-30%","30-40%", ">40%"))),
               
               "Sables fins" =  bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement, sables_fins) %>%
                 filter(sables_fins != 0 & !is.na(sables_fins)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 summarise(med = median(sables_fins)/10, n=n()) %>%
                 mutate(var = cut(med*10,
                                  breaks = c(0,100,200,250,300,400,1000),
                                  labels = c("<10%", "10-20%", "20-25%",
                                             "25-30%","30-40%", ">40%"))),
               "Sables grossiers" =  bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement, sables_grossiers) %>%
                 filter(sables_grossiers != 0 & !is.na(sables_grossiers)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 summarise(med = median(sables_grossiers)/10, n=n()) %>%
                 mutate(var = cut(med*10,
                                  breaks = c(0,100,200,250,300,400,1000),
                                  labels = c("<10%", "10-20%", "20-25%",
                                             "25-30%","30-40%", ">40%"))),
               
               
               
               "pH" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement, ph) %>%
                   filter(ph != 0 & !is.na(ph)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   summarise(med = median(ph), n=n()) %>%
                   mutate(var = cut(med,
                                    breaks = c(0,6,6.5,7,7.5,8,8.5,20),
                                    labels = c("<6","6-6.5","6.5-7",
                                               "7-7.5","7.5-8","8-8.5",
                                               ">8.5"))),
               
               "Potentiel" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          argile, limons_fins) %>%
                   filter(argile != 0 & limons_fins != 0 &
                              !is.na(argile) & !is.na(limons_fins)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   mutate(potentiel = 1.72 * (6.9 + 0.029 * (argile + limons_fins))) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(potentiel), n=n()) %>%
                   mutate(var = cut(med,
                                    breaks = c(0,25,30,35,40,45,50,100),
                                    labels = c("<2.5%","2.5-3%","3-3.5%",
                                               "3.5-4%","4-4.5%","4-5%",
                                               ">5%"))),
               
               "Remplissage" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          argile, limons_fins, mo) %>%
                   filter(argile != 0 & limons_fins != 0 &
                              !is.na(argile) & !is.na(limons_fins) &
                              mo != 0 & !is.na(mo)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   mutate(potentiel = 1.72 * (6.9 + 0.029 * (argile + limons_fins))) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(pot = median(potentiel), mo = median(mo), n=n()) %>%
                   mutate(med = mo * 0.85/pot,
                       
                       var = cut(med,
                                    breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1000),
                                    labels = c("<10%", "10-20%", "20-30%", "30-40%", 
                                               "40-50%", "50-60%", "60-70%", "70-80%",
                                               "80-90%", "90-100%", ">100%"))),
               
               "k2" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          argile, calcaire_total) %>%
                   filter(argile != 0 & !is.na(argile) &
                              calcaire_total != 0 & !is.na(calcaire_total)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   mutate(k2 = 100*(0.6*13.8 - 3) / ((1 + 0.005 * argile) * (100 + 0.15 * calcaire_total))) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(var = median(k2), med = median(k2), n=n()),
               
               "IPC" = bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement,
                        fer, calcaire_actif) %>%
                 filter(fer != 0 & !is.na(fer) &
                          calcaire_actif != 0 & !is.na(calcaire_actif)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 mutate(ipc = ((calcaire_actif/10) / fer)*10000) %>%
                 
                 group_by(CANTON) %>%
                 
                 summarise(med = floor(median(ipc)), n=n()) %>%
                 mutate(var = cut(med,
                                  breaks = c(0,100,250,500,750,1000,1500,10000),
                                  labels = c("<100","100-250","250-500",
                                             "500-750","750-1000","1000-1500",">1500"))),
               
               
               
               
               "Phosphate" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          phosphate) %>%
                   filter(phosphate != 0 & !is.na(phosphate)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(phosphate), n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0, 80, 120, 200, 300, 4000),
                                    labels = c("<80", "80-120 (Satisfaisant)",
                                               "120-200", "200-300", ">300"))),
               
               "CEC" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          cec) %>%
                   filter(cec != 0 & !is.na(cec)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(cec)/10, n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,5,7.5,10,12.5,15,300),
                                    labels = c("<5","5-7.5","7.5-10","10-12.5",
                                               "12.5-15",">15"))),
               
               
               "Magnesie" = bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement,
                        magnesium) %>%
                 filter(magnesium != 0 & !is.na(magnesium)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 
                 summarise(med = median(magnesium)*20.15, n=n()) %>%
                 
                 mutate(var = cut(med,
                                  breaks = c(0,90,140,200,300,4000),
                                  labels = c("<90","90-140","140-200",
                                             "200-300", ">300"))),
               
               "Potasse" = bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement,
                        potassium) %>%
                 filter(potassium != 0 & !is.na(potassium)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 
                 summarise(med = median(potassium)*47.1, n=n()) %>%
                 
                 mutate(var = cut(med,
                                  breaks = c(0,100,150,200,300,50000),
                                  labels = c("<100","100-150",
                                             "150-200","200-300",">300"))),
               
               "Oxyde de Calcium" = bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement,
                        calcium) %>%
                 filter(calcium != 0 & !is.na(calcium)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 
                 summarise(med = median(calcium)*28.1, n=n()) %>%
                 
                 mutate(var = cut(med,
                                  breaks = c(0,25,50,75,100,200,800),
                                  labels = c("<25","25-50","50-75","75-100",
                                             "100-200",">200"))),
               
               "Oxyde de Sodium" = bd %>%
                 select(CANTON, annee_labo, profondeur_prelevement,
                        sodium) %>%
                 filter(sodium != 0 & !is.na(sodium)) %>%
                 filter(annee_labo >= input$periode[1] & 
                          annee_labo <= input$periode[2]) %>%
                 filter(profondeur_prelevement >= input$profondeur[1] &
                          profondeur_prelevement <= input$profondeur[2]) %>%
                 
                 group_by(CANTON) %>%
                 
                 summarise(med = median(sodium)*31, n=n()) %>%
                 
                 mutate(var = cut(med,
                                  breaks = c(0,10,15,20,30,40,50,85,10000),
                                  labels = c("<10","10-15","15-20","20-30",
                                             "30-40","40-50","50-85", ">85"))),
               "Fer" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          fer) %>%
                   filter(fer != 0 & !is.na(fer)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(fer), n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,30,50,65,80,100,1500),
                                    labels = c("<30","30-50","50-65","65-80",
                                               "80-100",">100"))),
               
               "Zinc" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          zinc) %>%
                   filter(zinc != 0 & !is.na(zinc)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(zinc), n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,1,2,4,10,300),
                                    labels = c("<1","1-2","2-4","4-10",
                                               ">10"))),
               
               "Cuivre" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          cuivre) %>%
                   filter(cuivre != 0 & !is.na(cuivre)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(cuivre), n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,5,10,15,25,50,100,400),
                                    labels = c("<5","5-10","10-15","15-25",
                                               "25-50","50-100",">100"))),
               
               "Manganese" = bd %>%
                   select(CANTON, annee_labo, profondeur_prelevement,
                          manganese) %>%
                   filter(manganese != 0 & !is.na(manganese)) %>%
                   filter(annee_labo >= input$periode[1] & 
                              annee_labo <= input$periode[2]) %>%
                   filter(profondeur_prelevement >= input$profondeur[1] &
                              profondeur_prelevement <= input$profondeur[2]) %>%
                   
                   group_by(CANTON) %>%
                   
                   summarise(med = median(manganese), n=n()) %>%
                   
                   mutate(var = cut(med,
                                    breaks = c(0,5,10,15,20,150),
                                    labels = c("<5","5-10","10-15","15-20",">20")))
        )
    })
    
    #creer le titre de la carte ----
    titre <- reactive({
        
        switch(input$var,
               "Matiere Organique" = "Mediane cantonale de matiere organique en %",
               "Carbone" = "Mediane cantonale de carbone en g/kg",
               "Azote total" = "Mediane cantonale d'azote total en g/kg",
               "C/N" = "Mediane cantonale du rapport C/N",
               "CEC" = "Mediane cantonale de CEC METSON en cmol+/kg",
               
               
               "Argile" = "Mediane cantonale de la teneur en argile en %",
               "Limons fins" = "Mediane cantonale de la teneur en limons fins (0.002 a 0.02 mm) en %",
               "Limons grossiers" = "Mediane cantonale de la teneur en limons grossiers (0.02 a 0.05 mm) en %",
               "Sables fins" = "Mediane cantonale de la teneur en sables fins (0.05 a 0.2 mm) en %",
               "Sables grossiers" = "Mediane cantonale de la teneur en sables grossiers (0.2 a 2 mm) en %",
               
               
               
               "pH" = "Mediane cantonale du pH",
               "Potentiel" = "Mediane cantonale du potentiel de stockage de 
MO selon Hassink et al, 1997",
               "Remplissage" = "Remplissage du potentiel de stockage de MO
selon Hassink et al, 1997",
               "k2" = "Mediane cantonale du coefficient de mineralisation
selon la formule de Girard et al, 2011",
               "IPC" = "Mediane communale de l'indice de pouvoir chlorosant",
               
               
               
               "Phosphate" = "Mediane cantonale de phosphates en mg/kg",
               "Magnesie" = "Mediane cantonale de magnesie (MgO) en mg/kg",
               "Potasse" = "Mediane cantonale de potasse (K2O) en mg/kg",
               "Oxyde de Calcium" = "Mediane cantonale de calcium (CaO) en cmol+/kg",
               "Oxyde de Sodium" = "Mediane cantonale d'oxyde de sodium (NaO) en mg/kg",
               
               
               "Fer" = "Mediane cantonale de Fer en mg/kg",
               "Zinc" = "Mediane cantonale de zinc en mg/kg",
               "Cuivre" = "Mediane cantonale de cuivre en mg/kg",
               "Manganese" = "Mediane cantonale de manganese en mg/kg"
               )
        
    })
    
    #creer l'echelle de la carte ----
    echelle <- reactive({
        
        switch(input$var,
               "Matiere Organique" = scale_fill_viridis(na.value="snow3", discrete = TRUE, begin=1, end=0,
                                                        option="inferno", name="MO"),
               "Carbone" = scale_fill_brewer(palette = "OrRd", name = "Carbone"),
               "Azote total" = scale_fill_brewer(palette = "Blues", name = "Azote"),
               "C/N" = scale_fill_brewer(palette = "RdYlGn", name = "C/N"),
               "CEC" = scale_fill_brewer(palette = "RdPu", name = "CEC"),
               
               
               "Argile" = scale_fill_brewer(palette = "Reds", name = "Argile"),
               "Limons fins"= scale_fill_brewer(palette = "Reds", name = "Limons fins"),
               "Limons grossiers"= scale_fill_brewer(palette = "Reds", name = "Limons grossiers"),
               "Sables fins"= scale_fill_brewer(palette = "Reds", name = "Sables fins"),
               "Sables grossiers"= scale_fill_brewer(palette = "Reds", name = "Sables grossiers"),
               
               
               "pH" =  scale_fill_manual(values = setNames(object = c("#d7191c", "#f07332","#fea31b","#ffeb53",
                                                                      "#bbe274","#74bf96","#2b83ba"), 
                                                           nm = c("<6","6-6.5","6.5-7",
                                                                  "7-7.5","7.5-8","8-8.5",
                                                                  ">8.5")),
                                         name = "pH", na.value = "white"),
               "Potentiel" = scale_fill_viridis(na.value="snow3", discrete = TRUE, begin=1, end=0,
                                                option="inferno", name="Potentiel en %"),
               "Remplissage" = scale_fill_viridis(na.value="snow3", discrete = TRUE, begin=1, end=0,
                                                  option="inferno", name="% du potentiel rempli"),
               "k2" = scale_fill_viridis(direction = -1, name = "k2 en %"),
               "IPC" = scale_fill_brewer(na.value = "white", palette = "YlGn", name = "IPC"),
               
               
               
               "Phosphate" = scale_fill_brewer(palette = "Greens", name = "Phosphates"),
               "Magnesie" = scale_fill_brewer(palette = "RdPu", name = "Magnesie"),
               "Potasse" = scale_fill_brewer(palette = 'Oranges', name = "Potasse"),
               "Oxyde de Calcium" = scale_fill_brewer(palette = "Blues", name = "Calcium"),
               "Oxyde de Sodium" = scale_fill_viridis(discrete = TRUE, name = "Sodium", direction = -1),
               
               
               "Fer" = scale_fill_brewer(palette = "PuBuGn", name = "Fer"),
               "Zinc" = scale_fill_brewer(palette = "BuPu", name = "Zinc") ,
               "Cuivre" = scale_fill_brewer(palette = "BuGn", name = "Cuivre"),
               "Manganese" = scale_fill_brewer(palette = "PuRd", name = "Manganese")
        )
        
    })
    
    #CARTE SIMPLE ----
    output$carte1 <- renderPlot({
        
        if(v$doPlot == FALSE) return ()
        
        #on isole toute la section qui sera activee par le bouton go
        isolate({
            
            #appeler la fonction data() fait revenir le programme a la reactive value data()
            #et la calcule
            plot <- shp %>%
                inner_join(data(), by = "CANTON")
            
            
            #sortir la carte de la variable
            carte_var <- ggplot() +
                
                geom_sf(data = shp, fill = NA, size = 0.2) +
                
                geom_sf(data = plot, aes(fill = var), size = 0.2) +
                echelle() +
                
                ggtitle(titre()) +
                
                annotation_scale(location = "br") +
                annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm")) +
                
                theme_map()
            
            
            #Sortir la carte du nb d'observations ----
            if(input$graphtype == "nw_map") { #si l'utilisateur a selectionne
                #nouvelle carte ----
                
                plot <- plot %>%
                    mutate(nobs = cut(n,
                                      breaks = c(0,10,50,100,200,300,10000),
                                      labels = c("<10","10-50","50-100",
                                                 "100-200","200-300",">300")))
                
                carte_nobs <- ggplot() +
                    
                    geom_sf(data = shp, fill = NA, size = 0.2)  +
                    
                    geom_sf(data = plot, aes(fill = nobs), size = 0.2)  +
                    scale_fill_brewer(palette = "YlOrRd", na.value = "snow3", name = NULL) +
                    
                    theme_map() +
                    
                    ggtitle("Nombre d'observations par canton") +
                    
                    annotation_scale(location = "br") +
                    annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm")) +
                    
                    labs(caption = "Source : Logiciel de cartographie de la BD CA11, L. Caradec, 2020") +
                    theme(plot.caption = element_text(size = 8,hjust = 0))
                    
                
                sortie <- plot_grid(nrow = 2, carte_var, carte_nobs)
            
                
            } else {
                sortie <- carte_var
            }
            
            sortie
            
        })
        

    })
    
    output$warning1 <- renderPrint({ cat("Cette carte represente la mediane sur les anciens cantons (avant 2017) sur la 
periode", input$periode[1],"-",input$periode[2], "et sur l'horizon", input$profondeur[1], "-", input$profondeur[2])
    })
    
    #CARTE INTERACTIVE----------
    output$carte2 <- renderPlotly({
        
        if(v$doPlot == FALSE) return ()
        
        isolate({
            
            plot <- shp %>%
                right_join(data(), by = "CANTON") %>%
                filter(!is.na(CANTON))
            
            plot <- nngeo::st_remove_holes(plot)
            
            plot_centroid <- st_point_on_surface(plot)
            
            carte <- ggplot() +
                geom_sf(data = shp, fill = NA, size = 0.2) +
                geom_sf(data = plot, aes(fill = var), size = 0.2) +
                echelle() +
                
                geom_sf(data = plot_centroid, 
                        aes(text = paste("Canton =", CANTON, "\n",
                                         input$var, "=", round(med,2), "\n",
                                         "Nombre d'observations =", n)),
                        alpha = 0.2) +
                
                ggtitle(titre()) +
                
                theme_map()
            
            ggplotly(carte, tooltip = "text")
            
            
        })
    })
    
    output$warning2 <- renderPrint({ cat(
    "Attention : le procede utilise n'est pas tout a fait adapte pour representer
ce genre de donnees. Certains problemes d'affichage peuvent survenir.
    ",
paste("Cette carte represente la mediane sur les anciens cantons (avant 2017) sur la 
periode", input$periode[1],"-",input$periode[2], "et sur l'horizon", input$profondeur[1], "-", input$profondeur[2])
    )
        })
    
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)




         