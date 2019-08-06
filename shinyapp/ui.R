library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)

# define type of loading animation
options("spinner.type" = 4)

ui <- navbarPage("Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            withSpinner(leafletOutput("karte")),
            textOutput("number_of_crashes"),
            fluidRow(
              column(3,
                     selectizeInput(
                       "vehicles",
                       "Unfallbeteiligte",
                       choices = c("Fuß" = "ped", 
                                   "Rad" = "bike", 
                                   "PKW" = "car",
                                   "LKW" = "truck", 
                                   "sonstiges" = "rest"),
                       selected = c("bike", "car"),
                       multiple = TRUE, 
                       options = list(
                         'plugins' = list('remove_button'))
                       ),
                     selectizeInput(
                       "injured",
                       "Verletzte",
                       choices = c("Leichtverletzte" = "slightly", 
                                   "Schwerverletzte" = "seriously",
                                   "Tote" = "dead"),
                       selected = c("slightly"),
                       multiple = TRUE,
                       options = list(
                         'plugins' = list('remove_button')
                         )
                       ),
                     sliderInput(
                       "age_filter",
                       "Alter:",
                       min = 0,
                       max = 100,
                       value = c(0, 100)
                       )
                     ),
              column(3,
                     checkboxInput(
                       "bike_helmet",
                       "Fahrradhelm?",
                       value = FALSE),
                      checkboxInput(
                       "single_participant",
                       "Alleinunfall",
                       value = FALSE),
                     sliderInput(
                       "hour_filter",
                       "Uhrzeit:",
                       min = 0,
                       max = 24,
                       value = c(0, 24)
                       ),
                     selectizeInput(
                       "weekdays",
                       "Wochentage:",
                       selected = c(0, 1, 2, 3, 4, 5, 6),
                       choices = weekdays_string_to_numbers, 
                       multiple = TRUE, options = list(
                         'plugins' = list('remove_button'))
                       )
                     ),
              column(3,
                     selectizeInput(
                       "months",
                       "Monate:",
                       selected = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       choices = list("Januar" = 1, 
                                      "Februar" = 2, 
                                      "März" = 3, 
                                      "April" = 4, 
                                      "Mai" = 5,
                                      "Juni" = 6,
                                      "Juli" = 7,
                                      "August" = 8,
                                      "September" = 9,
                                      "Oktober" = 10, 
                                      "November" = 11,
                                      "Dezember" = 12), 
                       multiple = TRUE, options = list(
                         'plugins' = list('remove_button'))
                       ),
                     selectizeInput(
                       "years",
                       "Jahre:",
                       selected = c(2018),
                       choices = list("2007", 
                                      "2008",
                                      "2009",
                                      "2010",
                                      "2011",
                                      "2012",
                                      "2013",
                                      "2014",
                                      "2015",
                                      "2016",
                                      "2017",
                                      "2018"),
                       multiple = TRUE, options = list(
                         'plugins' = list('remove_button'))
                       )
                     ),
              column(3,
                     actionButton("update_button", 
                                  "Aktualisieren", 
                                  icon = icon("refresh")),
                     checkboxInput("heatmap_toggle",
                                    list("Heatmap", icon("binoculars")),
                                   value = TRUE),
                     checkboxInput("markers_toggle",
                                   list("Marker", icon("map-marker-alt")),
                                   value = FALSE),
                     sliderInput(
                        "heatmap_size",
                        "Heatmap-Größe (in m)",
                        min = 0, max = 1000,
                        value = 300),
                      actionButton("reset_map_button", 
                                  "Karte zurücksetzen", 
                                  icon = icon("search-minus"))
                      ) # end column
              ) # end fluidRow
            ), # end tabPanel Karte
   
   tabPanel("Rohdaten",
            p("Die Rohdaten lassen sich ", a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken", target = "_blank", "hier (Ordner Rohdaten) herunterladen.")),
            withSpinner(DT::dataTableOutput("crashes_table"))
            ),
   
   tabPanel("Über diese Seite",
            h4("Datenquelle"),
            p("Dank einer Anfrage bei der Polizei Münster über ",
              a(href = "https://fragdenstaat.de/anfrage/rohdaten-der-verkehrsunfallstatistik-munster/", target = "_blank", "Frag den Staat"),
              "sind wir an Rohdaten der Verkehrsunfälle der Jahre 2007 bis 2014 gekommen. Die Daten ab 2015 haben wir dankenswerterweise über den ADFC bekommen. Nach mehreren Versuchen ist es uns gelungen, die zwischen den Jahren unterschiedlichen Spaltennamen und Formate in eine Datenbank zu laden um sie zu visualisieren und einfach zugänglich zu machen. Bedauerlicherweise werden die Orte der Verkehrsunfälle in einem schwer maschinenlesbaren Format aufgenommen (z.B.: 'Kappenberger Damm, Höhe Kriegerweg'). Wir haben es uns nun zur Aufgabe gemacht alle Unfälle durchzugehen und den korrekten Ort einzupflegen. Momentan sind weder alle Daten visualisiert noch kann für die Korrektheit der Daten garantiert werden."),
            p("Die Rohdaten lassen sich ", a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken", target = "_blank", "hier herunterladen.")),
            h4("Lizenz des Quelltexts"),
            p("Ein Projekt von ",
              a(href = "https://codeformuenster.org", target = "_blank", "Code for Münster.")),
            p("Lizenziert unter der ",
              a(href = "https://github.com/codeformuenster/crashes-shiny#rechtliches", target = "_blank", "GPLv3 (mehr Infos zur Lizenz hier).")),
            p("Ideen und Feedback willkommen!", a(href = "https://github.com/codeformuenster/crashes-shiny/issues", target = "_blank", "Zum Beispiel auf github"), " oder ", a(href = "mailto:muenster@codefor.de", target = "_blank", "per e-Mail.") )
            ) # end tabPanel about
   ) # end navbarpage
