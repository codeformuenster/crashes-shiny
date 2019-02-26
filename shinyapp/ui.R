library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)

# define type of loading animation
options("spinner.type" = 4)

ui <- navbarPage("Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            div(h5("Beta-Version, Feedback willkommen!")),
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
                       "hour_filter",
                       "Wähle Uhrzeit:",
                       min = 0,
                       max = 24,
                       value = c(0, 24)
                       )
                     ),
              column(3,
                     selectizeInput(
                       "years",
                       "Wähle Jahre:",
                       selected = c(2013, 2014),
                       choices = list("2007", 
                                      "2008",
                                      "2009",
                                      "2010",
                                      "2011",
                                      "2012",
                                      "2013",
                                      "2014"), 
                       multiple = TRUE, options = list(
                         'plugins' = list('remove_button'))
                       ),
                     selectizeInput(
                       "weekdays",
                       "Wähle Wochentage:",
                       selected = c(0, 1, 2, 3, 4, 5, 6),
                       choices = list("Montag" = 1,
                                      "Dienstag" = 2,
                                      "Mittwoch" = 3,
                                      "Donnerstag" = 4,
                                      "Freitag" = 5,
                                      "Samstag" = 6,
                                      "Sonntag" = 0), 
                       multiple = TRUE, options = list(
                         'plugins' = list('remove_button'))
                       )
                     ),
              column(3,
                     selectizeInput(
                       "months",
                       "Wähle Monate:",
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
                       )
                     ),
              column(3,
                     actionButton("update_button", "Aktualisieren", icon = icon("refresh")),
                     checkboxInput("heatmap_toggle",
                                   "Heatmap",
                                   value = TRUE),
                     checkboxInput("markers_toggle",
                                   "Markers",
                                   value = FALSE),
                     sliderInput(
                       "heatmap_size",
                       "Heatmap-Größe (in m)",
                       min = 0, max = 1000,
                       value = 300
                       )
                     ) # end column
              ) # end fluidRow
            ), # end tabPanel Karte
   
   tabPanel("Rohdaten",
            div(h5("Beta-Version, Feedback willkommen!")),
            DT::dataTableOutput("crashes_table")
            ),
   
   tabPanel("Über diese Seite",
            div(h5("Beta-Version, Feedback willkommen!")),
            h4("Datenquelle"),
            p("Dank einer Anfrage bei der Polizei Münster über ",
              a(href = "https://fragdenstaat.de/anfrage/rohdaten-der-verkehrsunfallstatistik-munster/", target = "_blank", "Frag den Staat"),
              "sind wir an Rohdaten der Verkehsunfälle der Jahre 2007 bis 2016 gekommen. Nach mehreren Versuchen ist es uns gelungen, die zwischen den Jahren unterschiedlichen Spaltennamen und Formate in eine Datenbank zu laden um sie zu visualisieren und einfach zugänglich zu machen. Bedauerlicherweise werden die Orte der Verkehrsunfälle in einem schwer maschinenlesbaren Format aufgenommen (z.B.: 'Kappenberger Damm, Höhe Kriegerweg'). Wir haben es uns nun zur Aufgabe gemacht alle Unfälle durchzugehen und den korrekten Ort einzupflegen. Momentan sind weder alle Daten visualisiert noch kann für die Korrektheit der Daten garantiert werden."),
            p("Die Rohdaten lassen sich ", a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken", target = "_blank", "hier herunterladen.")),
            h4("Lizenz des Quelltexts"),
            p("Ein Projekt von ",
              a(href = "https://codeformuenster.org", target = "_blank", "Code for Münster")),
            p("Lizenziert unter der ",
              a(href = "https://github.com/codeformuenster/crashes-shiny#rechtliches", target = "_blank", "GPLv3 (mehr Infos zur Lizenz hier).")),
            p("Ideen und Feedback willkommen!")
            ) # end tabPanel about
   ) # end navbarpage
