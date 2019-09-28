library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)

# define type of loading animation
options("spinner.type" = 4)

ui <- navbarPage(
   
   "Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            p("Tipp: Zum Filtern der Daten unter die Karte scrollen. Datenquelle: Polizei Münster (Achtung: Datensatz noch nicht ganz vollständig)."),
            withSpinner(leafletOutput("karte", height = "600"))
            ), # end tabPanel Karte
   
   tabPanel("Rohdaten",
            p("Tipp: Zum Filtern der Daten unter die Tabelle scrollen. Datenquelle: Polizei Münster (Achtung: Datensatz noch nicht ganz vollständig). Die Originaldaten lassen sich als Excel-Dateien", a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken", target = "_blank", "hier (Ordner Rohdaten) herunterladen."), "Gefilterte Teilmengen lassen sich mit dem Knopf unten rechts herunterladen."),
            withSpinner(DT::dataTableOutput("crashes_table"))
            ), # end tabPanel Rohdaten
   
   tabPanel("Über diese Seite",
            h4("Datenquelle"),
            p("Dank einer Anfrage bei der Polizei Münster über ",
              a(href = "https://fragdenstaat.de/anfrage/rohdaten-der-verkehrsunfallstatistik-munster/",
                target = "_blank",
                "Frag den Staat"),
              "sind wir an Rohdaten der Verkehrsunfälle der Jahre 2007 bis 2014 gekommen. ",
              "Die Daten ab 2015 haben wir dankenswerterweise über den ADFC bekommen.",
              "Nach mehreren Versuchen ist es uns gelungen, die zwischen den Jahren unterschiedlichen Spaltennamen",
              "und Formate in eine Datenbank zu laden, um sie zu visualisieren und so einfach zugänglich zu machen.",
              "Bedauerlicherweise werden die Orte der Verkehrsunfälle in einem schwer ",
              "maschinenlesbaren Format aufgenommen (z.B.: 'Kappenberger Damm, Höhe Kriegerweg').",
              "Wir haben es uns nun zur Aufgabe gemacht alle Unfälle durchzugehen und den korrekten Ort einzupflegen.",
              HTML("<b>Momentan sind weder alle Daten visualisiert noch kann für die Korrektheit der Daten garantiert werden.</b>")),
            p("Die Rohdaten lassen sich ",
              a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken",
                target = "_blank",
                "hier herunterladen.")),
            h4("Lizenz des Quelltexts"),
            p("Ein Projekt von ",
              a(href = "https://codeformuenster.org", target = "_blank", img(src = "cfm_logo.png", alt = "Code for Münster."))),
            p("Lizenziert unter der ",
              a(href = "https://github.com/codeformuenster/crashes-shiny#rechtliches",
                target = "_blank",
                "GPLv3 (mehr Infos zur Lizenz hier).")),
            p("Ideen und Feedback willkommen!",
              a(href = "https://github.com/codeformuenster/crashes-shiny/issues", target = "_blank", "Zum Beispiel auf github"),
              " oder ", a(href = "mailto:muenster@codefor.de", target = "_blank", "per e-Mail.")),
            # whitespace to move filters below (nicht schön, aber selten!)
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
            ), # end tabPanel about
   
   tabPanel("Impressum & Datenschutz",
           includeMarkdown("impressum-datenschutz.md")
   ),
   
   fluidRow(
     column(3,
            selectizeInput(
              "vehicles",
              "mit Verkehrsmittel (UND-Verknüpfung)",
              choices = c("Fuß" = "ped",
                          "Rad" = "bike",
                          "PKW" = "car",
                          "LKW" = "truck",
                          "Bus" = "bus",
                          "sonstiges" = "rest"),
              selected = c("bike", "car"),
              multiple = TRUE,
              options = list(
                'plugins' = list('remove_button'))
              ),
            selectizeInput(
              "without_vehicles",
              "ohne Verkehrsmittel (UND-Verknüpfung)",
              choices = c("Fuß" = "ped",
                          "Rad" = "bike",
                          "PKW" = "car",
                          "LKW" = "truck",
                          "Bus" = "bus",
                          "sonstiges" = "rest"),
              selected = c(),
              multiple = TRUE,
              options = list(
                'plugins' = list('remove_button'))
              ),
            selectizeInput(
                 "causer",
                 "HauptverursacherIn (lt. Polizei; ODER-Verknüpfung)",
                 choices = c("Fuß" = "ped",
                             "Rad" = "bike",
                             "PKW" = "car",
                             "LKW/Lieferwagen" = "truck",
                             "Bus" = "bus"),
                 selected = c(),
                 multiple = TRUE,
                 options = list(
                    'plugins' = list('remove_button'))
              ),
            selectizeInput(
              "injured",
              "Verletzte (ODER-Verknüpfung)",
              choices = c("keine Verletzte" = "no_injuries",
                          "Leichtverletzte" = "slightly", 
                          "Schwerverletzte" = "seriously",
                          "Tote" = "dead"),
              selected = c(),
              multiple = TRUE,
              options = list(
                'plugins' = list('remove_button')
                )
              ),
            checkboxInput(
               "age_toggle",
               "nach Alter filtern (ab 2010)",
               value = FALSE),
            uiOutput("age_slider")
            ),
     column(3,
            checkboxInput(
              "bike_helmet",
              "Fahrradhelm? (ab 2015)",
              value = FALSE),
             checkboxInput(
              "single_participant",
              "Alleinunfall",
              value = FALSE),
            selectizeInput(
               "type",
               HTML("Unfalltyp (<a href = 'https://recht.nrw.de/lmi/owa/br_show_anlage?p_id=15549'>Details</a>)"),
               selected = c(1, 2, 3, 4, 5, 6, 7),
               choices = type_table,
               multiple = TRUE, options = list(
                'plugins' = list('remove_button'))
               ),
            textInput(
               "type_detail",
               HTML("dreistelliger Unfalltyp (<a href = 'https://recht.nrw.de/lmi/owa/br_show_anlage?p_id=15549'>Details</a>; mit Komma trennen, ODER-Verknüpfung)"),
               value = ""
            ),
            textInput(
               "crash_cause",
               HTML("zweistellige Unfallursache (<a href = 'https://recht.nrw.de/lmi/owa/br_show_anlage?p_id=15541'>Details</a>; mit Komma trennen, ODER-Verknüpfung)"),
               value = ""
            )
            ),
     column(3,
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
              ),
            actionButton("years_button", 
                         "Alle Jahre", 
                         icon = icon("calendar-plus")),
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
            htmlOutput("number_of_crashes"),
            uiOutput("refresh_button"), # refresh button, see renderUI in server.R
            actionButton("reset_map_button", 
                         "Karte zurücksetzen", 
                         icon = icon("search-minus")),
            downloadButton("downloadData", "gefilterte Daten herunterladen"),
            checkboxInput("markers_toggle",
                          list("Marker", icon("map-marker-alt")),
                          value = TRUE),
            checkboxInput("heatmap_toggle",
                          list("Heatmap", icon("binoculars")),
                          value = TRUE),
            uiOutput("heatmap_size_slider"),
            uiOutput("heatmap_intensity_sider")
             ) # end column
     ), # end fluidRow (filter)
   
   tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
   
   tags$script(src = "https://assets.digitalclimatestrike.net/widget.js", async = NA),
   
   tags$script(HTML(paste0("(function(f, a, t, h, o, m){",
                  "a[h]=a[h]||function(){",
                  "(a[h].q=a[h].q||[]).push(arguments)",
                  "};",
                  "o=f.createElement('script'),",
                  "m=f.getElementsByTagName('script')[0];",
                  "o.async=1; o.src=t; o.id='fathom-script';",
                  "m.parentNode.insertBefore(o,m)",
               "})(document, window, '//fathom.codeformuenster.org/tracker.js', 'fathom');",
               "fathom('set', 'siteId', '", Sys.getenv("FATHOM_SITEID"), "');",
               "fathom('trackPageview');"
               )
        )
   )
) # end navbarpage
