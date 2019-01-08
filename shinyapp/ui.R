library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- navbarPage("Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            div(h5("Beta-Version, Feedback willkommen!")),
            leafletOutput("karte"),
            textOutput("number_of_accidents"),
   				 	wellPanel(
   				 	  column(2,
					   				 selectizeInput(
														"vehicles",
														"Unfallbeteiligte?",
														choices = c("Fuß" = "ped", 
														            "Rad" = "bike", 
														            "PKW" = "car", 
														            "LKW" = "truck", 
														            "sonstiges" = "rest"),
														selected = c("car"),
														multiple = TRUE, 
														options = list(
														  'plugins' = list('remove_button'))
														)
					   				 ),
   				 	  column(2,
   				 		       selectizeInput(
   				 		         "injured",
   				 		         "Verletzte",
   				 		         choices = c("Leichtverletzte" = "slightly", 
   				 		                     "Schwerverletzte" = "seriously",
   				 		                     "Tote" = "dead"),
   				 		         selected = c("seriously"),
   				 		         multiple = TRUE,
   				 		         options = list(
														  'plugins' = list('remove_button')
														  )
   				 		         )
   				 		       ),
   				 	  column(2,
   				 	         selectizeInput(
    									"years",
                     	"Wähle Jahre:",
                     	selected = 2010,
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
								  )
   				 	  ),
							column(2,
							       selectizeInput(
							         "months",
							         "Wähle Monate:",
                       	selected = c(1, 10),
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
   				 	  #),
							column(2,
							       selectizeInput(
							         "weekdays",
							         "Wähle Wochentage:",
                       	selected = c(1, 5),
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
  					        )
   				 	  ),
#    				 		column(2,
#    				 					 selectizeInput(
# 											"hit_and_run",
# 											"Fahrerflucht?",
# 											choices = c("ja" = "yes", "nein" = "no", "egal" = "all"),
# 											selected = "all",
# 											multiple = FALSE, options = list(
# 											  'plugins' = list('remove_button'))
# 											)
#    				 		)
#   				 	),
   				 	wellPanel(
   				 	  actionButton("QueryBtn", "Aktualisieren", icon = icon("refresh"))
   				 	)
    ),
           
	tabPanel("Rohdaten",
	         div(h5("Beta-Version, Feedback willkommen!")),
	         DT::dataTableOutput("accidents_table")
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
	         	a(href = "https://github.com/codeformuenster/accidents#rechtliches", target = "_blank", "GPLv3 (mehr Infos zur Lizenz hier).")),
	         p("Ideen und Feedback willkommen!")
	)
)
