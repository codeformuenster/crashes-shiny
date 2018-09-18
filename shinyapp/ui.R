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
														choices = c("Fuß" = "ped", "Rad" = "bike", "PKW" = "car", "LKW" = "truck", "sonstiges" = "rest"),
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
   				 		         choices = c("Leichtverletzte" = "slightly", "Schwerverletzte" = "seriously", "Tote" = "dead"),
   				 		         selected = c("seriously"),
   				 		         multiple = TRUE,
   				 		         options = list(
														  'plugins' = list('remove_button')
														  )
   				 		         )
   				 		       )#,
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
   				 	),
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
