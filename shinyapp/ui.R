library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- navbarPage("Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            div(h5("instabile development Version, Finger weg!")),
            leafletOutput("karte"),
   				 	wellPanel(
   				 	  column(2,
					   				 selectizeInput(
														"vehicles",
														"Unfallbeteiligte?",
														selected = c("car"),
														choices = c("Fuß" = "ped", "Rad" = "bike", "PKW" = "car", "LKW" = "truck", "sonstiges" = "rest"),
														multiple = TRUE, options = list(
														  'plugins' = list('remove_button'))
														)
   				 		),
   				 		column(2,
   				 					 selectizeInput(
											"hit_and_run",
											"Fahrerflucht?",
											selected = "all",
											choices = c("ja" = "yes", "nein" = "no", "egal" = "all"), 
											multiple = FALSE, options = list(
											  'plugins' = list('remove_button'))
											)
   				 		)
   				 	)
    ),
           
	tabPanel("Rohdaten",
	         div(h5("instabile development Version, Finger weg!")),
	         DT::dataTableOutput("accidents_table")
	         ),
	
	tabPanel("Über diese Seite",
	         div(h5("instabile development Version, Finger weg!")),
	         h4("Datenquelle"),
	         p("TODO, Polizei Münster? Lizenz?"),
	         br(),
	         a(href = "https://github.com/codeformuenster/open-data/tree/master/Unfallstatistiken", "hier herunterzuladen."),
	         h4("Lizenz des Quelltexts"),
	         p("Ein Projekt von ",
	         	a(href = "https://codeformuenster.org", "Code for Münster"),
	         	"Lizenziert unter der ",
	         	a(href = "https://github.com/codeformuenster/accidents#rechtliches", "GPLv3 (mehr Infos zur Lizenz hier)."),
         	"Ideen und Feedback willkommen!
Datenquelle: TODO Polizei Münster? Lizenz?")
	)
)
