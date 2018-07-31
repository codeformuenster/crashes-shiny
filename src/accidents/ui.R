library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- navbarPage("Münsteraner Verkehrsunfälle", id = "nav",
                 
   tabPanel("Interaktive Karte",
            leafletOutput("karte")
    ),
           
	tabPanel("Rohdaten", div(h3("TODO")),
	         br()
	         #DT::dataTableOutput("accidents_data"),
	         ),
	
	tabPanel("Über diese Seite",
	         h4("Datenquelle"),
	         p("TODO, Polizei Münster? Lizenz? "), 
	         br(),
	         h4("Lizenz des Quelltexts"),
	         p("Ein Projekt von ",
	         	a(href = "https://codeformuenster.org", "Code for Münster"),
	         	"Lizenziert unter der ",
	         	a(href = "https://github.com/codeformuenster/accidents#rechtliches", "GPLv3 (mehr Infos zur Lizenz hier)."),
         	"Ideen und Feedback willkommen!
Datenquelle: TODO Polizei Münster? Lizenz?")
	)
)
