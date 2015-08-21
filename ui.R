library(shiny)
library(shinythemes)

dissdata<-read.csv('/lebanon/data/lebanondata.april.csv')
eventdata<-dissdata
colnames(eventdata)[1]<-"DayMonthYear"

shinyUI(
  navbarPage(theme = shinytheme("united"),
             "Lebanon Data Portal",
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(
                          h2(img(src='cedartree.jpg',"  Welcome", width = 55, height = 60)),
                          p("Here you can find near-real time data tracking Lebanon's ongoing refugee crisis along different dimensions:"),
                          tags$ul(tags$li("Conflict Data"),
                                  tags$li("Refugee Data"),
                                  tags$li("Demographic Data")
                          )
                        ),
                        mainPanel(
                          h1(strong(span("Lebanon Conflict Monitor", style="color:green"))),
                          h5("Since the begining of the civil war in 2011 more than 4 million Syrians have fled their homeland. The majority"), 
                          h5("of these refugees have fled to Syria's neighboring states of Iraq, Jordan, Turkey, and Lebanon. While"),
                          h5("substantial numbers of refugees have poured into Jordan and Iraq (600,000+ and 250,00+ respectively), Turkey"), 
                          h5("has opened up its borders to over 1.8 million refugees. While each of these states have shouldered substantial"), 
                          h5("burdens in hosting their Syrian neighbors in such large amounts, perhaps no country has paid as high a price"),
                          h5("as Lebanon, whose population of only 4.4 million is struggling to accomodate more than 1.1 million fleeing"), 
                          h5("Syrians. Nearly one in four individuals currently residing in Lebanon is a Syrian refugee. This project is part of a"),  
                          h5("larger effort to quantify and measure the impact of the regional refugee crisis on Syria and its neighbors in hopes"),
                          h5("that a better understanding  of the costs of war will increase our will to peace.To learn more about the conflict"),
                          h5("and how you can help please visit the", a("International Rescuee Committee", href="http://www.rescue.org/crisis-syria"),"."),
                          h1(strong(span("Data Collection", style="color:green"))),
                          h5("The data on refugees was obtained from the", a("UNHCR Syrian Regional Refugee Response", 
                                                                             href="http://data.unhcr.org/syrianrefugees/regional.php"),". Demographic"), 
                          h5("data was obtained from Lebanon's", a("Central Administration of Statistics", href="http://www.cas.gov.lb"),". The conflict
                             data was collected and hand coded"),
                          h5("based on news articles obtained from", a("Naharnet", href="http://www.naharnet.com"), "and from the Lebanese Ministry
                             of Information's own newswire service", a("NNA", href="http://nna-leb.gov.lb/en"),"." 
                          )
                          )
                          )),
             tabPanel("Maps",
                      tabsetPanel(id='tabs', "Panel 1.x",
                                  tabPanel("Refugees",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Refugee Data", style="color:green")),
                                                                      p("This map shows total refugee numbers by months"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can alter the date with the input widget"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Maps take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot", width="700px",height="700px"))
                                           )
                                  ),
                                  tabPanel("Conflict", 
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Conflict Data", style="color:green")),
                                                                      p("This map shows conflict totals by month"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("Alter the date & conflict type with the input widget"),
                                                                              tags$li("You can also drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Maps take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot2", width="700px",height="700px"))
                                           )),
                                  tabPanel("Population", 
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Population Data", style="color:green")),
                                                                      p("This map shows populaton totals"),
                                                                      p(h5(strong("NOTE:")), "Maps take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot3", width="700px",height="700px"))
                                           )),
                                  tabPanel("Sects", 
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Sectarian Data", style="color:green")),
                                                                      p("This map shows sect proportions"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can alter the sect using the input widget"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Maps take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot4", width="700px",height="700px"))
                                           )),
                                  absolutePanel(top = 420, left = 1040, fixed=FALSE,
                                                draggable = TRUE, 
                                                conditionalPanel(condition= "input.tabs=='Refugees' | input.tabs=='Conflict'",
                                                                 selectInput("monthyear", "Select Date:",
                                                                             choices = list("April 2013"="Apr-2013",
                                                                                            "May 2013"="May-2013",
                                                                                            "June 2013"="Jun-2013",
                                                                                            "July 2013"="Jul-2013",
                                                                                            "August 2013"="Aug-2013",
                                                                                            "September 2013"="Sep-2013",
                                                                                            "October 2013"="Oct-2013",
                                                                                            "November 2013"="Nov-2013",
                                                                                            "December 2013"="Dec-2013",
                                                                                            "January 2014"="Jan-2014",
                                                                                            "February 2014"="Feb-2014",
                                                                                            "March 2014"="Mar-2014",
                                                                                            "April 2014"="Apr-2014",
                                                                                            "May 2014"="May-2014",
                                                                                            "June 2014"="Jun-2014",
                                                                                            "July 2014"="Jul-2014",
                                                                                            "August 2014"="Aug-2014",
                                                                                            "September 2014"="Sep-2014",
                                                                                            "October 2014"="Oct-2014",
                                                                                            "November 2014"="Nov-2014",
                                                                                            "December 2014"="Dec-2014",
                                                                                            "January 2015"="Jan-2015",
                                                                                            "February 2015"="Feb-2015",
                                                                                            "March 2015"="Mar-2015",
                                                                                            "April 2015"="Apr-2015")
                                                                 )
                                                ),
                                                conditionalPanel(condition= "input.tabs=='Sects'",
                                                                 selectInput("sectinput", "Select Sect:",
                                                                             choices = list("Shia"="Shia",
                                                                                            "Sunni"="Sunni",
                                                                                            "Alawite"="Alawaite",
                                                                                            "Greek Orthodox"="Greek.Orthodox",
                                                                                            "Greek Catholic"="Greek.Catholic",
                                                                                            "Armenian Orthodox"="Armenian.Orthodox",
                                                                                            "Maronite"="Maronite",
                                                                                            "Druze"="Druze")
                                                                 )
                                                ),
                                                conditionalPanel(condition= "input.tabs=='Conflict'",
                                                                 selectInput("coninput", "Select Conflict Type:",
                                                                             choices = list("Violent Events"="viol",
                                                                                            "Arrests"="arrest",
                                                                                            "Dead"="dead",
                                                                                            "Injured"="injury")
                                                                             
                                                                 )      
                                                                 
                                                )
                                  ))),
             tabPanel("Time Series",
                      tabsetPanel(id='tabs2', "Panel 1.x2",
                                  tabPanel("Deaths",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Conflict Related Deaths", style="color:green")),
                                                                      p("This map shows total number of those killed in conflict related events overtime"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can select up to 26 districts"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Charts take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot5", width="900px",height="500px"))
                                           )
                                  ),
                                  tabPanel("Violence",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Violent Conflict Events", style="color:green")),
                                                                      p("This map shows total number of violent conflict related events overtime"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can select up to 26 districts"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Charts take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot6", width="900px",height="500px"))
                                           )
                                  ),
                                  tabPanel("Arrests",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Conflict Arrests", style="color:green")),
                                                                      p("This map shows total number of arrests related to conflict events overtime"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can select up to 26 districts"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Charts take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot7", width="900px",height="500px"))
                                           )
                                  ),
                                  tabPanel("Injuries",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(position="right",
                                                                      h1(span("Conflict Related Injuries", style="color:green")),
                                                                      p("This map shows total number of conflict related injuries overtime"),
                                                                      h3("Features"),
                                                                      tags$ul(tags$li("You can select up to 26 districts"),
                                                                              tags$li("You can drag and position the widget as you please")
                                                                      ),
                                                                      p(h5(strong("NOTE:")), "Charts take time to load."
                                                                      )
                                                         ),
                                                         mainPanel(plotOutput("plot8", width="900px",height="500px"))
                                           )
                                  ),
                                  absolutePanel(top = 435, left = 1055, fixed=FALSE,
                                                draggable = TRUE, 
                                                conditionalPanel(condition= "input.tabs2=='Violence' |input.tabs2=='Deaths' |input.tabs2=='Arrests'|input.tabs2=='Injuries'", 
                                                                 selectInput("disinput", "Select District:",
                                                                             choices = list("Akkar"="Akkar",
                                                                                            "Aley"="Aley",
                                                                                            "Baabda"="Baabda",
                                                                                            "Baalbek"="Baalbek",
                                                                                            "Batroun"="Batroun",
                                                                                            "Bcharré"="Bcharré",
                                                                                            "Beirut"="Beirut",
                                                                                            "Bekaa-West"="BekaaWest",
                                                                                            "Bent Jbayl"="BentJbayl",
                                                                                            "Chouf"="Chouf",
                                                                                            "Hasbaya"="Hasbaya",
                                                                                            "Hermel"="Hermel",
                                                                                            "Jbayl"="Jbayl",
                                                                                            "Jezzine "="Jezzine",
                                                                                            "Kesrouan"="Kesrouan",
                                                                                            "Koura"="Koura",
                                                                                            "Marjayoun"="Marjayoun",
                                                                                            "Matn"="Matn",
                                                                                            "Minie-Danniyeh"="MinieDanniyeh",
                                                                                            "Nabatiyeh"="Nabatiyeh",
                                                                                            "Rachaya"="Rachaya",
                                                                                            "Saida"="Saida",
                                                                                            "Sour"="Sour",
                                                                                            "Tripoli"="Tripoli",
                                                                                            "Zahlé"="Zahlé",
                                                                                            "Zgharta"="Zgharta"))
                                                                 
                                                )
                                  )
                      )
                      ),
             tabPanel("Bar Plots",
                      tabsetPanel(id='tabs3', "Panel 1.x3",
                                  tabPanel("Conflicts",
                                           sidebarLayout(position="right",
                                                         sidebarPanel( h1(span("Conflict Related Deaths", style="color:green")),
                                                                       p("This map shows total number of those killed in conflict related events overtime"),
                                                                       h3("Features"),
                                                                       tags$ul(tags$li("You can select up to 4 conflict types"),
                                                                               tags$li("You can drag and position the widget as you please")
                                                                       ),
                                                                       p(h5(strong("NOTE:")), "Charts take time to load."
                                                                       )
                                                         ),
                                                         mainPanel(plotOutput("plot9", width="900px",height="500px"))
                                           )
                                           
                                  ),
                                  tabPanel("Refugees",
                                           sidebarLayout(position="right",
                                                         sidebarPanel( h1(span("Refugee Flows by District", style="color:green")),
                                                                       p("This map shows total refugee numbers by district."),
                                                                       p(h5(strong("NOTE:")), "Charts take time to load."
                                                                       )
                                                         ),
                                                         mainPanel(plotOutput("plot10", width="900px",height="500px"))
                                           )
                                           
                                  ),
                                  absolutePanel(top = 435, left = 1055, fixed=FALSE,
                                                draggable = TRUE, 
                                                conditionalPanel(condition= "input.tabs3=='Conflicts'",
                                                   selectInput("eventinput", "Select Conflict Type:",
                                                               choices = list("Violent Events"="viol.count",
                                                                              "Arrests"="arrest.sum",
                                                                              "Dead"="dead.sum",
                                                                              "Injured"="injur.sum")
                                                               )
                                                   )
                                  )
                      )
                      ),
                                  
             tabPanel("Data",
                      tabsetPanel(id='tabs3', "Panel 1.x3",
                                  tabPanel("Event Data",
                                           h2('Conflict Event Data'),
                                           dataTableOutput('mytable')
                                  ), 
                                  tabPanel("Refugee Data",
                                           h2('Conflict Event Data'),
                                           dataTableOutput('mytable2')
                                           ),
                                  conditionalPanel('input.dataset === "Event Data"',
                                                                 checkboxGroupInput('show_vars', 'Choose variables to Display',
                                                                                    names(eventdata), names(eventdata)
                                                                                    )
                                                )
                      )
    
                      ),
             tabPanel("About", 
                      h1(strong(span("About Me", style="color:green"))),
                      h5("My name is Cyrus Mohammadian and I am a PhD Candidate studying politics
                   and international relations"),
                      h5("at the University of Southern California. I am currently completing my", span("dissertation", style = "color:red"), 
                         "that asses the impact of"), 
                      h5("Syrian refugee flows on sectarian violence in Lebanon at the subnational level. I rely heavily on classical"), 
                      h5("GIS methods of data collection and visualization as well as more modern spatial econometric tecniques"), 
                      h5("for modeling and analysis. This",a("Shiny", href = "http://www.rstudio.com/shiny"), "web app, powerd by", a("RStudio",href="http://www.rstudio.com"),"displays maps, plots, and data on"), 
                      h5("conflict events, refugee flows, and demography that I have collected since the begining of Lebanon's"),
                      h5("refugee crisis in 2013. For more on this project and other projects of mine please visit my website", span("here", style = "color:red"),".")
             ))
  )


  

