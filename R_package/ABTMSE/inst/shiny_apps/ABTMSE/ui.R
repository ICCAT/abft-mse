

shinyUI(
  fluidPage(
    hr(),
    titlePanel("Atlantic bluefin tuna Management Strategy Evaluation (ABT MSE)"),

    fluidRow(
      column(12,
             fluidRow(
               column(12,br()),
               column(2,
                      img(src = "ICCAT3.jpg", height = 70, width = 110),
                      img(src = "bft2.jpg", height = 70, width = 110)),
               column(10,h5("The Atlantic-Wide Research Programme on Bluefin Tuna (GBYP) is investigating MSE for
                      providing robust advice consistent with the precautionary approach. MSE aims to reveal
                      management procedures that are robust to uncertainties in data collection, population and
                      fishing dynamics. In MSE these uncertainties are represented by alternative operating models (OMs).",style="color:grey"),
                      h5(" "),
                      h5("    In this PRELIMINARY interactive demo, the user can choose different sets of operating models to investigate
                      how uncertainties affect performance trade-offs and stock projections.",style="color:grey")

                      ),
               column(12,br(),hr(),br())
             )
      ),


      column(12,
             fluidRow(
               column(2,
                      br(),
                      h4("OM SET 1"),
                      hr(),
                      br(),
                      selectInput("OM1","Select a single OM",choices=OMnames,selected=OMnames[1]),
                      checkboxGroupInput("Rec1", label = h5("Stock-recruitment",style = "font-weight:bold"),
                                  choices = list("(1) West: h=0.6 to HS 1975, East: h=0.98 1988" = "1",
                                                 "(2) West: h=0.6, East: h=0.98 to h=0.7 1988" = "2",
                                                 "(3) West: to pre 1975 BH, East: to pre 1988 BH" = "3"), selected = "1"),
                      checkboxGroupInput("SSB1", label = h5("Spawning biomass",style = "font-weight:bold"),
                                  choices = list("(A) Best estimates" = "A", "(B) SSB matches VPA" = "B"), selected = "A"),
                      checkboxGroupInput("SM1", label = h5("Age of spawning / natural mortality",style = "font-weight:bold"),
                                  choices = list("(I) Younger / high M" = "I", "(II) Younger / low M" = "II",
                                                 "(III) Older / High M" = "III", "(IV) Older / low M"= "IV"),selected = "I"),
                      checkboxGroupInput("Rob1", label = h5("Robustness sets",style = "font-weight:bold"),
                                  choices = list("20% overcatch" = "R1", "1% catchability increase" = "R2",
                                                 "Non linear indices" = "R3", "Mixing is halved"="R4"), selected = c(1,2,3,4)),

                      hr(),
                      h5("Load defaults",style = "color:blue"),

                      actionButton("oRef1",h5("Ref.",style = "color:grey")),
                      actionButton("oRob1",h5("Robust..",style = "color:grey")),
                      actionButton("oRes1",h5("Reset",style = "color:grey")),

                      hr(),
                      h5("Operating models included",style = "font-weight:bold"),
                      textOutput("test1"),
                      h5("OM codes",style = "font-weight:bold"),
                      textOutput("test12")

                     ),


               column(width = 8,
                      tabsetPanel(
                        tabPanel(h5("Performance table",style = "color:black"),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("Eastern Area",stype="font-weight:bold"))),
                                 column(width=6,DT::dataTableOutput('table1E')),
                                 column(width=6,DT::dataTableOutput('table2E')),
                                 column(12,h4(" ")),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("Western Area",stype="font-weight:bold"))),
                                 column(DT::dataTableOutput('table1W'),width = 6),
                                 column(DT::dataTableOutput('table2W'),width = 6),

                                 column(12,HTML("<br><br>")),

                                 column(12,h5("Performance metrics",style="font-weight:bold")),
                                 column(2,  selectInput("PM1","",choices=pnames,selected="AvC30")),
                                 column(2,  selectInput("PM2","",choices=pnames,selected="D30")),
                                 column(2,  selectInput("PM3","",choices=pnames,selected="Br30")),
                                 column(2,  selectInput("PM4","",choices=pnames,selected="AAVC")),

                                 value=1),
                        tabPanel(h5("Zeh plots",style = "color:black"),
                                 column(12,HTML("<br>")),
                                 column(2,  selectInput("Zeh_PM","Performance metric",choices=pnames,selected="AvC30")),
                                 column(12,HTML("<br>")),

                                 fluidRow(column(5),column(7,h4("Eastern Area",stype="font-weight:bold"))),
                                 column(width=6,plotOutput("Zeh1E",height="400px")),
                                 column(width=6,plotOutput("Zeh2E",height="400px")),
                                 column(12,HTML("<br>")),

                                 fluidRow(column(5),column(7,h4("Western Area",stype="font-weight:bold"))),
                                 column(width=6,plotOutput("Zeh1W",height="400px")),
                                 column(width=6,plotOutput("Zeh2W",height="400px")),



                                 value=2),
                        tabPanel(h5("Zeh OM Set 1",style = "color:black"),
                                 column(12,HTML("<br>")),
                                 column(3,  selectInput("Zeh_MP1","Management Procedure",choices=MPnames,selected=MPnames[1])),
                                 column(2,  selectInput("Zeh_PM1","Performance Metric",choices=pnames,selected="Br30")),
                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("Eastern Area",stype="font-weight:bold"))),
                                 fluidRow(column(1),column(8,plotOutput("ZehE",height="400px",width="800px"))),
                                 fluidRow(column(5),column(7,h4("Western Area",stype="font-weight:bold"))),
                                 fluidRow(column(1),column(8,plotOutput("ZehW",height="400px",width="800px"))),


                                 #plotOutput("Wormplot",height="650px"),
                                 value=3),
                        tabPanel(h5("Worm plots",style = "color:black"),
                                 h5("< under construction >",style="color:grey"),
                                 #plotOutput("Wormplot",height="650px"),
                                 value=4),

                        tabPanel(h5("Trade-off plots",style = "color:black"),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(3,  selectInput("T_PMx","Performance Metric (x axis)",choices=pnames,selected="Br30")),
                                 column(3,  selectInput("T_PMy","Performance Metric (y axis)",choices=pnames,selected="AvC30")),
                                 column(2,  checkboxInput("bars","Add 95th bars",value=FALSE)),
                                 column(2,  checkboxInput("labs","Include labels",value=FALSE))
                                 ),

                                 fluidRow(column(5),column(7,h4("Eastern Area",stype="font-weight:bold"))),
                                 fluidRow(
                                 column(5,plotOutput("Tplot11",height="300px",width="400px")),
                                 column(2,plotOutput("Tleg",height="300px")),
                                 column(5,plotOutput("Tplot12",height="300px",width="400px"))),
                                 column(12),

                                 fluidRow(column(12),
                                 column(12,  HTML("<br>"))
                                 ),

                                 fluidRow(column(5),column(7,h4("Western Area",stype="font-weight:bold"))),
                                 column(width=5,plotOutput("Tplot21",height="300px")),
                                 column(2,plotOutput("Tleg2",height="300px")),
                                 column(width=5,plotOutput("Tplot22",height="300px")),

                                 value=5),

                        tabPanel(h5("Satisficing",style = "color:black"),
                                 h5("< under construction >",style="color:grey"),
                                 #plotOutput("Wormplot",height="650px"),
                                 value=6),

                        tabPanel(h5("Help",style = "color:orange"),
                                 h5("For technical questions or bug reports please contact ", a("t.carruthers@oceans.ubc.ca", href="mailto:t.carruthers@ubc.ca", target="_blank"),style = "color:grey"),

                                 value=7),

                        id = "tabs1"
                      )),


               column(width = 2,

                      br(),
                      h4("OM SET 2"),
                      hr(),
                      br(),
                      selectInput("OM2","Select a single OM",choices=OMnames,selected=OMnames[1]),
                      checkboxGroupInput("Rec2", label = h5("Stock-recruitment",style = "font-weight:bold"),
                                         list("(1) West: h=0.6 to HS 1975, East: h=0.98 1988" = "1",
                                              "(2) West: h=0.6, East: h=0.98 to h=0.7 1988" = "2",
                                              "(3) West: to pre 1975 BH, East: to pre 1988 BH" = "3"), selected = "1"),
                      checkboxGroupInput("SSB2", label = h5("Spawning biomass",style = "font-weight:bold"),
                                         choices = list("(A) Best estimates" = "A", "(B) SSB matches VPA" = "B"),
                                         selected = "A"),
                      checkboxGroupInput("SM2", label = h5("Age of spawning / natural mortality",style = "font-weight:bold"),
                                         choices = list("(I) Younger / high M" = "I", "(II) Younger / low M" = "II",
                                                        "(III) Older / High M" = "III", "(IV) Older / low M"= "IV"),selected = "I"),
                      checkboxGroupInput("Rob2", label = h5("Robustness sets",style = "font-weight:bold"),
                                         choices = list("20% overcatch" = "R1", "1% catchability increase" = "R2",
                                                        "Non linear indices" = "R3", "Mixing is halved"="R4"), selected = c(1,2,3,4)),

                   hr(),
                   h5("Load defaults",style = "color:blue"),

                   actionButton("oRef2",h5("Ref.",style = "color:grey")),
                   actionButton("oRob2",h5("Robust..",style = "color:grey")),
                   actionButton("oRes2",h5("Reset",style = "color:grey")),

                   hr(),
                   h5("Operating models included",style = "font-weight:bold"),
                   textOutput("test2"),
                   hr(),
                   h5("OM codes",style = "font-weight:bold"),
                   textOutput("test22")
             ),

             column(12,
               HTML("<br>"),
               hr(),
               HTML("<br>")
             ),

             column(12,
              HTML("<br>"),
              hr(),
              HTML("<br>")
             )

        ) # fluid row

      ), # column
      column(8,style="height:40px"),
      column(2,style="height:40px; padding:9px",textOutput("SessionID")),
      column(2,style="height:40px", h6("copyright (c) ICCAT 2018"))
    ) # fluid row
  )# Fluidpage
)

