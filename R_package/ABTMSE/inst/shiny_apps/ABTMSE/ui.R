library(shinyBS)
library(shiny)
library(shinyWidgets)

shinyUI(
  fluidPage(

    hr(),

    column(12,


           # Help menu dropdown
           #tags$head(tags$style(HTML("#DD_options { background-color:black; border-color: #FFFFFF; border-width: 3px }"))),
           div(style="display: inline-block;vertical-align:top;float:right;",

               dropdownButton(
                 column(12,
                        h5(tags$b("Glossary",style="color:#347ab6")),
                        column(12,

                          tabsetPanel(

                                 tabPanel(h5("CMPS",style = "color:black"), HTML("<br>"), DT::dataTableOutput('CMPhelp'),value=1),
                                 tabPanel(h5("Performance metrics",style = "color:black"), HTML("<br>"), DT::dataTableOutput('PMhelp'),value=2)

                              )# end of dropdownbutton CMP

                        ),
                        column(12,HTML("<br>")),

                        h5(tags$b("Contact",style="color:#347ab6")),
                        column(12,
                          h5("For technical questions or bug reports please contact ", a("tom@bluematterscience.com", href="mailto:tom@bluematterscience.com", target="_blank"),style = "color:grey")
                        ),
                        h5(tags$b("Acknowledgements",style="color:#347ab6")),
                        column(12,
                          h5("This work was carried out under the provision of the ICCAT Atlantic Wide Research Programme for Bluefin Tuna (GBYP), funded by the European Union, several ICCAT CPCs, the ICCAT Secretariat and by other entities (see: http://www.iccat.int/GBYP/en/Budget.htm). The contents of this paper do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area.")

                        )


                 ),

                 inputId = "DD_help",
                 label = "Help",
                 icon = icon("info"),
                 status = "primary",
                 right=TRUE,
                 circle = FALSE,
                 width="800px"

               )
           ), # end of help menu dropdown




           # Options menu dropdown
           #tags$head(tags$style(HTML("#DD_options { background-color:black; border-color: #FFFFFF; border-width: 3px }"))),
           div(style="display: inline-block;vertical-align:top;float:right;",

               dropdownButton(
                 column(12,
                        h5(tags$b("Global CMP filtering",style="color:#347ab6")),
                        column(12,

                               checkboxGroupInput("CMPs",label=NULL,choices=MPnames, selected=MPnames,inline=T)
                        ),
                        h5(tags$b("Display stochastic or deterministic results",style="color:#347ab6")),
                        column(12,
                               tipify(radioButtons("StochDet",label=NULL,choices=c("Stochastic","Deterministic"),selected="Deterministic"),title="A full set of MSE runs with 48 simulations
                                      per OM including observation error and recruitment deviations or a 1 simulation run with almost perfect observation error and deterministic recruitment")

                        )
                 ),

                 inputId = "DD_options",
                 label = "Options",
                 icon = icon("grip-horizontal"),
                 status = "primary",
                 right=TRUE,
                 circle = FALSE,
                 width="400px"

               )
           ), # end of options menu dropdown


           # File menu dropdown
           #tags$head(tags$style(HTML("#DD_file { background-color:black; border-color: #FFFFFF; border-width: 3px }"))),
           div(style="display: inline-block;vertical-align:top; float:right;",

               dropdownButton(

                 column(12,
                        h5(tags$b("Load a compiled results object",style="color:#347ab6")),
                        column(12,
                               tipify(fileInput("Load",label=NULL),title="Load an object produced by the function Results_compiler() in the ABTMSE package (v6.6.16 or later)"))

                             ),
                 inputId = "DD_file",
                 label = "File",
                 icon = icon("file"),
                 status = "primary",
                 circle = FALSE,
                 right=TRUE,
                 width="400px"
               )

           ) # end of file menu dropdown


    ),  # end of tool bar



    column(12,hr()),

    titlePanel("ABT MSE: Atlantic bluefin tuna Management Strategy Evaluation"),

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
                      how uncertainties affect performance trade-offs and stock projections for multiple candidate management procedures.",style="color:grey")

                      ),
               column(12,br(),hr(),br())
             )
      ),

      column(12,
             fluidRow(
               column(2,
                      br(),
                      h4("OM SET 1",style="text-align: center;"),
                      hr(),
                      #h5("Toggles",style = "color:blue"),
                      actionButton("oRef1",h5("Ref.",style = "color:grey")),
                      actionButton("oRob1",h5("Robust..",style = "color:grey")),
                      actionButton("oRes1",h5("Reset",style = "color:grey")),
                      hr(),

                      selectInput("OM1","Select a single reference OM",choices=OMnames,selected=OMnames[1]),
                      checkboxGroupInput("Rec1", label = h5("Stock-recruitment",style = "font-weight:bold"),
                                  choices = list("(1): West: h=0.6 to h=0.9 1975+, East:  h=0.98 to h=0.98 1988+" = "1",
                                                 "(2): West: B-H h=0.6 all years, East: B-H h=0.7 all years" = "2",
                                                 "(3): West: post 75+ to pre '75, East: 88+ to '50-87 after 10 years" = "3"), selected = "1"),
                      checkboxGroupInput("SM1", label = h5("Age of spawning / natural mortality",style = "font-weight:bold"),
                                  choices = list("(A) Younger spawning, High M" = "A","(B) Older spawning, Low M" = "B"), selected = "A"),
                      checkboxGroupInput("Scale1", label = h5("Scale",style = "font-weight:bold"),
                                         choices = list("(--) mean SSB 15kt West, 200kt East" = "--",
                                                        "(-+) mean SSB 15kt West, 400kt East" = "-+",
                                                        "(+-) mean SSB 50kt West, 200kt East" = "+-",
                                                        "(++) mean SSB 50kt West, 400kt East" = "++"),selected = "--"),
                      checkboxGroupInput("Comp1", label = h5("Length composition weight",style = "font-weight:bold"),
                                         choices = list("(L) Low length composition weight of 1/20" = "L",
                                                        "(H) High length composition weight of 1" = "H"),selected = "L"),

                      checkboxGroupInput("Rob1", label = h5("Robustness",style = "font-weight:bold"),
                                  choiceNames = ROMcode, choiceValues=ROMnames, selected = ""),


                      hr(),
                      h5("Operating models included",style = "font-weight:bold"),
                      textOutput("test1"),
                      hr(),
                      h5("OM codes",style = "font-weight:bold"),
                      textOutput("test12")

                     ),

               column(width = 8,
                      tabsetPanel(
                        tabPanel(h5("P.Tab.1",style = "color:black"),

                                 column(12,HTML("<br>")),
                                 column(12,h5("Performance metrics",style="font-weight:bold")),
                                 column(2,  selectInput("PM1","",choices=pnames,selected="AvC30")),
                                 column(2,  selectInput("PM2","",choices=pnames,selected="D30")),
                                 column(2,  selectInput("PM3","",choices=pnames,selected="Br30")),
                                 column(2,  selectInput("PM4","",choices=pnames,selected="AAVC")),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("East Area",stype="font-weight:bold"))),
                                 column(width=6,DT::dataTableOutput('table1E')),
                                 column(width=6,DT::dataTableOutput('table2E')),
                                 column(12,h4(" ")),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("West Area",stype="font-weight:bold"))),
                                 column(DT::dataTableOutput('table1W'),width = 6),
                                 column(DT::dataTableOutput('table2W'),width = 6),
                                 column(1,h5("Table 1.",style="color:black")),
                                 column(11, h5("Mean of the selected performance metrics over all simulations and selected OMs",style="color:darkgrey")),

                                 value=1),
                        tabPanel(h5("P.Tab.2",style = "color:black"),

                                 column(12,HTML("<br>")),
                                # column(12,h5("Performance metric",style="font-weight:bold")),
                                 column(12,
                                       column(4,selectInput("PM2_1","Performance metric",choices=pnames,selected="Br30")),
                                       column(8,sliderInput("PT2_IQR","Inter-quantile range",min=50,max=99,value=90,step=1))
                                 ),
                                 #column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("East",stype="font-weight:bold"))),
                                 column(width=6,DT::dataTableOutput('table2_1E')),
                                 column(width=6,DT::dataTableOutput('table2_2E')),
                                 column(12,h4(" ")),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(5),column(7,h4("West",stype="font-weight:bold"))),
                                 column(DT::dataTableOutput('table2_1W'),width = 6),
                                 column(DT::dataTableOutput('table2_2W'),width = 6),
                                 column(1,h5("Table 2.",style="color:black")),
                                 column(11, h5("Median and interquantile range of the selected performance metrics over all simulations and selected OMs",style="color:darkgrey")),


                                 value=2),
                        tabPanel(h5("Zeh",style = "color:black"),
                                 column(12,HTML("<br>")),
                                 column(2,  selectInput("Zeh_PM","Performance metric",choices=pnames,selected="AvC30")),
                                 column(9),
                                 column(1,checkboxInput('zeh_violin',"Violin plot",FALSE)),
                                 column(12,HTML("<br>")),


                                 fluidRow(column(5),column(7,h4("East Area",stype="font-weight:bold"))),
                                 column(width=6,plotOutput("Zeh1E",height="400px")),
                                 column(width=6,plotOutput("Zeh2E",height="400px")),

                                 fluidRow(column(5),column(7,h4("West Area",stype="font-weight:bold"))),
                                 column(width=6,plotOutput("Zeh1W",height="400px")),
                                 column(width=6,plotOutput("Zeh2W",height="400px")),
                                 column(1,h5("Figure 1.",style="color:black")),
                                 column(11, h5("Zeh plot showing the median, interquartile and 90% interquantile range for a selected performance metric integrated over all simulations and selected OMs",style="color:darkgrey")),


                                 value=3),
                        tabPanel(h5("Zeh OMSet1",style = "color:black"),
                                 column(12,HTML("<br>")),
                                 column(3,  selectInput("Zeh_MP1","CMP1",choices=MPnames,selected=MPnames[2])),
                                 column(3,  selectInput("Zeh_MP2",label="CMP2",choices=c("None",MPnames),selected="None")),
                                 column(2,  selectInput("Zeh_PM1","Performance Metric",choices=pnames,selected="Br30")),
                                 column(2,  selectInput("Zeh_FacCol","Factor Color Coding",choices=c("None" = 0, "Stock-recruitment"=1, "Spawning-Mortality"=2,
                                                                                                      "Scale" = 3, "Length-Comp Wt"=4),selected="None")),
                                 column(12,HTML("<br>")),
                                 column(5),column(7,h4("East Area",stype="font-weight:bold")),
                                 column(12,plotOutput("ZehE",width="auto")),
                                 column(5),column(7,h4("West Area",stype="font-weight:bold")),
                                 column(12,plotOutput("ZehW",width="auto")),
                                 column(1,h5("Figure 2.",style="color:black")),
                                 column(11, h5("Zeh plot showing results for OM set 1 operating models individually. Plotted are the median, interquartile and 90% interquantile range for a selected performance metric over all simulations",style="color:darkgrey")),


                                 value=4),

                        tabPanel(h5("Zeh multi",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(11,
                                   column(2,  selectInput("ZehPM_PM1","Perf. Metric 1",choices=pnames,selected="Br30")),
                                   column(2,  selectInput("ZehPM_PM2","Perf. Metric 2",choices=pnames,selected="AvC30")),
                                   column(2,  selectInput("ZehPM_PM3","Perf. Metric 3",choices=pnames,selected="AAVC")),
                                   column(2,  selectInput("ZehPM_MP1","CMP 1",choices=MPnames,selected=MPnames[1])),
                                   column(2,  selectInput("ZehPM_MP2","CMP 2",choices=MPnames,selected=MPnames[2])),
                                   column(2,  selectInput("ZehPM_MP3","CMP 3",choices=MPnames,selected=MPnames[3]))
                                 ),

                                 column(1,checkboxInput('zehPM_violin',"Violin plot",FALSE)),
                                 column(12,HTML("<br>")),
                                 column(5),column(7,h4("East Area",stype="font-weight:bold")),
                                 column(6,plotOutput("ZehPM1E",width="auto")),
                                 column(6,plotOutput("ZehPM2E",width="auto")),

                                 column(5),column(7,h4("West Area",stype="font-weight:bold")),
                                 column(6,plotOutput("ZehPM1W",width="auto")),
                                 column(6,plotOutput("ZehPM2W",width="auto")),
                                 column(1,h5("Figure 3.",style="color:black")),
                                 column(11, h5("As Figure 1 but showing multiple performance metrics simultaneously. Plotted are the median, interquartile and 90% interquantile range for three performance metrics over all simulations",style="color:darkgrey")),


                                 value=5),

                        tabPanel(h5("T-O",style = "color:black"),

                                 column(12,HTML("<br>")),
                                 fluidRow(column(3,  selectInput("T_PMx","Performance Metric (x axis)",choices=pnames,selected="Br30")),
                                 column(3,  selectInput("T_PMy","Performance Metric (y axis)",choices=pnames,selected="AvC30")),
                                 column(2,  checkboxInput("bars","Add 90th bars",value=FALSE)),
                                 column(2,  checkboxInput("labs","Labels on plots",value=FALSE))
                                 ),

                                 column(5),
                                 column(7,h4("Eastern Area",stype="font-weight:bold")),
                                 column(5,plotOutput("Tplot11")),
                                 column(2,plotOutput("Tleg")),
                                 column(5,plotOutput("Tplot12")),


                                 column(5),
                                 column(7,h4("Western Area",stype="font-weight:bold")),
                                 column(5,plotOutput("Tplot21")),
                                 column(2,plotOutput("Tleg2")),
                                 column(5,plotOutput("Tplot22")),
                                 column(1,h5("Figure 4.",style="color:black")),
                                 column(11, h5("A trade-off plot showing mean performance over all selected OMs and simulations.",style="color:darkgrey")),


                                 value=6),

                        tabPanel(h5("Proj.",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(12,column(6, sliderInput("YB_quant","Quantile",min=5,max=95,value=50,step=5))),

                                 column(5),column(7,h4("Eastern Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("YBP1E",width="auto")),
                                 column(6,plotOutput("YBP2E",width="auto")),

                                 column(5),column(7,h4("Western Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("YBP1W",width="auto")),
                                 column(6,plotOutput("YBP2W",width="auto")),
                                 column(1,h5("Figure 5.",style="color:black")),
                                 column(11, h5("Mean Catch (by area) and SSB (by stock) projections averaged over all selected OMs and simulations.",style="color:darkgrey")),


                                 value=7),

                        tabPanel(h5("Stoch. Proj.",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(12,column(6, sliderInput("StochIQR","Inter-quantile range",min=50,max=99,value=90,step=1))),

                                 column(3,  selectInput("S_MP1","CMP 1",choices=MPnames,selected=MPnames[1])),
                                 column(3,  selectInput("S_MP2","CMP 2",choices=MPnames,selected=MPnames[2])),
                                 column(3,  selectInput("S_MP3","CMP 3",choices=MPnames,selected=MPnames[3])),

                                 column(12,HTML("<br>")),

                                 column(5),column(7,h4("Eastern Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("S1E",width="auto")),
                                 column(6,plotOutput("S2E",width="auto")),

                                 column(5),column(7,h4("Western Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("S1W",width="auto")),
                                 column(6,plotOutput("S2W",width="auto")),
                                 column(1,h5("Figure 6.",style="color:black")),
                                 column(11, h5("As figure 5 but showing uncertainty in Catch (by area) and SSB (by stock) outcomes for up to three MPs. The projected values of catch (by area) and SSB (by stock) are the mean values across all simulations and selected OMs.",style="color:darkgrey")),

                                 value=8),

                        tabPanel(h5("By Sim Proj.",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(3, selectInput("Sm_MP1","CMP 1",choices=MPnames,selected=MPnames[1])),
                                 column(3, selectInput("Sm_MP2","CMP 2",choices=MPnames,selected=MPnames[2])),

                                 column(4, sliderInput("nsim","Number of simulations to plot",min=2,max=10,value=3,step=1)),
                                 column(2, actionButton("resamp","Resample")),


                                 column(12,HTML("<br>")),

                                 column(5),column(7,h4("Eastern Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("Sm1E",width="auto")),
                                 column(6,plotOutput("Sm2E",width="auto")),

                                 column(5),column(7,h4("Western Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("Sm1W",width="auto")),
                                 column(6,plotOutput("Sm2W",width="auto")),
                                 column(1,h5("Figure 7.",style="color:black")),
                                 column(11, h5("As figure 6 but showing by-simulation results for a single OM for up to two MPs. The projected values of catch (by area) and SSB (by stock) are the mean values across all simulations and selected OMs.",style="color:darkgrey")),

                                 value=9),

                        tabPanel(h5("Radar",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(4, selectInput("R_MP1","CMP 1",choices=MPnames,selected=MPnames[1])),
                                 column(4, selectInput("R_MP2","CMP 2",choices=MPnames,selected=MPnames[2])),
                                 column(4, selectInput("R_MP3","CMP 3",choices=MPnames,selected=MPnames[3])),
                                 column(12,checkboxGroupInput("R_PMs","Performance metrics",choices=pnames,selected=c("AvC30","AAVC","Br30"),inline=T)),

                                 column(12,HTML("<br>")),

                                 column(5),column(7,h4("Eastern Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("R1E",width="auto")),
                                 column(6,plotOutput("R2E",width="auto")),

                                 column(5),column(7,h4("Western Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("R1W",width="auto")),
                                 column(6,plotOutput("R2W",width="auto")),

                                 column(1,h5("Figure 8.",style="color:black")),
                                 column(11, h5("Radar plots show the mean of the performance metrics (over all simulations and selected OMs).
                                               Radar plots are intended to distinguish good and bad performing CMPs according to the shaded area. It follows
                                               that some metrics are either inverted (denoted with an i) or are a reciprocal (r) such that larger plotted
                                               areas reflect better performance.",style="color:darkgrey")),

                                 #plotOutput("Wormplot",height="650px"),
                                 value=10),

                        tabPanel(h5("Radar E/W",style = "color:black"),
                                 column(12,HTML("<br>")),

                                 column(4, selectInput("REW_MP1","CMP 1",choices=MPnames,selected=MPnames[1])),
                                 column(4, selectInput("REW_MP2","CMP 2",choices=MPnames,selected=MPnames[2])),
                                 column(4, selectInput("REW_MP3","CMP 3",choices=MPnames,selected=MPnames[3])),
                                 column(12,checkboxGroupInput("RE_PMs","East Area / East Stock Performance metrics",choices=pnames,selected=c("AvC30","AAVC","Br30"),inline=T)),
                                 column(12,checkboxGroupInput("RW_PMs","West Area / West Stock Performance metrics",choices=pnames,selected=c("AvC30","AAVC","Br30"),inline=T)),

                                 column(12,HTML("<br>")),

                                 column(5),column(7,h4("Eastern Stock",stype="font-weight:bold")),
                                 column(6,plotOutput("REW1",width="auto")),
                                 column(6,plotOutput("REW2",width="auto")),

                                 column(1,h5("Figure 9.",style="color:black")),
                                 column(11, h5("As Figure 8 but allowing for cross-stock performance evaluation.  Plots show the mean of the performance metrics (over all simulations and selected OMs).
                                               Radar plots are intended to distinguish good and bad performing CMPs according to the shaded area. It follows
                                               that some metrics are either inverted (denoted with an i) or are a reciprocal (r) such that larger plotted
                                               areas reflect better performance.",style="color:darkgrey")),

                                 #plotOutput("Wormplot",height="650px"),
                                 value=10),


                        id = "tabs1"
                      )),


               column(width = 2,

                      br(),
                      h4("OM SET 2",style="text-align: center;"),
                      hr(),

                      #h5("Load defaults",style = "color:blue"),

                      actionButton("oRef2",h5("Ref.",style = "color:grey")),
                      actionButton("oRob2",h5("Robust..",style = "color:grey")),
                      actionButton("oRes2",h5("Reset",style = "color:grey")),
                      hr(),

                      selectInput("OM2","Select a single reference OM",choices=OMnames,selected=OMnames[1]),
                      checkboxGroupInput("Rec2", label = h5("Stock-recruitment",style = "font-weight:bold"),
                                         choices = list("(1): West: h=0.6 to h=0.9 1975+, East:  h=0.98 to h=0.98 1988+" = "1",
                                                        "(2): West: B-H h=0.6 all years, East: B-H h=0.7 all years" = "2",
                                                        "(3): West: post 75+ to pre '75, East: 88+ to '50-87 after 10 years" = "3"), selected = "1"),
                      checkboxGroupInput("SM2", label = h5("Age of spawning / natural mortality",style = "font-weight:bold"),
                                         choices = list("(A) Younger spawning, High M" = "A",
                                                        "(B) Older spawning, Low M" = "B"), selected = "A"),
                      checkboxGroupInput("Scale2", label = h5("Scale",style = "font-weight:bold"),
                                         choices = list("(--) mean SSB 15kt West, 200kt East" = "--",
                                                        "(-+) mean SSB 15kt West, 400kt East" = "-+",
                                                        "(+-) mean SSB 50kt West, 200kt East" = "+-",
                                                        "(++) mean SSB 50kt West, 400kt East" = "++"),selected = "--"),
                      checkboxGroupInput("Comp2", label = h5("Length composition weight",style = "font-weight:bold"),
                                         choices = list("(L) Low length composition weight of 1/20" = "L",
                                                        "(H) High length composition weight of 1" = "H"),selected = "L"),

                      checkboxGroupInput("Rob2", label = h5("Robustness",style = "font-weight:bold"),
                                         choiceNames = ROMcode, choiceValues=ROMnames, selected = ""),



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
             )

        ) # fluid row

      ), # column
      column(8,style="height:40px"),
      column(2,style="height:40px; padding:9px",textOutput("SessionID")),
      column(2,style="height:40px", h6("copyright (c) ICCAT 2020"))
    ) # fluid row
  )# Fluidpage
)

