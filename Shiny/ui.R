

shinyUI(
  fluidPage(
    
    titlePanel("Atlantic bluefin tuna Management Strategy Evaluation (MSE)"),
    
    fluidRow(
      column(12,
             fluidRow(
               column(2,
                      img(src = "ICCAT.jpg", height = 110, width = 160)),
               column(10,h5("The Atlantic-Wide Research Programme on Bluefin Tuna (GBYP) is investigating MSE for 
                      providing robust advice consistent with the precautionary approach. MSE aims to reveal
                      management procedures that are robust to uncertainties in data collection, population and 
                      fishing dynamics. In MSE these uncertainties are represented by alternative operating models (OMs).",style="color:grey"),
                      h5(" "),
                      h5("    In this interactive demo, the user can choose different sets of operating models to investigate 
                      how uncertainties affect performance trade-offs and stock projections.",style="color:grey"),
                      h5(" "),
                      h5("! This preliminary App provides results the Eastern Atlantic stock ONLY and catches are in arbitrary units !",style="color:grey")
                      )
             )   
      ),   
             
             
      column(12,
             fluidRow(
               column(2,
                      h4("OM set A",style = "color:blue"),
                       
                      selectInput("SR1", label = h5("Stock-recruitment",style = "color:blue"), 
                                  choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Mov1", label = h5("Movement model",style = "color:blue"), 
                                  choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Up1", label = h5("Update interval",style = "color:blue"), 
                                  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Rec1", label = h5("Rec. strength",style = "color:blue"), 
                                  choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("CBias1", label = h5("Future catch bias",style = "color:blue"), 
                                  choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("MMat1", label = h5("Mortality-maturity",style = "color:blue"), 
                                  choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                 "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                 "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Dep1", label = h5("Depletion level",style = "color:blue"), 
                                  choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      hr(),
                      h5("Load defaults",style = "color:blue"),
                      
                      tags$head(
                        tags$style(HTML('#Rob1{background-color:#C0C0C0}'))
                      ),
                      tags$head(
                        tags$style(HTML('#Ref1{background-color:#C0C0C0}'))
                      ),
                      tags$head(
                        tags$style(HTML('#Res1{background-color:#C0C0C0}'))
                      ),
                      
                      actionButton("Ref1",h5("Ref.",style = "color:white")),
                      actionButton("Rob1",h5("Robust..",style = "color:white")),
                      actionButton("Res1",h5("Reset",style = "color:white"))
                      
                     ),
               
               column(width = 2,
                      h4("OM set B",style = "color:red"),
                      selectInput("SR2", label = h5("Stock-recruitment",style = "color:red"), 
                                  choices = list("Bev-Holt h=0.7" = "BH7", "Bev-Holt h=0.98" = "BH98",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Mov2", label = h5("Movement model",style = "color:red"), 
                                  choices = list("Diffuse fractions" = "Frac", "Gravity model" = "Grav",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Up2", label = h5("Update interval",style = "color:red"), 
                                  choices = list("Every 3 years" = "Up3", "Every 4 years" = "Up4",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Rec2", label = h5("Rec. strength",style = "color:red"), 
                                  choices = list("Stable recruitment" = "StableR", "Low Recruitment" = "LowR",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("CBias2", label = h5("Future catch bias",style = "color:red"), 
                                  choices = list("No bias" = "NoCbias", "20% positive bias" = "SPCbias",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("MMat2", label = h5("Mortality-maturity",style = "color:red"), 
                                  choices = list("(E) M@age L.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mage-AMlow",
                                                 "(E) Mconst H.mat (W) Mconst H.mat" = "Mconst-AMhigh-Mconst-AMhigh",
                                                 "(E) M@age L.mat (W) Mage L.mat"="Mage-AMlow-Mage-AMlow",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      selectInput("Dep2", label = h5("Depletion level",style = "color:red"), 
                                  choices = list("Estimated depletion" ="Dep_30_40", "Half estimated depletion" = "Dep_15_20",
                                                 "ALL" = "ALL"), selected = "ALL"),
                      hr(),
                      h5("Load defaults",style = "color:red"),
                      
                      tags$head(
                        tags$style(HTML('#Rob2{background-color:#C0C0C0}'))
                      ),
                      tags$head(
                        tags$style(HTML('#Ref2{background-color:#C0C0C0}'))
                      ),
                      tags$head(
                        tags$style(HTML('#Res2{background-color:#C0C0C0}'))
                      ),
                      
                      actionButton("Ref2",h5("Ref.",style = "color:white")),
                      actionButton("Rob2",h5("Robust.",style = "color:white")),
                      actionButton("Res2",h5("Reset",style = "color:white"))
                     
                      ),
               
               column(width = 6,
                      tabsetPanel(      
                        tabPanel(h5("Trade-offs",style = "color:black"),
                                 h5("Trade-off plots illustrate compromises in performance metrics among management procedures (MPs).",style="color:grey"),
                                 plotOutput("Tplot",height="550px"),value=1),
                        tabPanel(h5("Worm plots",style = "color:black"),
                                 h5("The worm plots show biomass relative to BMSY for individual simulations.",style="color:grey"),
                                 plotOutput("Proplot",height="650px"),value=2),
                        tabPanel(h5("Pencil plots",style = "color:black"), 
                                 h5("The thickness of the pencil indicates the fraction of simulations for which biomass exceeded the reference level.",style="color:grey"),
                                 plotOutput("Wormplot",height="650px"),value=3),
                        
                        tabPanel(h5("Help",style = "color:orange",value=4), 
                                 h4("Glossary"),
                                 h5("MSE",style="color:orange"),
                                 h5("Management Strategy Evaluation: a closed-loop simulation for quantifying MP performance and robustness",style="color:grey"),
                                 h5("MP",style="color:orange"),
                                 h5("Management Procedure: everything that takes you from data to a management recommendation (e.g. assessment + harvest control rule)",style="color:grey"),
                                 h5("OM",style="color:orange"),
                                 h5("Operating Model: a computer simulation that represents a plausible hypothesis regarding population and fishery dynamics",style="color:grey"),
                                 h5("SPslope",style="color:orange"),
                                 h5("Originally proposed by Mark Maunder, SPslope aims to find a productive stock size by
                                    aiming for a slope of zero in surplus production relative to stock biomass",style="color:grey"),
                                 h5("DD",style="color:orange"),
                                 h5("A two parameter delay difference model (no process error) parameterized according to UMSY and MSY by Carl Walters",style="color:grey"),
                                 h5("DD_4010",style="color:orange"),
                                 h5("The DD MP with a 40-10 harvest control rule superimposed: F throttling below 40% unfished biomass, zero F at 10% unfished biomass",style="color:grey"),
                                 h5("SBT2",style="color:orange"),
                                 h5("Management procedure similar to that applied for Southern Bluefin Tuna",style="color:grey"),
                                 h5("Fadapt",style="color:orange"),
                                 h5("Adaptive fishign rate to find productive stock size, similar to SPslope but bounded",style="color:grey"),
                                 h5("Islope1",style="color:orange"),
                                 h5("Geromont and Butterworth index slope method, locates a catch at stable CPUE",style="color:grey"),
                                 h5("LstepCC4",style="color:orange"),
                                 h5("Geromont and Butterworth length stepping MP that uses length data to modify the TAC",style="color:grey"),
                                 h5("UMSY",style="color:orange"),
                                 h5("Fishes at observed current abundance x a guess at harvest rate at MSY",style="color:grey"),
                                 h5("UMSY_PI",style="color:orange"),
                                 h5("Fishes at perfectly known current biomass x harvest rate at MSY",style="color:grey"),
                                 h5("Cooke_DD",style="color:orange"),
                                 h5("Justin Cooke's HCR coupled with a delay-difference model",style="color:grey"),
                                 hr(),
                                 h5("For more info / help with this tool email t.carruthers@oceans.ubc.ca")
                                 ),
                        id = "tabs1"
                      )),
                  
               
               column(width = 2,
                      conditionalPanel(condition="input.tabs1==1",
                      h1(" "),
                     
                      h4("Perf. metrics",style = "color:black"),
                      hr(),
                      h5("Plot 1",style = "color:black"),
                      selectInput("P1", label = h5("x-axis",style="color:black"), 
                                  choices = list("Mean yield" = "Y", "AAVY" = "AAVY",
                                                 "P. overfishing" = "F_FMSY","P. overfished"="B_BMSY","Depletion"="Dep"), selected = "F_FMSY"),
                      selectInput("P2", label = h5("y-axis",style="color:black"), 
                                  choices = list("Mean yield" = "Y", "AAVY" = "AAVY",
                                                 "P. overfishing" = "F_FMSY","P. overfished"="B_BMSY","Depletion"="Dep"), selected = "Y"),
                      hr(),
                      h5("Plot 2",style = "color:black"),
                      selectInput("P3", label = h5("x-axis",style="color:black"), 
                                  choices = list("Mean yield" = "Y", "AAVY" = "AAVY",
                                                 "P. overfishing" = "F_FMSY","P. overfished"="B_BMSY","Depletion"="Dep"), selected = "B_BMSY"),
                      selectInput("P4", label = h5("y-axis",style="color:black"), 
                                  choices = list("Mean yield" = "Y", "AAVY." = "AAVY",
                                                 "P. overfishing" = "F_FMSY","P. overfished"="B_BMSY","Depletion"="Dep"), selected = "AAVY")
                      ),
                      conditionalPanel(condition="input.tabs1==2",
                      #"SPslope"  "DD"       "DD4010"   "SBT2"     "Fadapt"   "Islope1"  "LstepCC4" "UMSY"     "UMSY_PI" 
                      h1(" "),
                      h4("MPs",style = "color:black"),
                      hr(),
                      selectInput("MP1", label = h5("MP 1",style="color:black"), 
                                  choices = list("SPslope" = "SPslope", "DD" = "DD","DD4010"="DD4010","SBT2"="SBT2","Fadapt"="Fadapt","Islope"="Islope",
                                                 "LstepCC4"="LstepCC4","UMSY"="UMSY","UMSY_PI"="UMSY_PI","Cooke_DD"="Cooke_DD"), selected = "Fadapt"),
                      
                      selectInput("MP2", label = h5("MP 2",style="color:black"), 
                                  choices = list("SPslope" = "SPslope", "DD" = "DD","DD4010"="DD4010","SBT2"="SBT2","Fadapt"="Fadapt","Islope"="Islope",
                                                 "LstepCC4"="LstepCC4","UMSY"="UMSY","UMSY_PI"="UMSY_PI","Cooke_DD"="Cooke_DD"), selected = "DD4010"),
                      
                      selectInput("MP3", label = h5("MP 3",style="color:black"), 
                                  choices = list("SPslope" = "SPslope", "DD" = "DD","DD4010"="DD4010","SBT2"="SBT2","Fadapt"="Fadapt","Islope"="Islope",
                                                 "LstepCC4"="LstepCC4","UMSY"="UMSY","UMSY_PI"="UMSY_PI","Cooke_DD"="Cooke_DD"), selected = "SBT2"),
                      
                      selectInput("MP4", label = h5("MP 4",style="color:black"), 
                                  choices = list("SPslope" = "SPslope", "DD" = "DD","DD4010"="DD4010","SBT2"="SBT2","Fadapt"="Fadapt","Islope"="Islope",
                                                 "LstepCC4"="LstepCC4","UMSY"="UMSY","UMSY_PI"="UMSY_PI","Cooke_DD"="Cooke_DD"), selected = "UMSY_PI")
                      
                      ),
                      conditionalPanel(condition="input.tabs1==3",
                      #"SPslope"  "DD"       "DD4010"   "SBT2"     "Fadapt"   "Islope1"  "LstepCC4" "UMSY"     "UMSY_PI" 
                                       
                                       
                      h1(" "),
                      h4("Target biomass",style = "color:black"),
                      sliderInput("Bref", label = h5("Relative to BMSY:",style="color:black"), 
                                  min = 0, max = 1.5, value = 0.5, step= 0.01),
                      hr(),
                      h4("Threshold probabilities",style = "color:black"),
                      sliderInput("LB", label = h5("Bad:",style="color:red"), 
                                  min = 0, max = 0.5, value = 0.25, step= 0.01),
                      sliderInput("UB", label = h5("Good:",style="color:#00FF00"), 
                                  min = 0.5, max = 1, value = 0.75, step= 0.01)
                                       
                      )
             )
      )
    )
  )
  #tags$head(
   # tags$style(
  #    ".selectize-dropdown, .selectize-input, .selectize-input { line-height: 54px}"
  #))
  )
)