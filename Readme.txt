
# =======================================================================================================
# === ABT - MSE file structure ==========================================================================
# =======================================================================================================

Jan '17
Tom Carruthers (t.carruthers@oceans.ubc.ca)

Key:
OM = Operating Model
MP = Management procedure

//ABT-MSE

/Data                           All data used in conditioning operating models or simulating future indices are included here
   
   /Previous assessments           Results from previous stock assessments for use in comparing OM results
   
   /Processed                      Data that are filtered or processed for use
      /Area definitions               The various oceanareas and their names
      /Conditioning                   These are the various data in the correct format for input into the M3 OM
      /CPUE indices                   A compilation of the standardized / nominal CPUE indices (courtesy of A.Kimoto)
      /MP indices                     A compilation of indices that may be simulated in the future for use by MPs
   
   /Raw                            Data without filtering or processing to meet OM specifications     
      /Aging                          Data for deriving growth curves
      /CAS                            Catch at Size data
      /Catch                          Nominal total removals
      /Fleet structure                Fleet definition guides
      /Misc                           Some old tagging data
      /PSAT                           Electronic tagging data
      /SOO			      Stock of origin data (courtesy of D. Secor et al. )
      /SSB                            Larval indices (hypothesized to be indicative of SSB)
      /Task1                          ICCAT Task I catch and effort data
      /Task2                          ICCAT Task II catch and effort data (finer scale)
      
/Genie                          Bayesian belief network summary of MSE outputs

/M3                             OM model directory containing .tpl etc

/Manuals and design documents   Reference guides and help documentation      

/Objects                        MSE R objects

   /Observation models             Various scenarios for how well data can be observed for use in MPs
   /OMs                            The various operating models including data inputs, ADMB outputs, derived OM objects
   /Recruitment scenarios          R list objects containing possible recruitment scenarios
   
/R package                      Directory of the ABT-MSE R package

/Reports                        Project reporting

/Results                        Raw results data and analysis

/RScripts                       R code for building operating models, observation models and debugging

   /Data processing             A series of scripts for processing the various data for operating model fitting
   
/Shiny                          A shiny app for communicating MSE results

/Simulated data                 Data for use in developing MPs or collaborating with other scientists

/Source                         The source code for the ABT-MSE R framework

/Submissions                    ABT-MSE related publications

