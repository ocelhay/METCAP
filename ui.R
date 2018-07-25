# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ui.R
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# METCAP Shiny App
# Authors: Olivier Celhay - olivier.celhay@gmail.com, Sheetal Silal - Sheetal.Silal@uct.ac.za
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(leaflet)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "METCAP", titleWidth=250)



sidebar <- dashboardSidebar(width = 250,
                            
                            # menu
                            sidebarMenu(id='menu',
                                        menuItem("What is METCAP?", tabName = "welcome", icon=icon("info-circle")),
                                        menuItem("Baseline Data: Data Collation", tabName = 'data', icon = icon("database")),
                                        menuItem("Model Stage 1: Build Estimates", tabName = 'model1', icon = icon("sliders")),
                                        menuItem("Model Stage 2: Model Scenarios", tabName = 'model2', icon = icon("sliders")),
                                        menuItem("Model Stage 3: Scenario Costing", tabName = 'model3', icon = icon("dollar"))
                            ),
                            
                            # region and country selection
                            conditionalPanel(condition = "input.menu == 'data' | input.menu == 'model1' | input.menu == 'model2' | input.menu == 'model3'",
                                             selectInput('region', label='Area/Region:', choices=c('Asia-Pacific', 'Western Pacific Region', 'South-East Asia Region', 'Eastern Mediterranean Region', 'Greater Mekong Subregion', 'Custom'),
                                                         selected="Asia-Pacific"),
                                             checkboxGroupInput("countries", 'Countries:',
                                                                choiceNames = mapply(vector_countries, flags, FUN = function(vector_countries, flagUrl) {
                                                                  tagList(
                                                                    tags$img(src=flagUrl, width=20, height=20),
                                                                    vector_countries)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), 
                                                                choiceValues = vector_countries, inline = FALSE, selected = vector_countries)
                                             
                            )
                            
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "note",
            includeMarkdown('./www/markdown/disclaimer.md')),
    tabItem(tabName = "welcome",
            fluidRow(
              includeCSS("./www/styles.css"),
              column(width=12,
                     tabBox(width=NULL, title=NULL,
                            tabPanel('About',
                                     div(style="float:right", bsButton(inputId = "help_video_welcome", 
                                                                       label = "Instructional Video", icon = icon("video-camera"), style = "primary")),
                                     includeMarkdown('./www/markdown/about-app.md')),
                            tabPanel('The Project', includeMarkdown('./www/markdown/about-project.md')),
                            tabPanel('The Team', includeMarkdown('./www/markdown/about-team.md')),
                            tabPanel('Other Resources', includeMarkdown('./www/markdown/about-other.md')),
                            tabPanel('Contact', includeMarkdown('./www/markdown/contact.md'))
                     )))
    ),
    
    tabItem(tabName = "data",
            fluidRow(
              column(width=12,
                     tabBox(width=NULL, title=NULL,
                            tabPanel('About', 
                                     div(style="float:right", bsButton(inputId = "help_video_data", 
                                                                       label = "Instructional Video", icon = icon("video-camera"), style = "primary")),
                                     includeMarkdown('./www/markdown/about-data.md')),
                            tabPanel('Baseline Data', 
                                     h4('Source: WHO Malaria Report (2014) and Howes et al. (2012) for G6PDd'),
                                     h5('NA = Information Not Available'),
                                     absolutePanel(selectInput('indicator_map', NULL,
                                                               list(Population=c(
                                                                 'Population At Risk' = 'population_at_risk'),
                                                                 `G6PDd (Howes et al. (2012)`=c(
                                                                   'G6PDd rates' = 'G6PD_rates'),
                                                                 `Epidemiology (WHO Malaria Report 2014)`=c(
                                                                   '1000*(Total Cases/Population At Risk)'='API_total',
                                                                   'Proportion of Pf cases (Non including Community-detcted cases)'='ratio_nc_pf'
                                                                 ),
                                                                 `Intervention coverage (WHO Malaria Report 2014)`=c(
                                                                   '% IRS Coverage'='IRS_cov',
                                                                   '% ITN Coverage'='ITN_cov',
                                                                   '% Any Antimalarial Coverage'='AM_cov',
                                                                   '% ACT Coverage'='ACT_cov')
                                                               ),
                                                               selected='Population At Risk'),
                                                   top='10%', right='3%', width='250px', height='200px'),
                                     leafletOutput('map_data', height=600) %>% withSpinner(),
                                     HTML('<em>The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance.</em>'),
                                     HTML('<br><br><br>'),
                                     h4('Source: WHO Malaria Report (2008; 2010-2015)'),
                                     selectInput('indicator_ts', NULL, width='30%',
                                                 list(
                                                   `Morbidity`=c(
                                                     'Total Cases' = 'malaria_cases',
                                                     'Proportion of Pf cases (Non including Community-detected cases)'='ratio_nc_pf'
                                                     # 'Non Community Pf' = 'nc-pf',
                                                     # 'Non Community Pv' = 'nc-pv',
                                                     # 'Community cases (Pf+Pv)'='c-pfpv'
                                                   ),
                                                   `Mortality`=c(
                                                     'Reported Cases Fatalities'='death'
                                                   ),
                                                   `Interventions`=c(
                                                     '% ITN Coverage'='cover-itn',
                                                     '% IRS Coverage'='cover-irs',
                                                     '% Any Antimalarial Coverage'='cover-antimal',
                                                     '% ACT Coverage'='cover-act'
                                                   ),
                                                   `Population`=c(
                                                     'Population At Risk (High Transmission)' = 'pop-risk-high',
                                                     'Population At Risk (Low Transmission)' = 'pop-risk-low'
                                                   )),
                                                 selected='nc-pf'),
                                     plotOutput('ts_data_all'),
                                     plotOutput('ts_data'))
                     )
              ))),
    
    tabItem(tabName = "model1",
            fluidRow(
              column(width=12,
                     tabBox(width=NULL, title=NULL,
                            tabPanel('About', 
                                     div(style="float:right", bsButton(inputId = "help_video_model1", 
                                                                       label = "Instructional Video", icon = icon("video-camera"), style = "primary")),
                                     includeMarkdown('./www/markdown/about-model1.md')),
                            
                            tabPanel('Health Seeking Estimates',
                                     absolutePanel(selectInput('indicator_map_model1', NULL,
                                                               list(
                                                                 `Health Seeking Estimates`=c(
                                                                   'Minimum'='minimum',
                                                                   'Median'='median',
                                                                   'Maximum'='maximum'
                                                                 )),
                                                               selected='median'),
                                                   top='15%', right='3%', width='250px', height='200px'),
                                     h4('Health Seeking Estimates'),
                                     leafletOutput('map_model1', height=600) %>% withSpinner(),
                                     HTML('<em>The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance.</em>')
                            ),
                            
                            tabPanel('Estimated Clinical Burden', 
                                     HTML('<h3>Estimated Clinical Pf burden</h3>'),
                                     plotOutput('ts_model1_pf') %>% withSpinner(),
                                     HTML('<h3>Estimated Clinical Pv burden</h3>'),
                                     plotOutput('ts_model1_pv')),
                            tabPanel('Model Calibration',
                                     fluidRow(
                                       column(width=12,
                                              includeMarkdown('./www/markdown/calibration.md'),
                                              # HTML('<bold>The mathematical model was calibrated to simulate the estimated clinical burden for <em>Plasmodium falciparum</em> and <em>Plasmodium vivax</em>  for the period 2000 to 2015</bold><br><br><br>'),
                                              HTML('<h3>Model Calibration for Pf</h3>'),
                                              plotOutput('model_calibration_pf') %>% withSpinner(),
                                              HTML('<h3>Model Calibration for Pv</h3>'),
                                              plotOutput('model_calibration_pv')
                                       )))
                            
                            
                     )
                     
                     
              )
            )
    ),
    
    tabItem(tabName = "model2",
            
            fluidRow(
              column(width=12,
                     tabBox(width=NULL, title=NULL,
                            tabPanel('About',
                                     div(style="float:right", bsButton(inputId = "help_video_model2", 
                                                                       label = "Instructional Video", icon = icon("video-camera"), style = "primary")),
                                     includeMarkdown('./www/markdown/about-model2-1.md'),
                                     fluidRow(
                                       column(width=3, 
                                              h3('REVERSE'),
                                              popup_reverse),
                                       column(width=3, 
                                              h3('CONTINUE'),
                                              popup_continue),
                                       column(width=3, 
                                              h3('ACCELERATE'),
                                              popup_accelerate),
                                       column(width=3, 
                                              h3('INNOVATE'),
                                              popup_innovate)
                                     )
                            ),
                            tabPanel('Scenario Comparison',
                                     fluidRow(
                                       column(width=8,
                                              h3('Select Scenario(s):'),
                                              
                                              fluidRow(
                                                column(width=3,
                                                       checkboxGroupInput('scenario_reverse', 'Reverse:', choices=scenario_names[4:2], selected=NULL, inline = FALSE)),
                                                column(width=3,
                                                       checkboxGroupInput('scenario_continue', 'Continue:', choices=scenario_names[1], selected=scenario_names[1], inline = FALSE)),
                                                column(width=3,
                                                       checkboxGroupInput('scenario_accelerate', 'Accelerate:', choices=scenario_names[5:7], selected=NULL, inline = FALSE)),
                                                column(width=3,
                                                       checkboxGroupInput('scenario_innovate', 'Innovate:', choices=scenario_names[8:10], selected=NULL, inline = FALSE)),
                                                
                                                bsPopover(id='scenario_reverse', title='Reverse:', content=popup_reverse, placement='right', options = list(container = "body")),
                                                bsPopover(id='scenario_continue', title='Continue:', content=popup_continue, placement='right', options = list(container = "body")),
                                                bsPopover(id='scenario_accelerate', title='Accelerate:', content=popup_accelerate, placement='right', options = list(container = "body")),
                                                bsPopover(id='scenario_innovate', title='Innovate:', content=popup_innovate, placement='right', options = list(container = "body"))
                                              ),
                                              fluidRow(
                                                column(width=6,
                                                       radioButtons('resistance', 'Drug Resistance:', choices=c('Stable', 'Increasing'), selected = 'Stable', inline = TRUE, width = NULL)),
                                                column(width=6,
                                                       radioButtons('mda', 'MDA:', choices=c('Without MDA', 'With MDA', 'Show both'), selected = 'Without MDA', inline = TRUE, width = NULL),
                                                       radioButtons('itn', 'ITN Scale-up:', choices=c('Maintain ITN', 'Scale-up ITN', 'Show both'), selected = 'Maintain ITN', inline = TRUE, width = NULL)),
                                                
                                                
                                                bsPopover("resistance", title='Drug Resistance', "<p>Stable Resistance: Probability of treatment failure to ACTs is constant at 5% for all countries</p><p>Increasing Resistance: Probability of treatment failure to ACTs is constant at 5% across all countries until 2018, when it increases steadily to 30% by 2025</p>", "right", trigger='hover', options = list(container = "body")),
                                                bsPopover("mda", title='MDA', 'Five annual rounds of MDA at 50% coverage, from 2018, starting 4 months before the peak of the season', "right", trigger='hover', options = list(container = "body")),
                                                bsPopover("itn", title='ITN Scale-up', '<p>Maintain ITN: Sustain current levels of net coverage</p><p>Scale-up ITN: Increase net coverage to 80% in three year cycles from 2017 to 2026</p>', "right", trigger='hover', options = list(container = "body"))
                                              ),
                                              fluidRow(column(width=12, htmlOutput('message_counterfactual')))),
                                       
                                       column(width=4,
                                              h3('Select Predictors and Scale:'),
                                              selectInput('prediction', label='Model Prediction:', choices=list_prediction),
                                              selectInput('scale', label='Graph scale:', choices=c('Log scale', 'Absolute numbers'), selected='Absolute numbers'),
                                              HTML('<br><br>'),
                                              actionButton('goButton', 'Make/Refresh Plots', icon('refresh'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       )),
                                     fluidRow(
                                       column(width=12,
                                              plotOutput('ts_model2_global', height=600) %>% withSpinner(),
                                              HTML('<br><br>'),
                                              plotOutput('ts_model2_grid', height=600)
                                       ))),
                            tabPanel('Elimination Timeline',
                                     fluidRow(
                                       column(width=4,
                                              selectInput('scenario_unique', 'Scenario:', choices=scenario_names[c(4:1, 5:10)], selected='Business as usual'),
                                              radioButtons('elimination_type', 'Elimination:', choices=c('Observed'='elim_obs_year', 'Clinical'='elim_clinical_year'), selected='elim_obs_year', inline=TRUE)
                                       ),
                                       column(width=4,
                                              radioButtons('resistance_elim', 'Drug Resistance:', choices=c('Stable', 'Increasing'), selected='Stable', inline=TRUE)
                                              
                                       ),
                                       column(width=4,
                                              radioButtons('mda_elim', 'MDA:', choices=c('Without MDA'='No', 'With MDA'='Yes'), selected='No', inline=TRUE),
                                              radioButtons('itn_elim', 'ITN Scale-up:', choices=c('Maintain ITN'='No', 'Scale-up ITN'='Yes'), selected='No', inline=TRUE)
                                       ),
                                       bsPopover("elimination_type", title='Elimination Thresholds', '<p>Observed: Estimated reported treated cases</p><p>Clinical: Estimated clinical cases (treated and untreated)</p>', "right", trigger='hover', options = list(container = "body")),
                                       bsPopover("resistance_elim", title='Drug Resistance', "<p>Stable Resistance: Probability of treatment failure to ACTs is constant at 5% for all countries</p><p>Increasing Resistance: Probability of treatment failure to ACTs is constant at 5% across all countries until 2018, when it increases steadily to 30% by 2025</p>", "right", trigger='hover', options = list(container = "body")),
                                       bsPopover("mda_elim", title='MDA', 'Five annual rounds of MDA at 50% coverage, from 2018, starting 4 months before the peak of the season', "left", trigger='hover', options = list(container = "body")),
                                       bsPopover("itn_elim", title='ITN Scale-up', '<p>Maintain ITN: Sustain current levels of net coverage</p><p>Scale-up ITN: Increase net coverage to 80% in three year cycles from 2017 to 2026</p>', "left", trigger='hover', options = list(container = "body"))
                                     ),
                                     
                                     fluidRow(
                                       column(width=12,
                                              HTML('<h4>Year of elimination for the selected scenario:</h4> Countries arranged from top to bottom in alphabetical order and from right to left based on year of elimination (min, median, max). “NO” means elimination was not predicted to be achieved by 2030.'),
                                              plotOutput('elim_timeline', height='550px') %>% withSpinner()
                                       ))
                            ),
                            tabPanel('Minimum Elimination Package',
                                     fluidRow(
                                       column(width=4, includeMarkdown('./www/markdown/conservative-package.md'),
                                              actionButton("link_to_tabpanel_costing", "Costs of these packages", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ),
                                       column(width=8,
                                              leafletOutput('elimination_minimal_scenario', height=600) %>% withSpinner(),
                                              tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
                                              HTML('<em>The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance.</em>')
                                       )),
                                     fluidRow(
                                       column(width=12,
                                              HTML('<br><br><br>'),
                                              DT::dataTableOutput('table_elimination')))
                            )
                     )
              )
            )
    ),
    
    tabItem(tabName = "model3",
            
            fluidRow(
              column(width=12,
                     tabBox(width=NULL, title=NULL, id='panel_costs',
                            tabPanel('About', value='costs_about_tab', 
                                     div(style="float:right", bsButton(inputId = "help_video_model3", 
                                                                       label = "Instructional Video", icon = icon("video-camera"), style = "primary")),
                                     includeMarkdown('./www/markdown/about-model3.md')),
                            tabPanel('Scenario Cost Comparison', value='costs_tab',
                                     fluidRow(
                                       column(width=4,
                                              selectInput('scenario_unique_cost', 'Scenario:', choices=scenario_names[c(4:1, 5:10)], selected='Business as usual'),
                                              sliderInput('range_cost', 'Years range:', min=2016, max=2030, value=c(2016, 2030), step=1, sep='')
                                       ),
                                       column(width=4,
                                              radioButtons('resistance_cost', 'Drug Resistance:', choices=c('Stable', 'Increasing'), selected='Stable', inline=TRUE)
                                       ),
                                       column(width=4,
                                              radioButtons('mda_cost', 'MDA:', choices=c('Without MDA'='No', 'With MDA'='Yes'), selected='No', inline=TRUE),
                                              radioButtons('itn_cost', 'Scale-up ITN:', choices=c('Maintain ITN'='No', 'Scale-up ITN'='Yes'), selected='No', inline=TRUE)
                                       ),
                                       bsPopover("resistance_cost", title='Drug Resistance', "<p>Stable Resistance: Probability of treatment failure to ACTs is constant at 5% for all countries</p><p>Increasing Resistance: Probability of treatment failure to ACTs is constant at 5% across all countries until 2018, when it increases steadily to 30% by 2025</p>", "right", trigger='hover', options = list(container = "body")),
                                       bsPopover("mda_cost", title='MDA', 'Five annual rounds of MDA at 50% coverage, from 2018, starting 4 months before the peak of the season', "left", trigger='hover', options = list(container = "body")),
                                       bsPopover("itn_cost", title='ITN Scale-up', '<p>Maintain ITN: Sustain current levels of net coverage</p><p>Scale-up ITN: Increase net coverage to 80% in three year cycles from 2017 to 2026</p>', "left", trigger='hover', options = list(container = "body"))
                                     ),
                                     fluidRow(
                                       column(width=6,
                                              htmlOutput('total_pop_risk')),
                                       column(width=6,
                                              htmlOutput('total_target_pop_risk'))
                                     ),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput('boxplot_cost_total') %>% withSpinner()
                                       ),
                                       column(width=6,
                                              plotOutput('boxplot_cost_target')
                                       )
                                     ),
                                     fluidRow(
                                       # column(width=6,
                                       #        plotOutput('plot_cost_component')
                                       # ),
                                       column(width=12,
                                              plotOutput('plot_cost_year')
                                       )
                                     )
                                     
                                     # ,
                                     # 
                                     # fluidRow(
                                     #   column(width=4,
                                     #          uiOutput("plots_c1")
                                     #   ),
                                     #   column(width=4,
                                     #          uiOutput("plots_c2")
                                     #   ),
                                     #   column(width=4,
                                     #          HTML('TO BE ADDEDD a table per country with the name of the package, total cost during the whole period, cost breakdown per post (LLIN, CHWs etc.)')
                                     #          # plotOutput("cost_c3", height = '1000px')
                                     #          # uiOutput("plots_c1") # This is the dynamic UI for the plots
                                     #   )
                                     # )
                                     
                            ),
                            tabPanel('Minimum Elimination Package Cost', value="mep_cost_tab",
                                     fluidRow(
                                       column(width=4,
                                              includeMarkdown('./www/markdown/conservative-package.md')
                                       ),
                                       column(width=8,
                                              leafletOutput('elimination_minimal_scenario_2', height=600) %>% withSpinner(),
                                              HTML('<em>The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance.</em>')
                                       )
                                     ),
                                     fluidRow(
                                       column(width=6,
                                              htmlOutput('total_pop_risk_mep')),
                                       column(width=6,
                                              htmlOutput('total_target_pop_risk_mep'))
                                     ),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput('boxplot_cost_total_mep')
                                       ),
                                       column(width=6,
                                              sliderInput('range_cost_2', 'Years range:', min=2016, max=2030, value=c(2016, 2030), step=1, sep='')
                                       )
                                     ),
                                     fluidRow(
                                       # column(width=6,
                                       #        plotOutput('plot_cost_component_mep')
                                       # ),
                                       column(width=12,
                                              plotOutput('plot_cost_year_mep')
                                       )
                                     )
                            )
                            
                            
                     )
              )
              
            )
    )
    
  )
)


dashboardPage(header, sidebar, body)