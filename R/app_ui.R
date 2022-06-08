#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import plotly
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinythemes::shinytheme("united"),
      navbarPage("EDA :) 1.4",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel("Load txt, csv or xlsx data file", 
                                         radioButtons('rodzaj_dane', 'Choose data file type?', c('txt', 'excel', 'example'), inline = TRUE),
                                         conditionalPanel(condition = 'input.rodzaj_dane == "txt"',
                                                          fileInput("dane", 'Choose .txt file',
                                                                    accept=c('.txt'), 
                                                                    multiple = TRUE)
                                         ),
                                         conditionalPanel(condition = 'input.rodzaj_dane == "excel"',
                                                          fileInput('dane_xls', 'Choose excel file')
                                         ),
                                         conditionalPanel(condition = 'input.rodzaj_dane == "txt"',
                                                          radioButtons('sep', 'Separator?',
                                                                       c(Comma=',',
                                                                         Semicolon=';',
                                                                         Tabulator='\t', 
                                                                         Space = " "),
                                                                       ' ', inline = TRUE)
                                         ),
                                         radioButtons("header", "Do data contain headers?", choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE),
                                         radioButtons('format', 'Change dataformat to long?', choices = list('Yes' = TRUE, 'No' = FALSE), selected = FALSE, inline = TRUE),
                                         uiOutput('kolumna_var'),
                                         uiOutput('kolumna_factor'),
                                         uiOutput('grupy'),
                                         h5('Produce html report containing all plots and statistical tests, may take a minute to compute :)'),
                                         downloadButton(
                                           outputId = "report",
                                           label = "Generate report"
                                         )
                                         
                                         # radioButtons('format', 'Format dokumentu', c('HTML'),
                                         #              inline = TRUE),
                                         # downloadButton('downloadReport')
                            ),
                            mainPanel(
                              h4('Data'),
                              dataTableOutput('input_data'),
                              h4('Data chosen for analysis'),
                              dataTableOutput("contents"),
                              uiOutput('summary_all')
                            )
                          )
                 ),
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("bin", "Bin width", value=0, step = 0.1),
                              radioButtons("facet", "Divide plot into facets?", choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE),
                              radioButtons("os_y", "Y axis type?", choices = list("count" = 1, "density" = 2), selected = 1, inline = TRUE),
                              radioButtons('kolory_hist', 'Choose color scale', c('default', 'colorbrewer', 'viridis', 'grayscale', 'custom'),
                                           selected = 'default', inline = TRUE),
                              conditionalPanel(
                                condition = "input.kolory_hist == 'colorbrewer'",
                                selectInput('colorbrewer_hist', label = 'Choose colorbrewer scale',
                                            choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                        'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                        'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                            selected = 'Set1', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory_hist == 'viridis'",
                                selectInput('viridis_hist', label = 'Choose viridis scale',
                                            choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                            selected = 'viridis', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory_hist == 'custom'",
                                textInput('wlasne_kolory_hist', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format')
                              ),
                              textInput('os_x', 'X axis name', 'Value'),
                              textInput('os_y_nazwa', 'Y axis name', 'Count'),
                              downloadButton('download_histogram', 'Download png plot'),
                              numericInput('width_hist', 'Plot width [cm]', 20, min = 5, max = 25),
                              numericInput('height_hist', 'Plot height [cm]', 14, min = 5, max = 25),
                              numericInput('res_hist', 'Resolution', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("histogram", height = "600px"))
                          )
                 ),
                 tabPanel("Density",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('fill_dens', 'Fill in density curves?', 
                                           choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                           selected = "FALSE", inline = TRUE),
                              radioButtons('kolory_dens', 'Choose color scale', c('default', 'colorbrewer', 'viridis', 'grayscale', 'custom'),
                                           selected = 'default', inline = TRUE),
                              conditionalPanel(
                                condition = "input.kolory_dens == 'colorbrewer'",
                                selectInput('colorbrewer_dens', label = 'Choose colorbrewer scale',
                                            choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                        'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                        'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                            selected = 'Set1', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory_dens == 'viridis'",
                                selectInput('viridis_dens', label = 'Choose viridis scale',
                                            choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                            selected = 'viridis', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory_dens == 'custom'",
                                textInput('wlasne_kolory_dens', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format')
                              ),
                              textInput('os_x_dens', 'X axis name', 'Value'),
                              textInput('os_y_dens', 'Y axis name', 'density'),
                              downloadButton('download_density', 'Download png plot'),
                              numericInput('width_dens', 'Plot width [cm]', 20, min = 5, max = 25),
                              numericInput('height_dens', 'Plot height [cm]', 14, min = 5, max = 25),
                              numericInput('res_dens', 'Resolution', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("density", height = "600px"))
                          )
                 ),
                 tabPanel("Boxplot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('boxviolin', 'Choose plot type', 
                                           c('Boxplot' = 'Boxplot', 'Violin' = 'Violin'
                                             #, 
                                             #'Mean with confidence intervals' = 'mean_ci'
                                           ), 
                                           inline = TRUE),
                              radioButtons('porownanie', 'Choose comparison type (for p-value display)', 
                                           list('None' = 'brak', 'Against control' = 'kontrola', 
                                                'Between certain groups (choose below)' = 'grupy')),
                              conditionalPanel(condition = 'input.porownanie == "kontrola"',
                                               numericInput('kontrola', 'Which group is the control?', 1, min = 1)
                              ),
                              conditionalPanel(condition = 'input.porownanie == "grupy"',
                                               textInput('porownania', 'Provide groups for comaprison, please use format:
                                          Typ_A Typ_B;Typ_A Typ_C')
                              ),
                              radioButtons('punkty', 'Include all observations?', 
                                           c('No' = 'none', 'Yes (beeswarm)' = 'beeswarm', 
                                             'Yes (quasirandom)' = 'quasirandom'), 
                                           inline = TRUE),
                              conditionalPanel(condition ='input.porownanie != "brak"' ,
                                               radioButtons('rodzaj_test', 'Choose test type?', 
                                                            c('t.test' = 't.test', 'wilcoxon' = 'wilcox.test'), inline = TRUE),
                                               radioButtons('p_format', 'Choose p-value format?', c('Numeric' = 'p.adj', 'Symbol' = 'p.signif'), inline = TRUE)
                              ),
                              radioButtons('anova', 'Show Anov or Kruskal-Wallis test result on the plot?', 
                                           c('No' = 'nie','Anova' = 'anova', 'Kruskal Wallis' = 'kruskal.test'), 
                                           selected = 'nie', inline = TRUE),
                              radioButtons('kolory', 'Choose color scale', c('default', 'colorbrewer', 'viridis', 'grayscale', 'custom'),
                                           selected = 'default', inline = TRUE),
                              conditionalPanel(
                                condition = "input.kolory == 'colorbrewer'",
                                selectInput('colorbrewer', label = 'Choose colorbrewer scale',
                                            choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                        'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                        'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                            selected = 'Set1', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory == 'viridis'",
                                selectInput('viridis', label = 'Choose viridis scale',
                                            choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                            selected = 'viridis', multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.kolory == 'custom'",
                                textInput('wlasne_kolory', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format')
                              ),
                              textInput('os_x_box', 'X axis name', 'Variable'),
                              textInput('os_y_box', 'Y axis name', 'Value'),
                              #textInput('legenda_nazwa_box', 'Nazwa legendy', ''),
                              downloadButton('download_boxplot', 'Download png plot'),
                              numericInput('width_box', 'Plot width [cm]', 20, min = 5, max = 25),
                              numericInput('height_box', 'Plot height [cm]', 14, min = 5, max = 25),
                              numericInput('res_box', 'Resolution', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("boxplot", height = "600px"))
                          )
                 ),
                 tabPanel("Summary",
                          
                          mainPanel(tableOutput("podsum"))
                          
                 ),
                 tabPanel("Normal distribution test",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(
                              h5('Distribution does not differ from normal distribution if the p-value is above 0.05'),
                              tableOutput("test_nor"),
                              plotOutput("nor_plot", height = 600))
                          )
                 ),
                 tabPanel("T-student test",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(tableOutput("ttest"))
                          )
                 ),
                 
                 tabPanel("Wilcoxon test",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(tableOutput("wtest"))
                          )
                 ),
                 
                 tabPanel("Anova",
                          sidebarLayout(
                            sidebarPanel(radioButtons("posthoc", 
                                                      "Are variances similar between groups?", 
                                                      choices = list("Yes (will use Tukey post-hoc test)" = "TRUE", "No (will use Games-Howell post-hoc test)" = "FALSE"), selected = "TRUE")),
                            mainPanel(verbatimTextOutput("anova1"),
                                      verbatimTextOutput("anova2"),
                                      conditionalPanel(
                                        condition = "input.posthoc == 'TRUE'",
                                        plotOutput('anova_plot')
                                      ),
                                      plotOutput('anova_plot_games'))
                          )
                 ),
                 tabPanel("Scatterplot",
                          #sidebarLayout(
                          #sidebarPanel(
                          fluidRow(
                            column(2, style = "background-color: #f8f9fa;",
                                   uiOutput('kolumna_scatter_x'),
                                   uiOutput('kolumna_scatter_y'),
                                   uiOutput('kolumna_scatter_color'),
                                   uiOutput('kolumna_scatter_facet'),
                                   
                                   #numericInput('alpha_point', 'Podaj zakres alpha dla punktów', value = 1, min = 0, max = 1, step = 0.1),
                                   sliderInput("alpha_point", "Choose point alpha value", min = 0, max = 1, value = 1, step = 0.1),
                                   sliderInput("size_point", "Choos epoint size", min = 1, max = 10, value = 2, step = 0.5),
                                   radioButtons('kolory_scatter', 'Choose color scale', c('default', 'colorbrewer', 'viridis', 'grayscale', 'custom'),
                                                selected = 'default', inline = TRUE),
                                   conditionalPanel(
                                     condition = "input.kolory_scatter == 'colorbrewer'",
                                     selectInput('colorbrewer_scatter', label = 'Choose colorbrewer scale',
                                                 choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                             'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                             'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                 selected = 'Spectral', multiple = FALSE)
                                   ),
                                   conditionalPanel(
                                     condition = "input.kolory_scatter == 'viridis'",
                                     selectInput('viridis_scatter', label = 'Choose viridis scale',
                                                 choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                 selected = 'viridis', multiple = FALSE)
                                   ),
                                   conditionalPanel(
                                     condition = "input.kolory_scatter == 'custom'",
                                     textInput('wlasne_kolory_scatter', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format')
                                   ),
                                   #textInput('os_x_sc', 'X axis name', 'Wartość'),
                                   #textInput('os_y_sc', 'Y axis name', 'Liczba'),
                                   #textInput('legenda_nazwa_box', 'Nazwa legendy', ''),
                                   downloadButton('download_scatter', 'Download plot'),
                                   numericInput('width_scatter', 'Plot width [cm]', 20, min = 5, max = 25),
                                   numericInput('height_scatter', 'Plot height [cm]', 14, min = 5, max = 25),
                                   numericInput('res_scatter', 'Resolution', 200, min = 100, max = 500)
                                   
                                   
                            ),
                            #mainPanel(
                            column(8,
                                   plotlyOutput("scatter", height = 600),
                                   tableOutput('tabela_korelacja'),
                                   tableOutput('tabela_lm'),
                                   tableOutput('scatter_test')
                                   
                            ),
                            column(2, style = "background-color: #f8f9fa;",
                                   checkboxInput('trend', 'Add trend line?',
                                                 value =  FALSE),
                                   conditionalPanel(
                                     condition = "input.trend",
                                     radioButtons('rodzaj_trend', 'Choose trend line type',
                                                  choices = list('Loess' = 'loess',
                                                                 'Linear (lm)' = 'lm')),
                                     conditionalPanel(
                                       condition = "input.rodzaj_trend == 'loess'",
                                       numericInput('span', 'Choose the degree of smoothing',
                                                    value = 0.75, min = 0, step = 0.05)
                                     ),
                                     checkboxInput('se', 'Show confidence interval?', value = TRUE),
                                     numericInput('size_trend', "Choose trend line size",
                                                  value = 1, step = 0.5, min = 0)
                                   ),
                                   radioButtons('corr', 'Calculate correlation?',
                                                choices = list('No' = 'nie', 
                                                               'Yes (pearson)' = 'pearson',
                                                               'Yes (spearman)' = 'spearman'
                                                ))
                            )
                          )
                 )
                 
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'EDA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

