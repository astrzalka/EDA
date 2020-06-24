#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      navbarPage("EDA :) 1.3",
                 tabPanel("Dane",
                          sidebarLayout(
                            sidebarPanel("Wczytaj plik w formacie .txt, .csv lub .excel", 
                                         radioButtons('rodzaj_dane', 'Jakie dane chcesz wczytać?', c('txt', 'excel', 'przykład'), inline = TRUE),
                                         fileInput("dane", 'Wybierz plik .txt',
                                                   accept=c('.txt')),
                                         fileInput('dane_xls', 'Wybierz plik excel'),
                                         radioButtons('sep', 'Separator? (tylko pliki txt)',
                                                      c(Przecinek=',',
                                                        Średnik=';',
                                                        Tabulator='\t', 
                                                        Spacja = " "),
                                                      ' ', inline = TRUE),
                                         radioButtons("header", "Czy kolummy mają nagłówki", choices = list("Tak" = TRUE, "Nie" = FALSE), selected = TRUE, inline = TRUE),
                                         radioButtons('format', 'Czy zmienić format danych na wąski', choices = list('Tak' = TRUE, 'Nie' = FALSE), selected = FALSE, inline = TRUE),
                                         numericInput("num1", "Numer kolumny zmiennej objaśnianej", value = 1),
                                         numericInput("num2", "Numer kolumny zmiennej objaśniającej", value = 2)
                                         # radioButtons('format', 'Format dokumentu', c('HTML'),
                                         #              inline = TRUE),
                                         # downloadButton('downloadReport')
                            ),
                            mainPanel(tableOutput("contents"))
                          )
                 ),
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("bin", "Szerokość słupków", value=1, step = 0.1),
                              radioButtons("facet", "Czy podzielić na panele?", choices = list("Tak" = "TRUE", "Nie" = "FALSE"), selected = "TRUE", inline = TRUE),
                              radioButtons("os_y", "Oś Y?", choices = list("count" = 1, "density" = 2), selected = 1, inline = TRUE),
                              radioButtons('kolory_hist', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                           selected = 'domyślna', inline = TRUE),
                              selectInput('colorbrewer_hist', label = 'Którą skalę Colorbrewer zastosować?', 
                                          choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent'), 
                                          selected = 'Set1', multiple = FALSE),
                              selectInput('viridis_hist', label = 'Którą skalę viridis zastosować?', 
                                          choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'), 
                                          selected = 'viridis', multiple = FALSE),
                              textInput('wlasne_kolory_hist', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000'),
                              textInput('os_x', 'Nazwa osi X', 'Wartość'),
                              textInput('os_y_nazwa', 'Nazwa osi Y', 'Liczba'),
                              downloadButton('download_histogram', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                              numericInput('width_hist', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                              numericInput('height_hist', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                              numericInput('res_hist', 'Rozdzielczość', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("histogram", height = "600px"))
                          )
                 ),
                 tabPanel("Density",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('fill_dens', 'Czy dodać wypełnienie?', 
                                           choices = list("Tak" = "TRUE", "Nie" = "FALSE"), 
                                           selected = "FALSE", inline = TRUE),
                              radioButtons('kolory_dens', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                           selected = 'domyślna', inline = TRUE),
                              selectInput('colorbrewer_dens', label = 'Którą skalę Colorbrewer zastosować?', 
                                          choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent'), 
                                          selected = 'Set1', multiple = FALSE),
                              selectInput('viridis_dens', label = 'Którą skalę viridis zastosować?', 
                                          choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'), 
                                          selected = 'viridis', multiple = FALSE),
                              textInput('wlasne_kolory_dens', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000'),
                              textInput('os_x_dens', 'Nazwa osi X', 'Wartość'),
                              textInput('os_y_dens', 'Nazwa osi Y', 'Liczba'),
                              downloadButton('download_density', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                              numericInput('width_dens', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                              numericInput('height_dens', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                              numericInput('res_dens', 'Rozdzielczość', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("density", height = "600px"))
                          )
                 ),
                 tabPanel("Boxplot i przyjaciele",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('boxviolin', 'Jaki wykres narysować?', 
                                           c('Boxplot' = 'Boxplot', 'Violin' = 'Violin', 'Średnia z przedziałem ufności' = 'mean_ci'), 
                                           inline = TRUE),
                              radioButtons('porownanie', 'Jakie chcesz wykonać porównanie', 
                                           list('brak' = 'brak', 'Tylko wobec kontroli' = 'kontrola', 
                                                'Pomiędzy niektórymi grupami (podaj niżej)' = 'grupy')),
                              numericInput('kontrola', 'Która grupa to kontrola?', 1, min = 1),
                              textInput('porownania', 'Tutaj wpisz grupy do porównania w formacie:
                                          Typ_A Typ_B;Typ_A Typ_C'),
                              radioButtons('punkty', 'Czy dodać wszystkie obserwacje?', 
                                           c('Nie' = 'none', 'Tak (jitter)' = "jitter", 'Tak (beeswarm)' = 'beeswarm', 
                                             'Tak (quasirandom)' = 'quasirandom'), 
                                           inline = TRUE),
                              radioButtons('rodzaj_test', 'Jaki test zastosować?', 
                                           c('t.test' = 't.test', 'wilcoxon' = 'wilcox.test'), inline = TRUE),
                              radioButtons('p_format', 'Jak pokazać wartość p?', c('Liczbowo' = 'p.adj', 'Gwiazdki' = 'p.signif'), inline = TRUE),
                              radioButtons('anova', 'Czy dodać wynik Anova lub Kruskal Wallis test?', 
                                           c('Nie' = 'nie','Anova' = 'anova', 'Kruskal Wallis' = 'kruskal.test'), 
                                           selected = 'nie', inline = TRUE),
                              radioButtons('kolory', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                           selected = 'domyślna', inline = TRUE),
                              selectInput('colorbrewer', label = 'Którą skalę Colorbrewer zastosować?', 
                                          choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent'), 
                                          selected = 'Set1', multiple = FALSE),
                              selectInput('viridis', label = 'Którą skalę viridis zastosować?', 
                                          choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'), 
                                          selected = 'viridis', multiple = FALSE),
                              textInput('wlasne_kolory', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000'),
                              textInput('os_x_box', 'Nazwa osi X', 'Wartość'),
                              textInput('os_y_box', 'Nazwa osi Y', 'Liczba'),
                              #textInput('legenda_nazwa_box', 'Nazwa legendy', ''),
                              downloadButton('download_boxplot', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                              numericInput('width_box', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                              numericInput('height_box', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                              numericInput('res_box', 'Rozdzielczość', 200, min = 100, max = 500)
                            ),
                            mainPanel(plotOutput("boxplot", height = "600px"))
                          )
                 ),
                 tabPanel("Podsumowanie",
                          
                          mainPanel(tableOutput("podsum"))
                          
                 ),
                 tabPanel("Test t-studenta",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(tableOutput("ttest"))
                          )
                 ),
                 
                 tabPanel("Test Wilcoxona",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(tableOutput("wtest"))
                          )
                 ),
                 
                 tabPanel("Anova",
                          sidebarLayout(
                            sidebarPanel(radioButtons("posthoc", 
                                                      "Czy wariancje w grupach są podobne?", 
                                                      choices = list("TAK (użyje testu post-hoc Tukeya)" = "TRUE", "NIE (użyje testu post-hoc Gamesa-Howella)" = "FALSE"), selected = "TRUE")),
                            mainPanel(verbatimTextOutput("anova1"),
                                      verbatimTextOutput("anova2"),
                                      plotOutput('anova_plot'))
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
