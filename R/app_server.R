#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggpubr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  library(magrittr)
  #mean_ci <- ggpubr::mean_ci
  dane <- reactive({
    if(input$rodzaj_dane == 'txt'){
      inFile <- input$dane
      if (is.null(inFile))
        # d <- przyklad
        # return(d)
        return(NULL)
      d <- read.table(inFile$datapath, header=as.logical(input$header), fill = TRUE, sep = input$sep, quote = "\"")
      return(d)
    }
    if(input$rodzaj_dane == 'excel'){
      inFile_excel <- input$dane_xls
      if (is.null(inFile_excel))
        return(NULL)
      d <- readxl::read_excel(path = inFile_excel$datapath, col_names = as.logical(input$header))
      d <- as.data.frame(d) # tibble did not work in filtering later
      return(d)
    }
    
    if(input$rodzaj_dane == 'przykład'){
      d <- przyklad
      return(d)
    }
  }) 
  
  
  final <- reactive ({
    
    if(input$format == FALSE){
      
      grupy <- colnames(dane())
      
      numer_1 <- which(grupy == input$kolumna_var)
      numer_2 <- which(grupy == input$kolumna_factor)
      
      dane <- wybor(dane(), num1 = numer_1, num2 = numer_2)
    } else {
      dane_2 <- dane()
      
      dane_2 <- tidyr::gather(dane_2, rodzaj, wartosc)
      
      dane <- dane_2[,c(2,1)]
      
    }
    
    dane <- dane[dane[,2] %in% input$grupy,]
    
    
    
    return(dane)
  })
  
  
  final_scatter <- reactive ({
    
    if(input$format == FALSE){
      dane <- dane()
      grupy <- colnames(dane)
      
      numer_1 <- which(grupy == input$kolumna_scatter_x)
      numer_2 <- which(grupy == input$kolumna_scatter_y)
      
      numery <- c(numer_1, numer_2)
      
      if(input$kolumna_scatter_color != 'brak'){
        numer_3 <- which(grupy == input$kolumna_scatter_color)
        numery <- c(numery, numer_3)
      }
      
      if(input$kolumna_scatter_facet != 'brak'){
        numer_4 <- which(grupy == input$kolumna_scatter_facet)
        numery <- c(numery, numer_4)
      }
      
      dane <- dane[,numery]
    } else {
      return(NULL)
    }
    
    # dane <- dane[dane[,2] %in% input$grupy,]
    
    
    
    return(dane)
  })
  
  output$kolumna_var <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_var", "Wybierz zmienną do analizy",
                choices = grupy, selected = grupy[1])
    
  })
  
  output$kolumna_factor <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_factor", "Wybierz zmienną zawierającą grupy",
                choices = grupy, selected = grupy[2])
    
  })
  
  output$grupy <- renderUI({
    
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    if(input$format == TRUE){
      dane <- dane()
      
      dane <- tidyr::gather(dane, rodzaj, wartosc)
      
      dane <- dane[,c(2,1)]
    } else {
      grupy <- colnames(dane())
      
      numer_1 <- which(grupy == input$kolumna_var)
      numer_2 <- which(grupy == input$kolumna_factor)
      
      dane <- wybor(dane(), num1 = numer_1, num2 = numer_2)
      
    }
    
    colnames(dane) <- c('wartosc', 'grupy')
    
    grupy <- unique(dane$grupy)
    
    checkboxGroupInput("grupy", label = ("Wybierz grupy do analizy"), 
                       choices = grupy,
                       selected = grupy)
    
  })
  
  
  output$input_data <- renderDataTable(dane(), options = list(
    pageLength = 5
  ))
  
  output$contents <- renderDataTable(final(), options = list(
    pageLength = 5
  ))
  
  histogramInput <- reactive({
    
    envir <- environment()
    
    wb <- final()
    
    nazwy <- colnames(wb)
    
    
    if(input$bin == 0){
      
      # domyślnie zgaduje szerokość słupków do histogramu na podstawie zakresu danych
      bin <- abs((range(wb[,nazwy[1]])[2] - range(wb[,nazwy[1]])[1])/20) 
    } else {
      
      bin <- input$bin
    }
    
    p <- draw_histogram(wb = wb,
                        variable = nazwy[1],
                        facet_draw = input$facet,
                        facet_var = nazwy[2],
                        bin = bin,
                        y_density = input$os_y,
                        x_name = input$os_x,
                        y_name = input$os_y_nazwa,
                        kolory = input$kolory_hist,
                        viridis = input$viridis_hist,
                        brewer = input$colorbrewer_hist,
                        wlasne = input$wlasne_kolory_hist)
    
    print(p)
    
    
  })  
  
  output$histogram <- renderPlot({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    print(histogramInput())
  })
  
  output$download_histogram <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res_hist, width = input$width_hist, input$height_hist, unit = 'cm')
      print(histogramInput())
      dev.off()
    })
  
  densityInput <- reactive({
    envir <- environment()
    
    wb <- final()
    
    nazwy <- colnames(wb)
    
    p <- draw_density(wb = wb,
                      variable = nazwy[1],
                      color_var = nazwy[2],
                      fill = input$fill_dens,
                      x_name = input$os_x_dens,
                      y_name = input$os_y_dens,
                      kolory = input$kolory_dens,
                      viridis = input$viridis_dens,
                      brewer = input$colorbrewer_dens,
                      wlasne = input$wlasne_kolory_dens)
    
    print(p)
    
  })
  
  output$density <- renderPlot({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    print(densityInput())
  })
  
  output$download_density <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res_dens, width = input$width_dens, input$height_dens, unit = 'cm')
      print(densityInput())
      dev.off()
    })
  
  boxplotInput <- reactive({
    
    wb <- final()
    wb <- as.data.frame(wb)
    nazwy <- colnames(wb)
    
    p <- draw_boxplot(wb = wb,
                      x_var = nazwy[2],
                      y_var = nazwy[1],
                      type = input$boxviolin,
                      p_format = input$p_format,
                      porownanie = input$porownanie,
                      punkty = input$punkty,
                      anova = input$anova,
                      test_type = input$rodzaj_test,
                      kontrola = input$kontrola,
                      grupy_porownania = input$porownania,
                      x_name = input$os_x_box,
                      y_name = input$os_y_box,
                      kolory = input$kolory,
                      viridis = input$viridis,
                      brewer = input$colorbrewer,
                      wlasne = input$wlasne)
    
    return(p)
    
  })
  
  output$boxplot <- renderPlot({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    print(boxplotInput())
  })
  
  output$download_boxplot <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res_box, width = input$width_box, input$height_box, unit = 'cm')
      print(boxplotInput())
      dev.off()
    })
  
  
  output$podsum <- renderTable({
    
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    dane <- final()
    
    nazwy <- colnames(dane)
    
    envir <- environment()
    
    colnames(dane)[2] <- 'grupy'
    
    dane %>% dplyr::group_by(grupy) %>% 
      dplyr::summarise(średnia = round(mean(eval(parse(text = nazwy[1])), na.rm = TRUE),2),
                        mediana = round(median(eval(parse(text = nazwy[1])), na.rm = TRUE),2),
                        odchylenie = round(sd(eval(parse(text = nazwy[1])), na.rm = TRUE),2),
                        minimum = round(min(eval(parse(text = nazwy[1])), na.rm = TRUE),2),
                        maximum = round(max(eval(parse(text = nazwy[1])), na.rm = TRUE),2), 
                        n = length(eval(parse(text = nazwy[1]))),
                        przed.ufnosci.nor = round(1.96 * (odchylenie/sqrt(n)),2))
    
  })
  
  
  test_nor_wynik <- reactive({
    
    dane <- final()
    
    nazwy <- colnames(dane)
    
    colnames(dane) <- c('wartosc', 'grupy')
    
    models <- dane %>% 
      dplyr::nest_by(grupy)  %>%
      dplyr::mutate(model = list(shapiro.test(data$wartosc)))
    
    wyniki <- models %>% dplyr::summarise(broom::tidy(model))
    
    
    return(wyniki)
    
  })
  
  output$test_nor <- renderTable(test_nor_wynik())
  
  normality_plot <- reactive({
    
    dane <- final()
    
    nazwy <- colnames(dane)
    
    colnames(dane) <- c('wartosc', 'grupy')
    
    p <- ggpubr::ggqqplot(dane, "wartosc")
    
    p <- p + ggplot2::facet_wrap(~grupy)+
      ggplot2::theme_bw()+
      ggplot2::theme(aspect.ratio = 1) # makes the plots to be always square
    
    print(p)
    
  })
  
  
  output$nor_plot <- renderPlot({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    print(normality_plot())
  })
  
  
  output$ttest <- renderTable({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    dane <- final()
    
    formula <- paste0(colnames(dane)[1], '~', colnames(dane)[2])
    
    ggpubr::compare_means(formula = as.formula(formula), dane, method = 't.test')
    
  }, 
  digits = -1)
  
  output$wtest <- renderTable({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    dane <- final()
    
    formula <- paste0(colnames(dane)[1], '~', colnames(dane)[2])
    
    ggpubr::compare_means(formula = as.formula(formula), dane)
  },
  digits = -1)
  
  output$anova1 <- renderPrint({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    dane <- final()
    
    formula <- paste0(colnames(dane)[1], '~', colnames(dane)[2])
    
    model <- stats::aov(formula = as.formula(formula), dane)
    
    summary(model)
    
  })
  
  output$anova2 <- renderPrint({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    dane <- final()
    
    if(input$posthoc == TRUE){
      
      formula <- paste0(colnames(dane)[1], '~', colnames(dane)[2])
      
      model <- stats::aov(formula = as.formula(formula), dane)
      
      stats::TukeyHSD(model)
      
    } else {
      
      dane <- final()
      
      dane <- na.omit(dane)
      dane <- as.data.frame(dane)
      
      nazwy <- colnames(dane)
      
      y <- dane[,1]
      x <- dane[,2]
      userfriendlyscience::oneway(y = y, x = x, posthoc = 'games-howell', digits = 3)
      
    }
  })
  
  output$anova_plot <- renderPlot({
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    if(input$posthoc == TRUE){
      
      envir <- environment()
      
      dane <- final()
      
      formula <- paste0(colnames(dane)[1], '~', colnames(dane)[2])
      
      model <- stats::aov(formula = as.formula(formula), dane)
      
      tukey <- stats::TukeyHSD(model)
      
      tabela <- as.data.frame(tukey[[1]])
      tabela$zmienna <- rownames(tabela)
      p <- ggplot2::ggplot(tabela, ggplot2::aes(x = zmienna, y = diff, ymin = lwr, ymax = upr), 
                           environment = envir)
      p <- p + ggplot2::geom_pointrange()+
        ggplot2::coord_flip()+
        ggplot2::geom_hline(yintercept = 0)+
        ggplot2::geom_errorbar(width = 0.2) + 
        ggplot2::theme_bw()+
        ggplot2::ggtitle('95 % przedział ufności')+
        ggplot2::ylab('Różnica średnich poziomach zmiennych') + 
        ggplot2::xlab('Zmienna')
      
      print(p)
    }
  })
  
  output$kolumna_scatter_x <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_scatter_x", "Wybierz zmienną dla osi X",
                choices = grupy, selected = grupy[1])
    
  })
  
  output$kolumna_scatter_y <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_scatter_y", "Wybierz zmienną dla osi Y",
                choices = grupy, selected = grupy[2])
    
  })
  
  output$kolumna_scatter_color <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_scatter_color", "Pokoloruj według",
                choices = c('brak', grupy), selected = 'brak')
    
  })
  
  output$kolumna_scatter_facet <- renderUI({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    if (input$format == TRUE){
      return(NULL)
    }
    
    dane <- dane()
    
    grupy <- colnames(dane)
    
    selectInput("kolumna_scatter_facet", "Podziel na panele według",
                choices = c('brak', grupy), selected = 'brak')
    
  })
  
  scatterInput <- reactive({
    
    wb <- final_scatter()
    
    nazwy <- colnames(wb)
    
    color <- input$kolumna_scatter_color
    facet <- input$kolumna_scatter_facet
    
    p <- draw_scatter(wb = wb,
                      x_var = nazwy[1],
                      y_var = nazwy[2],
                      color_var = color,
                      facet_var = facet,
                      trend = input$trend)
    
    return(p)
    
  })
  
  output$scatter <- renderPlot({
    if (is.null(input$dane) & is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    print(scatterInput())
  })
  
  output$scatter_test <- renderTable(head(final_scatter()))
  
  
  # observe ({
  # 
  #   inFile <- input$dane
  # 
  #   if (is.null(inFile)){
  #     return(NULL)
  #   } else {
  # 
  #   dane <- final()
  # 
  #   write.table(dane, "dane/dane.txt")
  #   }
  # })
  # 
  # 
  # output$downloadReport <- downloadHandler(
  #   filename = function() {
  #     paste('raport', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },
  # 
  #   content = function(file) {
  #     src <- normalizePath('raport_eda.Rmd')
  # 
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'raport_eda.Rmd')
  # 
  #     library(rmarkdown)
  #     out <- render('raport_eda.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ), encoding = "UTF-8")
  #     file.rename(out, file)
  #   }
  # )
  
  
  # additional UI elements
  
  # color
  
  output$color_scale <- renderUI({
    
    radioButtons('kolory_skala', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                 selected = 'domyślna', inline = TRUE)
    
    
  })
  
  
  output$color_brewer <- renderUI({
    if(input$kolory_skala == 'colorbrewer'){
      
      selectInput('brewer_kolory', label = 'Którą skalę Colorbrewer zastosować?', 
                  choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent'), 
                  selected = 'Set1', multiple = FALSE)
      
    } else {
      return(NULL)
    }
    
  })
  
  output$color_viridis <- renderUI({
    if(input$kolory_skala == 'viridis'){
      
      selectInput('viridis_kolory', label = 'Którą skalę viridis zastosować?', 
                  choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'), 
                  selected = 'viridis', multiple = FALSE)
      
    } else {
      return(NULL)
    }
    
  })
  
}
