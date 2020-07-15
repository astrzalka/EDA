#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
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
      return(d)
    }
    
    if(input$rodzaj_dane == 'przykład'){
      d <- przyklad
      return(d)
    }
  }) 
  
  
  final <- reactive ({
    
    if(input$format == FALSE){
      dane <- wybor(dane(), input$num1, input$num2)
    } else {
      dane_2 <- dane()
      
      dane_2 <- tidyr::gather(dane_2, rodzaj, wartosc)
      
      dane <- dane_2[,c(2,1)]
      
    }
    
    dane <- dane[dane[,2] %in% input$grupy,]
    
    
    return(dane)
  })
  
  output$grupy <- renderUI({
    
    if (is.null(input$dane)&is.null(input$dane_xls) & input$rodzaj_dane != 'przykład')
      return(NULL)
    
    if(input$format == TRUE){
      dane <- dane()
      
      dane <- tidyr::gather(dane, rodzaj, wartosc)
      
      dane <- dane[,c(2,1)]
    } else {
      dane <- wybor(dane(), input$num1, input$num2)
    }
    
    colnames(dane) <- c('wartosc', 'grupy')
    
    grupy <- unique(dane$grupy)
    
    checkboxGroupInput("grupy", label = ("Wybierz grupy do analizy"), 
                       choices = grupy,
                       selected = grupy)
    
  })
  
  
  output$contents <- renderTable({
    final()
  })
  
  histogramInput <- reactive({
    
    envir <- environment()
    
    wb <- final()
    
    nazwy <- colnames(wb)
    
    p <- ggplot2::ggplot(wb, environment = envir)
    
    facet = paste("~", nazwy[2])
    
    if ( as.logical(input$facet) == TRUE){
      if ( input$os_y == 2){
        p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = nazwy[1])), y = ..density..), binwidth = input$bin) + 
          ggplot2::facet_grid(facet)+ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])
      } else {
        p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = nazwy[1]))), binwidth = input$bin) + 
          ggplot2::facet_grid(facet)+ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])
      }
    } else {
      if ( input$os_y == 2){
        p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = nazwy[1])), y = ..density.., fill = as.factor(eval(parse(text = nazwy[2])))), 
                                         position = "dodge", binwidth = input$bin)+
          ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])#+ggplot2::scale_fill_discrete(nazwy[2])
      } else {
        p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = nazwy[1])), fill = as.factor(eval(parse(text = nazwy[2])))), position = "dodge", 
                                         binwidth = input$bin)+
          ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])#+ggplot2::scale_fill_discrete(nazwy[2])
      }
      
    }
    p <- p + ggplot2::xlab(input$os_x) + ggplot2::ylab(input$os_y_nazwa)
    
    if(input$kolory_hist == 'domyślna'){
      p <- p + ggplot2::scale_fill_discrete(name = nazwy[2])
    }
    
    if(input$kolory_hist == 'viridis'){
      p <- p + ggplot2::scale_fill_viridis_d(option = input$viridis_hist, end = 0.92, name = nazwy[2])
    }
    
    if(input$kolory_hist == 'colorbrewer'){
      p <- p + ggplot2::scale_fill_brewer(palette = input$colorbrewer_hist,  name = nazwy[2])
    }
    
    if(input$kolory_hist == 'odcienie szarości'){
      p <- p + ggplot2::scale_fill_grey( name = nazwy[2])
    }
    
    if(input$kolory_hist == 'własna :)'){
      
      my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory_hist, ',')))
      
      
      p <- p + ggplot2::scale_fill_manual(values = my_colors,  name = nazwy[2])
    }
    
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
    
    p <- ggplot2::ggplot(wb, environment = envir)
    
    p <- p + ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = nazwy[1])), color = as.factor(eval(parse(text = nazwy[2])))))+
      ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])
    
    if(input$fill_dens == TRUE){
      p <- p + ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = nazwy[1])), color = as.factor(eval(parse(text = nazwy[2]))),
                                                  fill = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.3)+
        ggplot2::theme_bw()+ggplot2::xlab(nazwy[1])
      
      if(input$kolory_dens == 'domyślna'){
        p <- p + ggplot2::scale_fill_discrete(name = nazwy[2])
      }
      if(input$kolory_dens == 'viridis'){
        p <- p + ggplot2::scale_fill_viridis_d(option = input$viridis_dens, end = 0.92, name = nazwy[2])
      }
      if(input$kolory_dens == 'colorbrewer'){
        p <- p + ggplot2::scale_fill_brewer(palette = input$colorbrewer_dens,  name = nazwy[2])
      }
      if(input$kolory_dens == 'odcienie szarości'){
        p <- p + ggplot2::scale_fill_grey( name = nazwy[2])
      }
      if(input$kolory_dens == 'własna :)'){
        my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory_dens, ',')))
        p <- p + ggplot2::scale_fill_manual(values = my_colors,  name = nazwy[2])
      }
    }
    
    if(input$kolory_dens == 'domyślna'){
      p <- p + ggplot2::scale_color_discrete(name = nazwy[2])
    }
    if(input$kolory_dens == 'viridis'){
      p <- p + ggplot2::scale_color_viridis_d(option = input$viridis_dens, end = 0.92, name = nazwy[2])
    }
    if(input$kolory_dens == 'colorbrewer'){
      p <- p + ggplot2::scale_color_brewer(palette = input$colorbrewer_dens,  name = nazwy[2])
    }
    if(input$kolory_dens == 'odcienie szarości'){
      p <- p + ggplot2::scale_color_grey( name = nazwy[2])
    }
    if(input$kolory_dens == 'własna :)'){
      my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory_dens, ',')))
      p <- p + ggplot2::scale_color_manual(values = my_colors,  name = nazwy[2])
    }
    
    p <- p + ggplot2::xlab(input$os_x_dens) + ggplot2::ylab(input$os_y_dens)
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
  
  # # update group and
  # # variables based on the data
  #   observe({
  #   #browser()
  #   if(!exists(input$dane)) return() #make sure upload exists
  #   dane <- final()
  #   var.opts<-unique(dane[,2])
  #   updateSelectInput(session, "kontrola", choices = var.opts)
  # })
  
  
  boxplotInput <- reactive({
    envir <- environment()
    
    wb <- final()
    wb <- as.data.frame(wb)
    nazwy <- colnames(wb)
    if(input$boxviolin == 'Boxplot'){
      if(input$p_format == 'p.adj'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
        }
      }
      
      if(input$p_format == 'p.signif'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            wb <- as.data.frame(wb)
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggboxplot(wb, x = nazwy[2], y = nazwy[1],
                                   color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                   xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
          }
          
        }
        
        #print(p)
      }
    }
    
    if(input$boxviolin == 'Violin'){
      if(input$p_format == 'p.adj'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
        }
      }
      
      if(input$p_format == 'p.signif'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            wb <- as.data.frame(wb)
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggviolin(wb, x = nazwy[2], y = nazwy[1],
                          color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                          xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggviolin(wb, x = nazwy[2], y = nazwy[1],
                                  color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                  xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
          }
          
        }
      }
      #print(p)
    }
    
    
    if(input$boxviolin == 'mean_ci'){
      #library(Hmisc)
      library(ggpubr)
      if(input$p_format == 'p.adj'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2],  add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2],  legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
        }
      }
      
      if(input$p_format == 'p.signif'){
        
        if(input$punkty %in% c('none', 'jitter')){
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            wb <- as.data.frame(wb)
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], add = input$punkty, legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
          }
        }
        if(input$punkty %in% c('beeswarm', 'quasirandom')){
          
          if(input$porownanie == 'brak'){
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            #print(p)
          }
          
          if(input$porownanie == 'kontrola'){
            grupy <- levels(as.factor(wb[,2]))
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, ref.group = grupy[input$kontrola])
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
            #print(p)
          }
          
          if(input$porownanie == 'grupy'){
            req(input$porownania)
            
            my_comparisons <- stringr::str_split(input$porownania, ';')
            my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
            
            p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = nazwy[2], y = nazwy[1],
                                     color = nazwy[2], legend.title = input$legenda_nazwa_box,
                                     xlab = input$os_x_box, ylab = input$os_y_box)
            p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                                method = input$rodzaj_test, comparisons = my_comparisons)
            if(input$punkty == 'beeswarm'){
              p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))), 
                                                              color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$punkty == 'quasirandom'){
              p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = nazwy[1])), x = as.factor(eval(parse(text = nazwy[2]))),
                                                                 color = as.factor(eval(parse(text = nazwy[2])))), alpha = 0.4)
            }
            if(input$anova != 'nie') {
              p <- p + ggpubr::stat_compare_means(method = input$anova, label.y = max(wb[,1]) *1.2)
            }
            
          }
          
        }
      }
      #print(p)
    }
    
    if(input$kolory == 'viridis'){
      p <- p + ggplot2::scale_color_viridis_d(option = input$viridis, end = 0.9)
    }
    
    if(input$kolory == 'colorbrewer'){
      p <- p + ggplot2::scale_color_brewer(palette = input$colorbrewer)
    }
    
    if(input$kolory == 'odcienie szarości'){
      p <- p + ggplot2::scale_color_grey()
    }
    
    if(input$kolory == 'własna :)'){
      
      my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory, ',')))
      
      
      p <- p + ggplot2::scale_color_manual(values = my_colors)
    }
    
    
    print(p)
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
