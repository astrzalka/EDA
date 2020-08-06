wybor <- function(dane, num1, num2){
  d <- dane[,c(num1, num2)]
  return(d)
}


#' Title
#'
#' Draws a ggplot histogram
#'
#' @param wb dataframe
#' @param variable name for x
#' @param facet_draw draw facets (logical)
#' @param facet_var name for facet/colorscale
#' @param bin bin width
#' @param y_density density on y scale?
#' @param x_name x label
#' @param y_name y label
#' @param kolory choose colour scale
#' @param viridis choose viridis scale
#' @param brewer choose colorbrewer scale
#' @param wlasne choose custom scale
#'
#' @return ggplot
#' @export
#'
#' @examples
draw_histogram <- function(wb, variable, facet_draw = TRUE, facet_var, bin = 1, y_density = 1, x_name = 'x', y_name = 'y', kolory = 'domyślna',
                           viridis = 'magma', brewer = 'Set1', wlasne){
  
  
  p <- ggplot2::ggplot(wb)
  
  facet = paste("~", facet_var)
  
  if (facet_draw == TRUE){
    if ( y_density == 2){
      p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = variable)), y = ..density..), binwidth = bin) +
        ggplot2::facet_wrap(facet)+ggplot2::theme_bw()+ggplot2::xlab(variable)
    } else {
      p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = variable))), binwidth = bin) +
        ggplot2::facet_wrap(facet)+ggplot2::theme_bw()+ggplot2::xlab(variable)
    }
  } else {
    if (y_density == 2){
      p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = variable)), y = ..density.., fill = as.factor(eval(parse(text = facet_var)))),
                                       position = "dodge", binwidth = bin)+
        ggplot2::theme_bw()+ggplot2::xlab(variable)
    } else {
      p <- p + ggplot2::geom_histogram(ggplot2::aes(x = eval(parse(text = variable)), fill = as.factor(eval(parse(text = facet_var)))), position = "dodge",
                                       binwidth = bin)+
        ggplot2::theme_bw()+ggplot2::xlab(variable)
    }
    
  }
  p <- p + ggplot2::xlab(x_name) + ggplot2::ylab(y_name)
  
  if(kolory == 'domyślna'){
    p <- p + ggplot2::scale_fill_discrete(name = facet_var)
  }
  
  if(kolory == 'viridis'){
    p <- p + ggplot2::scale_fill_viridis_d(option = viridis, end = 0.92, name = facet_var)
  }
  
  if(kolory == 'colorbrewer'){
    p <- p + ggplot2::scale_fill_brewer(palette = brewer,  name = facet_var)
  }
  
  if(kolory == 'odcienie szarości'){
    p <- p + ggplot2::scale_fill_grey( name = facet_var)
  }
  
  if(kolory == 'własna :)'){
    
    my_colors <- sub(' ', '', unlist(stringr::str_split(wlasne, ',')))
    
    
    p <- p + ggplot2::scale_fill_manual(values = my_colors,  name = facet_var)
  }
  
  return(p)
}


#' Title
#'
#' @param wb dataframe
#' @param variable name for x
#' @param color_var name for facet/colorscale
#' @param fill add fill to density curves?
#' @param x_name x label
#' @param y_name y label
#' @param kolory choose colour scale
#' @param viridis choose viridis scale
#' @param brewer choose colorbrewer scale
#' @param wlasne choose custom scale
#'
#' @return ggplot 
#' @export
#'
#' @examples
draw_density <- function(wb, variable, color_var, fill = FALSE, x_name = 'x', y_name = 'y', 
                         kolory = 'domyślna',
                         viridis = 'magma', brewer = 'Set1', wlasne){
  
  p <- ggplot2::ggplot(wb)
  
  p <- p + ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = variable)), color = as.factor(eval(parse(text = color_var)))))+
    ggplot2::theme_bw()+ggplot2::xlab(variable)
  
  if(fill == TRUE){
    p <- p + ggplot2::geom_density(ggplot2::aes(x = eval(parse(text = variable)), color = as.factor(eval(parse(text = color_var))),
                                                fill = as.factor(eval(parse(text = color_var)))), alpha = 0.3)+
      ggplot2::theme_bw()+ggplot2::xlab(variable)
    
    if(kolory == 'domyślna'){
      p <- p + ggplot2::scale_fill_discrete(name = color_var)
    }
    if(kolory == 'viridis'){
      p <- p + ggplot2::scale_fill_viridis_d(option = viridis, end = 0.92, name = color_var)
    }
    if(kolory == 'colorbrewer'){
      p <- p + ggplot2::scale_fill_brewer(palette = brewer,  name = color_var)
    }
    if(kolory == 'odcienie szarości'){
      p <- p + ggplot2::scale_fill_grey( name = color_var)
    }
    if(kolory == 'własna :)'){
      my_colors <- sub(' ', '', unlist(stringr::str_split(wlasne, ',')))
      p <- p + ggplot2::scale_fill_manual(values = my_colors,  name = color_var)
    }
  }
  
  if(kolory == 'domyślna'){
    p <- p + ggplot2::scale_color_discrete(name = color_var)
  }
  if(kolory == 'viridis'){
    p <- p + ggplot2::scale_color_viridis_d(option = viridis, end = 0.92, name = color_var)
  }
  if(kolory == 'colorbrewer'){
    p <- p + ggplot2::scale_color_brewer(palette = brewer,  name = color_var)
  }
  if(kolory == 'odcienie szarości'){
    p <- p + ggplot2::scale_color_grey( name = color_var)
  }
  if(kolory == 'własna :)'){
    my_colors <- sub(' ', '', unlist(stringr::str_split(wlasne, ',')))
    p <- p + ggplot2::scale_color_manual(values = my_colors,  name = color_var)
  }
  
  p <- p + ggplot2::xlab(x_name) + ggplot2::ylab(y_name)
  return(p)
}


#' Title
#'
#'Draws boxplot, violin of mean_ci plot based upon ggpubr package
#'
#' @param wb dataframe
#' @param x_var variable for x axis
#' @param y_var variable for y axis
#' @param type plot type
#' @param p_format format for p value
#' @param porownanie comparison type
#' @param punkty draw points? based upon ggbeeswarm package
#' @param anova show anova result?
#' @param test_type which statistical test should be used?
#' @param konrola which group is the control group?
#' @param grupy_porownania which groups should be compared 
#' @param x_name x axis title
#' @param y_name y axis title
#' @param kolory color scale type
#' @param viridis viridis scale type
#' @param brewer colorbrewer palette
#' @param wlasne custom colors
#'
#' @return ggplot
#' @export
#'
#' @examples
draw_boxplot <- function(wb, 
                         x_var, 
                         y_var,
                         type = 'Boxplot', 
                         p_format = 'p.adj', 
                         porownanie = 'brak',
                         punkty = 'none', 
                         anova = 'nie', 
                         test_type = 't.test', 
                         kontrola = 1,
                         grupy_porownania = '',
                         x_name = 'x', 
                         y_name = 'y', 
                         kolory = 'domyślna',
                         viridis = 'magma', 
                         brewer = 'Set1', 
                         wlasne){
  
  wb <- wb[,c(y_var, x_var)]
  
  if(type == 'Boxplot'){
    if(p_format == 'p.adj'){
      
      if(porownanie == 'brak'){
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,
                               xlab = x_name, ylab = y_name)
        
        
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,  
                               xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                            method = test_type, ref.group = grupy[kontrola])
        
        
      }
      
      if(porownanie == 'grupy'){
        
        
        my_comparisons <- stringr::str_split(grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,  
                               xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
    }
    
    if(p_format == 'p.signif'){
      
      if(porownanie == 'brak'){
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,  
                               xlab = x_name, ylab = y_name)
        
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,  
                               xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, ref.group = grupy[kontrola])
        
        
      }
      
      if(porownanie == 'grupy'){
        
        
        my_comparisons <- stringr::str_split( grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggboxplot(wb, x = x_var, y = y_var,
                               color = x_var,  
                               xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
      
      
    }
  }
  
  if(type == 'Violin'){
    if(p_format == 'p.adj'){
      if(porownanie == 'brak'){
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                            method = test_type, ref.group = grupy[kontrola])
        
      }
      
      if(porownanie == 'grupy'){
        
        
        my_comparisons <- stringr::str_split( grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
    }
    
    if(p_format == 'p.signif'){
      
      
      if(porownanie == 'brak'){
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, ref.group = grupy[kontrola])
        
      }
      
      if(porownanie == 'grupy'){
        
        
        my_comparisons <- stringr::str_split( grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggviolin(wb, x = x_var, y = y_var,
                              color = x_var,
                              xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
    }
  }
  
  if(type == 'mean_ci'){
    
    
    if(p_format == 'p.adj'){
      
      if(porownanie == 'brak'){
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,
                                 xlab = x_name, ylab = y_name)
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,  
                                 xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.adj..),
                                            method = test_type, ref.group = grupy[kontrola])
        
      }
      
      if(porownanie == 'grupy'){
        
        my_comparisons <- stringr::str_split( grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,
                                 xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.format..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
      
    }
    
    
    if(p_format == 'p.signif'){
      
      if(porownanie == 'brak'){
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,
                                 xlab = x_name, ylab = y_name)
        
      }
      
      if(porownanie == 'kontrola'){
        grupy <- levels(as.factor(wb[,2]))
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,
                                 xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, ref.group = grupy[kontrola])
        
        
      }
      
      if(porownanie == 'grupy'){
        
        
        my_comparisons <- stringr::str_split( grupy_porownania, ';')
        my_comparisons <- stringr::str_split(unlist(my_comparisons), ' ')
        
        p <- ggpubr::ggerrorplot(wb, error.plot = 'crossbar', desc_stat = 'mean_ci', x = x_var, y = y_var,
                                 color = x_var,
                                 xlab = x_name, ylab = y_name)
        p <- p + ggpubr::stat_compare_means(ggplot2::aes(label = ..p.signif..),
                                            method = test_type, comparisons = my_comparisons)
        
      }
      
    }
  }
  
  if(punkty == 'beeswarm'){
    p <- p + ggbeeswarm::geom_beeswarm(ggplot2::aes(y = eval(parse(text = y_var)), 
                                                    x = as.factor(eval(parse(text = x_var))), 
                                                    color = as.factor(eval(parse(text = x_var)))), 
                                       alpha = 0.4)
  }
  if(punkty == 'quasirandom'){
    p <- p + ggbeeswarm::geom_quasirandom(ggplot2::aes(y = eval(parse(text = y_var)), 
                                                       x = as.factor(eval(parse(text = x_var))),
                                                       color = as.factor(eval(parse(text = x_var)))), 
                                          alpha = 0.4)
  }
  
  
  if(anova != 'nie') {
    p <- p + ggpubr::stat_compare_means(method = anova, label.y = max(wb[,1]) *1.2)
  }
  
  if(kolory == 'viridis'){
    p <- p + ggplot2::scale_color_viridis_d(option = viridis, end = 0.9)
  }
  
  if(kolory == 'colorbrewer'){
    p <- p + ggplot2::scale_color_brewer(palette = brewer)
  }
  
  if(kolory == 'odcienie szarości'){
    p <- p + ggplot2::scale_color_grey()
  }
  
  if(kolory == 'własna :)'){
    
    my_colors <- sub(' ', '', unlist(stringr::str_split(wlasne, ',')))
    
    
    p <- p + ggplot2::scale_color_manual(values = my_colors)
  }
  
  
  return(p)
  
  
}


#' Title
#'
#' @param wb data frame
#' @param x_var x axis variable
#' @param y_var y axis variable
#' @param color_var color variable
#' @param facet_var facet_wrap variable
#' @param trend add trendline?
#' @param size_trend size of trend line
#' @param model model, option: 'loess', 'lm'
#' @param span span of loess
#' @param se add error?
#' @param alpha alpha of points
#' @param size_point size of points
#' @param kolory color type
#' @param viridis viridis scale
#' @param brewer colorbrewer scale
#' @param wlasne custom colors
#'
#' @return ggplot
#' @export
#'
#' @examples
draw_scatter <- function(wb, x_var, y_var, color_var = 'brak', facet_var = 'brak', 
                         trend = FALSE,
                         size_trend = 2, 
                         model = 'loess', 
                         span = 0.75, 
                         se = TRUE, 
                         alpha = 1, 
                         size_point = 1.5,
                         kolory = 'domyślna',
                         viridis = 'magma', 
                         brewer = 'Set1', 
                         wlasne){
  
  
  if(color_var == 'brak'){
    p <- ggplot2::ggplot(wb, ggplot2::aes(x = eval(parse(text = x_var)), y = eval(parse(text = y_var))))
  } else {
    p <- ggplot2::ggplot(wb, ggplot2::aes(x = eval(parse(text = x_var)), 
                                          y = eval(parse(text = y_var)),
                                          color = eval(parse(text = color_var))))
    
  }
  
  p <- p + ggplot2::geom_point(alpha = alpha, size = size_point)
  
  if(facet_var != 'brak'){
    
    facet <- paste0('~', facet_var)
    
    p <- p + ggplot2::facet_wrap(eval(parse(text = facet)))
    
  }
  
  if(trend){
    p <- p + ggplot2::geom_smooth(size = size_trend,
                                  method = model,
                                  span = span,
                                  se = se)
  }
  
  numer_color_var <- which(colnames(wb) == color_var)
  if(kolory == 'viridis'){
    if(is.numeric(wb[,numer_color_var])){
      p <- p + ggplot2::scale_color_viridis_c(option = viridis, end = 0.9, name = color_var)
    } else {
      p <- p + ggplot2::scale_color_viridis_d(option = viridis, end = 0.9, name = color_var)
    }
  }
  
  if(kolory == 'colorbrewer'){
    if(is.numeric(wb[,numer_color_var])){
      p <- p + ggplot2::scale_color_distiller(palette = brewer, name = color_var)
    } else {
      p <- p + ggplot2::scale_color_brewer(palette = brewer, name = color_var)
    }
  }
  
  if(kolory == 'odcienie szarości'){
    p <- p + ggplot2::scale_color_grey()
  }
  
  if(kolory == 'własna :)'){
    
    my_colors <- sub(' ', '', unlist(stringr::str_split(wlasne, ',')))
    
    
    p <- p + ggplot2::scale_color_manual(values = my_colors, name = color_var)
  }
  
  if(kolory == 'domyślna'){
    if(is.numeric(wb[,numer_color_var])){
      p <- p + ggplot2::scale_color_gradient(name = color_var)
    } else {
      p <- p + ggplot2::scale_color_discrete(name = color_var)
    }

  }
  
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::xlab(x_var) +
    ggplot2::ylab(y_var)
  
  return(p)
}


