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


draw_density <- function(){


}


draw_boxplot <- function(){



}



