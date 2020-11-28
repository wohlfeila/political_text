vectorIsEmpty <- function(x) return(length(x) == 0)

#' Custom ggplot theme
#'
#' @param  none
#' @return ggplot() + theme()
#' @examples
#' customTheme()
#' @export
customTheme <- function(base_size = 14,
                        base_family = "Times New Roman",
                        base_line_size = base_size / 170,
                        base_rect_size = base_size / 170) {
  ggplot2::theme_minimal(base_size = base_size,
                         base_family = base_family,
                         base_line_size = base_line_size) +
    ggplot2::theme(
      plot.title = element_text(
        color = randompersonalthings::customPalette("blue"),
        face = "bold",
        hjust = 0),
      plot.subtitle = element_text(
        color = randompersonalthings::customPalette("blue"),
        hjust = 0),
      plot.caption = element_text(
        color = randompersonalthings::customPalette("blue"),
        face = "italic",
        hjust = 1,
        size = rel(.6)),
      axis.title = element_text(
        color = randompersonalthings::customPalette("blue"),
        size = rel(0.85)),
      axis.text = element_text(
        color = randompersonalthings::customPalette("blue"),
        size = rel(0.75)),
      panel.grid.major = element_line(
        color = randompersonalthings::customPalette("blue"),
        linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.title = element_text(size = rel(.75)),
      legend.text = element_text(size = rel(.7)),
      legend.background = element_rect(fill = randompersonalthings::customPalette("blue"),
                                       color = randompersonalthings::customPalette("blue")),
      
      axis.line = element_line(color = randompersonalthings::customPalette("blue")),
      
      complete = TRUE
      
    )
  
}


top25WorPlot <- function(df, filter, color){
  df %>% 
    filter(ideology == filter) %>% 
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity", fill = color) + 
    coord_flip() +
    labs(x = "", y = "Number of Words", title = paste0("Top 25 Words from ", filter)) +
    theme_classic() +
    customTheme()
}



# counts number of classes in a list and turns it into a matrix
tabList <- function(vects) {
  
  lev <- sort(unique(unlist(vects)))
  
  dat <- do.call(rbind, lapply(vects, function(x, lev){
    
    tabulate(factor(x, levels = lev, ordered = TRUE),
             
             nbins = length(lev))}, lev = lev))
  
  colnames(dat) <- sort(lev)
  
  data.frame(dat, check.names = FALSE)
  
}



proportionPlotFunction <- function(df, comparison_blog, ylab_title){
  
  comparison_blog <- enquo(comparison_blog)
  
  df %>% 
    ggplot(aes(x = proportion, y = !!comparison_blog, color = abs(!!comparison_blog - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.00001), low = "red", high = "blue") +
    labs(y = ylab_title, x = NULL, subtitle = "Reference Blog: Politico") +
    theme_classic() +
    customTheme() +
    theme(legend.position="none") 
}


wordFilterFunction <- function(word){
    # removes any words with numeric digits
    !str_detect(word, pattern = "[[:digit:]]") 
    # removes any remaining punctuations
    !str_detect(word, pattern = "[[:punct:]]") 
    # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "(.)\\1{2,}") 
    # removes any remaining single letter words
    !str_detect(word, pattern = "\\b(.)\\b")    
}

cleanTextFunction <- function(text_clean){
  x <- gsub("#","", text_clean) 
  x <- gsub("[a-zA-Z]-[1-9]\\w+", "<highway>", x) 
  x <- gsub(" W ", "<west>" , x) 
  x <- gsub(" E ", "<east>" , x) 
  x <- gsub(" S ", "<south>" , x) 
  x <- gsub(" N ", "<north>" , x) 
  x <- gsub("@\\w+", "<twitter handle>", x) 
  x <- gsub("(http[^ ]*)", "", x)
  x <- tolower(x)
  
  return(x)
}

cleanSpeeches <- function(text){
  tmp <- paste0(text, collapse = "")
  tmp <- gsub("\n", "", tmp)
  tmp <- gsub("<.*?>", "", tmp)
  tmp <- tolower(tmp)
  tmp <- trimws(tmp)
  
  return(tmp)
}

