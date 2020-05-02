require(ggplot2)

neat_theme =   theme(panel.background=element_rect(fill="white", color="black", size=1.1), 
                      axis.text=element_text(size=rel(1), color="black"), 
                      strip.text=element_text(size=rel(1.1)), 
                      legend.text=element_text(size=rel(1.1)), strip.background=element_blank(),
                      title=element_text(size=rel(1.1)),
                      panel.grid=element_blank(),
                      plot.title=element_text(hjust=0.5))

rotateX = theme(axis.text.x=element_text(angle=90))

adjX = theme(axis.text.x=element_text(vjust=0.5))


### THIS FUNCTION COURTESY
### KEVIN USHEY
### https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}