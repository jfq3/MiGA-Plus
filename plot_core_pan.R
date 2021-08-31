plot_core_pan <- function(a) {
  a.core <- a %>% 
    dplyr::select(genomes, core_avg, core_q1, core_q2, core_q3) %>% 
    dplyr::mutate(Genome = "Core") %>% 
    dplyr::rename(Average = core_avg,
                  q1 = core_q1,
                  Median = core_q2,
                  q3 = core_q3)
  
  a.pan <- a %>% 
    dplyr::select(genomes, pan_avg, pan_q1, pan_q2, pan_q3) %>% 
    dplyr::mutate(Genome = "Pan") %>% 
    dplyr::rename(Average = pan_avg,
                  q1 = pan_q1,
                  Median = pan_q2,
                  q3 = pan_q3)
  
  df <- rbind(a.core, a.pan) %>% 
    hablar::convert(fct(Genome)) %>% 
    pivot_longer(cols = c(Average, Median), names_to = "cntr")
  
  my.colors <- c("cadetblue1", "rosybrown2")
  
  plt <- ggplot() +
    geom_ribbon(data = df, aes(x=genomes, ymin = q1, ymax = q3, fill = Genome))  +
    geom_line(data = dplyr::filter(df, Genome == "Pan"),
              aes(x=genomes, y=value, linetype = cntr)) +
    geom_line(data =  dplyr::filter(df, Genome == "Core"),
              aes(x=genomes, y=value, linetype = cntr)) + 
    labs(x="Genomes", y="Orthologous Groups") +
    scale_fill_manual(values = my.colors, aesthetics = "fill") +
    theme_bw() +
    theme(legend.position = c(0.2, 0.9),
          legend.title = element_blank(),
          legend.box = "horizontal",
          panel.grid = element_blank())
  
  core.pan.list <- list(df = df, plt = plt)
  
  return(core.pan.list)
  
}
