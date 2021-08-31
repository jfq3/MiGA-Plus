a <- read.table("./extra_data/cattle-miga-project.core-pan.tsv", sep="\t", header=TRUE)
plt <- plot_core_pan(a)
plt
plt$plt + ggtitle("Cattle Genomes")

a <- read.table("./extra_data/clinical-miga-project.core-pan.tsv", sep="\t", header=TRUE)
plt <- plot_core_pan(a)
plt + ggtitle("Clinical Genomes")

core_pan <- "./data/10.clades/03.ogs/miga-project.core-pan.tsv"
a <- read.table(core_pan, sep="\t", header=TRUE)
plt <- plot_core_pan(a)
plt + ggtitle("Dehalococoides Genomes")


a <- read.table("./extra_data/cattle-miga-project.core-pan.tsv", sep="\t", header=TRUE)
head(a)
a <- dplyr::filter(a, genomes <=  10)
plt <- plot_core_pan(a)
plt + ggtitle("Cattle Genomes")


a <- read.table("./extra_data/cattle-miga-project.core-pan.tsv", sep="\t", header=TRUE)
rslt.cattle <- plot_core_pan(a)
cattle.df <- rslt.cattle$df %>% 
  dplyr::filter(genomes <= 10) %>% 
  mutate(source = "Cattle")
cattle.df

a <- read.table("./extra_data/clinical-miga-project.core-pan.tsv", sep="\t", header=TRUE)
rslt.cllinical <- plot_core_pan(a)
df.clinical <- rslt.cllinical$df %>% 
  dplyr::filter(genomes <= 10) %>% 
  mutate(source = "Clinical")
df.clinical

core_pan <- "./data/10.clades/03.ogs/miga-project.core-pan.tsv"
a <- read.table(core_pan, sep="\t", header=TRUE)
dehalo <- plot_core_pan(a)
df.dehalo <- dehalo$df %>% 
  mutate(source = "Dehalococcoides")
df.dehalo

df <- rbind(cattle.df, df.clinical, df.dehalo) %>% 
  dplyr::filter(genomes <= 10)

plt <- ggplot() +
  # scale_x_continuous(breaks = seq(0,max(df$genomes)*1.05,2)) +
  # scale_y_continuous(breaks = seq(0,max(df$q3)*1.05,500),limits = c(0,max(a$pan_q3)*1.05)) +
  geom_ribbon(data = df, aes(x=genomes, ymin = q1, ymax = q3, fill = Genome))  +
  geom_line(data = dplyr::filter(df, Genome == "Pan"),
            aes(x=genomes, y=value, linetype = cntr)) +
  geom_line(data =  dplyr::filter(df, Genome == "Core"),
            aes(x=genomes, y=value, linetype = cntr)) + 
  labs(x="Genomes", y="Orthologous Groups") +
  scale_fill_manual(values = my.colors, aesthetics = "fill") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank()) +
  facet_grid(~source)
plt

##################
# More concise method of creating data to plot
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

plt <- ggplot() +
  # scale_x_continuous(breaks = seq(0,max(df$genomes)*1.05,2)) +
  # scale_y_continuous(breaks = seq(0,max(df$q3)*1.05,500),limits = c(0,max(a$pan_q3)*1.05)) +
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
plt
