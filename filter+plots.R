rm(list=ls())
identities <-  read.csv("geneious-identities.csv", header=T)

# FILTER CRITERIA

# Filter out nucleotides that didn't a) have full coverage, b) were identical in alpine, c) non-identical in Panthera, and d) more than half identical overall (i.e. not too variable):
data <- dplyr::filter(identities, pantheraCoverage == 5 & alpineCoverage == 3 & alpineIdentity == 1 & pantheraIdentity != 1 & nineSeqIdentity < 0.5 & product != "other")

# PLOTS

# Generate dot plot of nucleotide position vs panthera identity:
library(ggplot2)
p0 <- ggplot(data, aes(position, pantheraIdentity)) + 
  geom_point() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Nucleotide Position") + 
  ylab("% Identity (Panthera)")

library(ggpubr)
ggsave("posVsPanthIdentity.png", plot = p0)

# Generate barplot of gene family vs sequence length:
cols1 <- c('red','orange','yellow','green','blue','violet')
p1 <- ggplot(data.frame(data), aes(x=product, fill=geneFamily)) +
  geom_bar() + 
  theme(legend.position="bottom", panel.background = element_rect(fill='white', colour='black'), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values=cols1) + 
  xlab("") + ylab("# Nucleotides") + labs(fill="Gene Family")

# Generate barplot of gene family vs number of nucleotides found:
p2 <- ggplot(data.frame(data), aes(x=product, y=geneLength, fill=geneFamily)) +
  geom_bar(stat='identity') +  
  theme(legend.position="none", panel.background = element_rect(fill='white', colour='black'), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values=cols1) +
  xlab("") + ylab("Gene Length") + labs(fill="")

library(ggpubr)
p3 <- ggarrange(p2 + rremove("x.text"), p1, 
                heights = c(4, 8.5),
                labels = c("A", "B"),
                ncol = 1, nrow = 2, align = "v")
ggsave("nucleotidesVlength.png", plot = p3)


# Generate plot of gene family vs nucleotides/geneLength:
data$NTs <- table(data$product)[data$product] # get new row with number of nucleotides for each gene product:

p4 <- ggplot(data.frame(data), aes(x=product, y=(NTs/geneLength), fill=geneFamily)) +
  geom_bar(stat='identity') +  
  theme(legend.position="bottom", panel.background = element_rect(fill='white', colour='black'), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values=cols1) +
  xlab("") + ylab("# Nucleotides / Gene Length") + labs(fill="Gene Family")

ggsave("nucleotides-dividedby-length.png", plot = p4)