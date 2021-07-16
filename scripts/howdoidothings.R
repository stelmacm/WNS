#July 16th meeting 

#What I think I still have left to do:
# Redo CI's of spatial statistics
# Create nicer plots of the cliff and then link the plot to the discussion points
# Hypothesis testing of rho
# Calculate parameter bias + coverage
# Fix computational considerations subsection


#Extra stuff I think I might do but also might not do:
# Explain Nedler Meads?
# SBC if even possible
# Discuss more about random effects 

#Stuff I have mildly looked into and tried and gave up:
# R_0
# Markov Random Field

#Stuff BMB thinks I MUST include/change:



#Stuff BMB thinks would be cool to include:



#Questions on how to improve the following:
### NEW PLOT ###

paramsandoutputs <- read.csv("data/optimgridtesting.csv")
paramsandoutputs$Classification <- "Fit"
for (i in seq(1:nrow(paramsandoutputs))) {
  if(paramsandoutputs$optimll[i] > 1000){
    paramsandoutputs$Classification[i] <- "Acceptable"
  }
  if(paramsandoutputs$optimll[i] < 1000){
    paramsandoutputs$Classification[i] <- "Unacceptable"
  }
}
paramsandoutputs$Classification <- as.factor(paramsandoutputs$Classification)

optimresults <- (ggplot(data = paramsandoutputs)
                 + geom_point(aes(x = d, y = (optimll - min(optimll)),
                                  shape = Classification, colour = Classification), size = 3, alpha = 0.7) 
                 + theme_bw()
                 + labs(x = "Log(Delta)", y = "LL - min(LL)")
                 + theme(
                   axis.line.x = element_line(color = "black"),
                   axis.line.x.top = element_blank(),
                   axis.ticks.x = element_line(color = "black"),
                   axis.ticks.x.top = element_line(color = "gray50"),
                   #axis.title.x = element_text(hjust = 1),
                   #legend.position = "none",
                   axis.text.y.left  = element_text(angle = 90, size = 12,  hjust = 0.5, vjust = 1),
                   legend.position = c(0.95, .05),
                   legend.justification = c(1, 0),
                   legend.key.height = grid::unit(6, "pt"),
                   legend.key.width = grid::unit(30, "pt"),
                   legend.spacing.x = grid::unit(6, "pt"),
                   legend.spacing.y = grid::unit(3, "pt"),
                   legend.box.background = element_rect(fill = "white", color = "grey"),
                   legend.box.spacing = grid::unit(0, "pt"),
                   legend.title.align = 0.5
                 )
                 
)
optimresults

#I have no clue what I am doing..... Come back to this 


#So we have log delta on the x axis and NLL on y axis
#Lots of white space here which is what I dont like.....

showingoffbadparams <- (ggplot(data = paramsandoutputs) 
                        + geom_tile(aes(x = dseq, y = thetaseq, fill = optimll)) 
                        + facet_grid(rhoseq ~ offsetseq)
                        + scale_fill_viridis_c(trans = "log10", name = "Log Likelihood") 
                        + theme_bw()
                        + theme(
                          axis.line.x = element_line(color = "black"),
                          axis.line.x.top = element_blank(),
                          axis.ticks.x = element_line(color = "black"),
                          axis.ticks.x.top = element_line(color = "gray50"),
                          legend.box.background = element_rect(fill = "white", color = "grey"),
                          legend.title.align = 0.5
                        )
                        + labs(x = "Delta", y = "Theta")
)

showingoffbadparams
#What I would like to do here is change "0.01" and "0.99" to "no shared users" and "only SU"
#Maybe have y axis title on both sides? I like the idea of tile and having a small sample 
#representation of my problem
