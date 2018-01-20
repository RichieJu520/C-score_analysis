# Checkerboard-score-C-score-analysis

#install.packages("EcoSimR")
#install.packages("devEMF")

library(EcoSimR)    # load EcoSimR library
library(devEMF)
set.seed(56)        # for reproducible results

T_list <-c('N1W1.txt','Pos0.6-N1W1.txt','Neg0.6-N1W1.txt')

for (n in T_list){
  OTU_Abu <- read.table(n,header=T);
  OTU_Abu[OTU_Abu>0]<-1;
  
  #Filter out empty rows
  OTU_Abu.nonzerorow<-OTU_Abu[which(rowSums(OTU_Abu)>0),]
  OTU_Abu<-OTU_Abu.nonzerorow
  
  csModel <-cooc_null_model(OTU_Abu, algo = "sim9", metric = "c_score",
                            nReps = 30000, saveSeed = FALSE, burn_in = 500, algoOpts = list(),
                            metricOpts = list(), suppressProg = FALSE);
  n
  summary(csModel);
  
  write.table(n,"c-score.N1w1.xls",append = TRUE);
  sink("c-score.N1w1.xls", append = TRUE);
  summary(csModel);
  sink(NULL);
  
  emf(file = sprintf("%s.c-score.hist.30000.emf",n), width = 7, height = 7,
      bg = "transparent", fg = "black", pointsize = 12,
      family = "Helvetica", custom.lty = FALSE);
  plot(csModel,type = "hist");
  dev.off();
  
  emf(file = sprintf("%s.c-score.burnin.30000.emf",n), width = 2.1, height = 2.1,
      bg = "transparent", fg = "black", pointsize = 12,
      family = "Helvetica", custom.lty = FALSE);
  plot(csModel,type = "burn_in");
  dev.off()
}
