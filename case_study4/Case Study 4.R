library(tswge)


# use ILINET

ilinet = read.csv("/Users/kaileyayala/Desktop/QTW/FluViewPhase2Data/ILINet.csv")

head(ilinet)


total_ili = ilinet$ILITOTAL

total_ili

# probably seasonality of 52 b/c weekly data
plotts.wge(total_ili)

flu = total_ili[600:1200]
flu = flu[1:586]
flu_2y = flu[483:586]
plotts.wge(flu)
plotts.wge(flu_2y)

#length(flu_2y)
#which(is.na(flu))

flu_dif_s1 = artrans.wge(flu, phi.tr = (c(rep(0,103), 1))) # takes out 104 week seasonality 
flu_dif_s2 = artrans.wge(flu_dif_s1, phi.tr = (c(rep(0,51), 1))) # takes out 52 week seasonality 
flu_dif_s3 = artrans.wge(flu_dif_s2, phi.tr = (c(rep(0,11), 1))) # takes out 12 week seasonality 
flu_dif_s4 = artrans.wge(flu_dif_s3, phi.tr = (c(rep(0,21), 1))) # takes out 22 week seasonality 
flu_dif_s5 = artrans.wge(flu_dif_s4, phi.tr = (c(rep(0,4), 1))) # takes out 5 week seasonality 

parzen.wge(flu_dif_s5)

flu_dif_d_s = artrans.wge(flu_dif_s5, phi.tr = (1)) # takes first difference 

plotts.sample.wge(flu_dif_d_s)
#parz = parzen.wge(flu_dif_d_s) #investiage periodicity 
#finding frequency of biggest parzen peak
#which(parz$pzgram > 4)
#parz$pzgram[34:40]
#1/parz$freq[35]

# modeling arma components
aic5.wge(flu_dif_d_s)

arma_resid = est.arma.wge(flu_dif_d_s, p=4, q=2, factor=T) # models with arma(4,2)

parz2 = plotts.sample.wge(arma_resid$res) # still shows minor periodicity to visual inspection. 

ljung.wge(arma_resid$res) #0
ljung.wge(arma_resid$res, K=48) #0

#finding frequency of biggest parzen peak
#which(parz2$dbz > 3.4)

#parz2$dbz[81:83]
#parz2$freq[82]
#1/parz2$freq[82] # 5 week period




