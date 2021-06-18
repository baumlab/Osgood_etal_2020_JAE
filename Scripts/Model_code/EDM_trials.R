
##A bunch of code from online for trying out EDM 


setwd("~/Documents/git_repos/Cocos_El_Nino/")

cocos<-read.csv("Data/Cocos_Final.csv", header=TRUE, stringsAsFactors = FALSE) #  read in the data updated to Aug 2019

cocos<-subset(cocos, Hammerheads>=0) #remove -1's which indicate presence only

library(rEDM) #https://rdrr.io/cran/rEDM/f/vignettes/rEDM.Rmd


cocos_sub<-subset(cocos, Date>"1997-04-30" & Date<"2004-02-01" & Hammerheads>=0)

cocos_sub2<-subset(cocos, Hammerheads>=0)

cocos_sub3<-subset(cocos, Hammerheads>=0 & EagleRays>=0)
#for subset

trend<-glm.nb(Hammerheads~Year, data=cocos_sub)


cocos_edm<-data.frame(Year=cocos_sub$Year, Month=cocos_sub$Month, Hammerheads=trend$residuals)

cocos_edm_mean <- merge(data.frame(Year=rep(1997:2004, rep(12,8)), Month = rep(1:12,8)), 
                        aggregate(Hammerheads~Year+Month, cocos_edm, mean), all.x = T)

temp_edm_mean <- merge(data.frame(Year=rep(1997:2004, rep(12,8)), Month = rep(1:12, 8)), 
                       aggregate(ONI~Year+Month, cocos_sub, mean), all.x = T)

#for full
trend<-glm.nb(Hammerheads~Year, data=cocos_sub3)

cocos_edm<-data.frame(Year=cocos_sub3$Year, Month=cocos_sub3$Month, Hammerheads=trend$residuals)

cocos_edm_mean <- merge(data.frame(Year=rep(1994:2019, rep(12,26)), Month = rep(1:12, 26)), 
           aggregate(Hammerheads~Year+Month, cocos_edm, mean), all.x = T)

trendeagle<-glm.nb(EagleRays~Year, data=cocos_sub3)

eagle_edm<-data.frame(Year=cocos_sub3$Year, Month=cocos_sub3$Month, EagleRays=trendeagle$residuals)

eagle_edm_mean <- merge(data.frame(Year=rep(1994:2019, rep(12,26)), Month = rep(1:12, 26)), 
                        aggregate(EagleRays~Year+Month, eagle_edm, mean), all.x = T)

temp_edm_mean <- merge(data.frame(Year=rep(1994:2019, rep(12,26)), Month = rep(1:12, 26)), 
           aggregate(ONI~Year+Month, cocos_sub3, mean), all.x = T)

#for subset
cocos_edm_mean <- cocos_edm_mean[-c(1:4, 86:96),]
temp_edm_mean <- temp_edm_mean[-c(1:4, 86:96),]

#for full
cocos_edm_mean <- cocos_edm_mean[-c(310:312),]
temp_edm_mean <- temp_edm_mean[-c(310:312),]
eagle_edm_mean <- eagle_edm_mean[-c(310:312),]

#make data frame for edm
for_edm<-data.frame(Hammerheads=scale(cocos_edm_mean$Hammerheads),
                    SST=temp_edm_mean$ONI)

for_edm<-data.frame(Hammerheads=scale(cocos_edm_mean$Hammerheads), EagleRays=scale(eagle_edm_mean$EagleRays))

#selecting embedding dimension

ts <- for_edm$Hammerheads
lib <- c(1, 100)
pred <- c(101, 309)

lib <- c(1, 25)
pred <- c(26, 81)

simplex_output <- simplex(ts, lib, pred, E=1:20)


par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
    ylab = "Forecast Skill (rho)")

smap_output <- s_map(ts, lib, pred, E = 4)

par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
    ylab = "Forecast Skill (rho)")

##
###is the lag negative, implying causality?
vars <- names(for_edm)

# generate all combinations of lib_column, target_column, tp
params <- expand.grid(lib_column = vars,
                      target_column = vars,
                      tp = -30:30)

# throw out cases where lib == target
params <- params[params$lib_column != params$target_column, ]
params$E <- c(10,3)

#params$E <- 3

output <- do.call(rbind, lapply(seq_len(NROW(params)), function(i) {
    ccm(for_edm, E = params$E[i],
        lib_sizes = NROW(for_edm), random_libs = FALSE,
        lib_column = params$lib_column[i],
        target_column = params$target_column[i],
        tp = params$tp[i], silent = TRUE)
}))

output$direction <- paste(output$lib_column, "xmap to\n", output$target_column)


time_delay_ccm_fig <- ggplot(output, aes(x = tp, y = rho, color = direction)) +
    geom_line() + theme_bw()
print(time_delay_ccm_fig)

#rho vs library size
SST_xmap_hammerhead <- ccm(for_edm, E = 10, lib_column = "SST", 
    target_column = "Hammerheads", lib_sizes = seq(2,NROW(for_edm),10), 
    num_samples = 100, random_libs = TRUE, replace = TRUE,
    silent = TRUE, tp=(0))

Hammerhead_xmap_SST <- ccm(for_edm, E = 10, lib_column = "Hammerheads", target_column = "SST", 
    lib_sizes = seq(2,NROW(for_edm),10), num_samples = 100, 
    random_libs = TRUE, replace = TRUE, silent = TRUE,
    tp=(0))

Eagle_xmap_hammerhead <- ccm(for_edm, E = 10, lib_column = "EagleRays", 
                           target_column = "Hammerheads", lib_sizes = seq(2,NROW(for_edm),10), 
                           num_samples = 100, random_libs = TRUE, replace = TRUE,
                           silent = TRUE, tp=(0))

Hammerhead_xmap_Eagle <- ccm(for_edm, E = 10, lib_column = "Hammerheads", target_column = "EagleRays", 
                           lib_sizes = seq(2,NROW(for_edm),10), num_samples = 100, 
                           random_libs = TRUE, replace = TRUE, silent = TRUE,
                           tp=(0))

t_xmap_h_means <- ccm_means(Eagle_xmap_hammerhead)
h_xmap_t_means <- ccm_means(Hammerhead_xmap_Eagle)


par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(t_xmap_h_means$lib_size, pmax(0, t_xmap_h_means$rho), type = "l", col = "red", 
    xlab = "Library Size", ylab = "Cross Map Skill (rho)",
    ylim=c(0,1))
lines(h_xmap_t_means$lib_size, pmax(0, h_xmap_t_means$rho), col = "blue")
legend(x = "topleft", legend = c("Eagle xmap ham", "ham xmap Eagle"), col = c("red", 
    "blue"), lwd = 1, inset = 0.02, cex = 0.8)

#rho
model_out<- ccm(for_edm, E = 10, lib_column = "Hammerheads", target_column = "SST", 
       lib_sizes = NROW(for_edm), tp=(-13), random_libs = FALSE)

model_out$rho


model_out2<- ccm(for_edm, E = 10, lib_column = "SST", 
                 target_column = "Hammerheads", 
                lib_sizes = NROW(for_edm), tp=(-1), random_libs = FALSE)

model_out2$rho

num_surr <- 1000
surr_SST <- make_surrogate_data(for_edm$SST, method = "random_shuffle", num_surr = num_surr)
surr_ham <- make_surrogate_data(for_edm$Hammerheads, method = "random_shuffle", num_surr = num_surr)

rho_surr <- data.frame(SST=numeric(num_surr))
rho_surr <- data.frame(Hammerheads=numeric(num_surr))

for (i in 1:num_surr) {
    rho_surr$SST[i] <- ccm(cbind(for_edm$Hammerheads, surr_SST[, i]), E = 4, lib_column = 1, target_column = 2, lib_sizes = NROW(for_edm), 
        replace = FALSE)$rho
    
}
   (sum(model_out$rho < rho_surr$SST) + 1) / (length(rho_surr$SST) + 1)
   
for (i in 1:num_surr) {
  rho_surr$Hammerheads[i] <- ccm(cbind(for_edm$SST, surr_ham[, i]), E = 4, lib_column = 1, target_column = 2, lib_sizes = NROW(for_edm), 
                         replace = FALSE)$rho
  
}
(sum(model_out2$rho < rho_surr$Hammerheads) + 1) / (length(rho_surr$Hammerheads) + 1)


select.lags<-function(x,y,max.lag=15) {
  y<-as.numeric(y)
  y.lag<-embed(y,max.lag+1)[,-1,drop=FALSE]
  x.lag<-embed(x,max.lag+1)[,-1,drop=FALSE]

  t<-tail(seq_along(y),nrow(y.lag))

  ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))

  aic<-as.numeric(lapply(ms,AIC))
  bic<-as.numeric(lapply(ms,BIC))
  structure(list(ic=cbind(aic=aic,bic=bic),
    selection=list(aic=which.min(aic),bic=which.min(bic))))
}

library(vars)
VARselect(for_edm)
s<-select.lags(for_edm$SST,for_edm$Hammerheads,12)

t(s$selection)
plot.ts(s$ic)
grangertest(Hammerheads ~ SST, order = 2, data = for_edm)

```