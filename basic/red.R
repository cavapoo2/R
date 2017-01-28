dat = data.frame(id=c(1,1,2,2),group=c(1,1,1,2),session=c(1,2,1,2),outcome=c("BDI","BDI","IQ","IQ"),score=c(10,11,100,98))

models = c("BDI","IQ")
for (mod in models)
{
    nxt_mod = dat$outcome == mod 
    new_dat = data.frame(dat,nxt_mod)
    mod_fit = lmer(nxt_mod~group+session+group*session+(1|id),new_dat)
    s = summary(mod_fit)
    s$model_type = nxt_mod
    
}
models1 = c("AA","BB","CC","DD")
#suma = vector(mode = "list", length = length(models))
suma = data.frame()
tr = table(models1)
for (i in 1:4)
{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
#suma[[i]] =summary(lm.D9) 
a = summary(lm.D9)

}
