asiodata <- read.csv("22forVariogram500/aniso500g_InitValue2.csv",header =  T)


# 初期値出力用
spm.fit.ex
spm.fit.sp
spm.fit.ln

i_site = 26

# 確認用
# 指数
spm.model.aniso.ex<-vgm(psill=0.8 ,
                        model="Exp", range= 1.6, nugget= 0) 
spm.fit.ex<-fit.variogram(d.aniso, spm.model.aniso.ex)
plot(d.aniso,spm.model.aniso.ex)
plot(d.aniso,spm.fit.ex)
abline(v = spm.model.aniso.ex$range[2]*3)
# 球
spm.model.aniso.sp<-vgm(psill=40 ,
                        model="Sph", range= 2, nugget= 0) 
spm.fit.sp<-fit.variogram(d.aniso, spm.model.aniso.sp)
plot(d.aniso,spm.model.aniso.sp)
plot(d.aniso,spm.fit.sp)
# 線形
spm.model.aniso.ln<-vgm(psill=0 ,
                        model="Lin", range= 0, nugget= 0.97)
spm.fit.ln<-fit.variogram(d.aniso, spm.model.aniso.ln)
plot(d.aniso,spm.model.aniso.ln)
plot(d.aniso,spm.fit.ln)
# ガウス
spm.model.aniso.ga<-vgm(psill=15 ,
                        model="Gau", range= 4, nugget= 40) 
spm.fit.ga<-fit.variogram(d.aniso, spm.model.aniso.ga)
plot(d.aniso,spm.model.aniso.ga)
plot(d.aniso,spm.fit.ga)
