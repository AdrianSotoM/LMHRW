##### SET UP ######
setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/LMHRW")
pacman::p_load(tidyverse,performance,lmerTest,see,dplyr,openai,gptstudio,
               ggstatsplot,tableone,readxl)
data <- read_excel("data.xlsx")
data <- data %>% mutate(tratio= t3/t4) %>% 
  mutate(homa=((insulin/6)*((glucose*18))/405)) 
##### TABLE 1 #####
table1 <- CreateContTable(vars = c("age","bmi","fmi","ffmindex","fatox",
                                  "choox","insulin","glucose","homa",
                                  "adiponectin","ldl","tg","hdl",
                                  "t3","t4","tsh","rt3","chog","satfatm",
                                  "satfats","rmr","enint"),
                         strata = "phase",
                         data=data)
table1
##### LDL~BMI ##### 
bmi <- lmerTest::lmer(ldl~bmi+(1|id),data)
summary(bmi) 
##### LDL~FATMASSINDEX #####
fmi <- lmerTest::lmer(ldl~fmi+(1|id),data)
summary(fmi)
##### LDL~FAT-FREE MASS INDEX #####
ffmi <- lmerTest::lmer(ldl~ffmi+(1|id),data)
summary(ffmi)
##### LDL~THYROID HORMONES #####
t3 <- lmerTest::lmer(ldl~t3+(1|id),data)
summary(t3)
t4 <- lmerTest::lmer(ldl~t4+(1|id),data)
summary(t4)
tsh <- lmerTest::lmer(ldl~tsh+(1|id),data)
summary(tsh)
rt3 <- lmerTest::lmer(ldl~rt3+(1|id),data)
summary(rt3)
##### LDL~INTERACTION MODEL #####
bmibyt3  <- lmerTest::lmer(ldl~bmi*t3+(1|id),data,)
summary(bmibyt3)
##### SENSITIVITY ANALYSES #####
sm1 <- lmerTest::lmer(ldl~bmi+adiponectin+(1|id),data)
summary(sm1)
sm2 <- lmerTest::lmer(ldl~bmi+fatox+(1|id),data)
summary(sm2)
sm3 <- lmerTest::lmer(ldl~bmi+insulin+(1|id),data)
summary(sm3)
sm4 <- lmerTest::lmer(ldl~bmi+leptinmgml+(1|id),data)
summary(sm4)
sm5 <- lmerTest::lmer(ldl~bmi+homa+(1|id),data)
summary(sm5)
sm6 <- lmerTest::lmer(ldl~bmi+satfats+(1|id),data)
summary(sm6)
sm7 <- lmerTest::lmer(t3~bmi+(1|id),data)
summary(sm7)
sm8 <- lmerTest::lmer(t4~bmi+(1|id),data)
summary(sm8)
sm9 <- lmerTest::lmer(tsh~bmi+(1|id),data)
summary(sm9)
sm10 <- lmerTest::lmer(rt3~bmi+(1|id),data)
summary(sm10)
sm11 <- lmerTest::lmer(rt3~bmi+(1|id),data)
summary(sm11)
##### FIGURES #####
Fig1 <- compare_performance(t3,t4,tsh,rt3,bmi,fmi,ffmi,bmibyt3,
                               metrics = "common")
Fig1
plot(Fig1)

Fig2 <- ggplot(data,aes(x=phase,y=ldl,fill=bmi,color=bmi,group=id))+
  geom_point(size=4,alpha=0.7)+
  geom_smooth(color="gray",alpha=0.5)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(1,3,by=1))+
  scale_color_gradient(low = "blue", high = "red") +
  scale_fill_gradient(low = "blue", high = "red")+
  labs(title="Fig 2. LDLc change by phase and BMI",
       x="Phase",
       y="LDLc (mg/dL)",
       fill="BMI",
       color="BMI")+
  theme(
    plot.title = element_text(face="bold",size = 14),
    axis.title.x = element_text(face="bold",size = 11),
    axis.title.y = element_text(face="bold",size=11),
    legend.title  = element_text(face="bold", size=11),
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black")
  )
Fig2