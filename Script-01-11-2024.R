
cana=factor(rep(1:6, each=5))
cana

cana=factor(rep(seq(1:6), 5)); cana

y=c(
112.3, 125.3, 118.4, 127.9, 130.1, 115.2,
121.0, 119.7, 120.5, 128.3, 122.4, 123.2,
114.3, 120.8, 119.7, 129.5, 126.7, 117.8,
115.8, 120.5, 118.3, 126.5, 127.3, 120.8,
117.2, 122.3, 117.8, 127.3, 128.9, 116.4
)
y0 = y

saida_y0 = anova(aov(y0~cana))
saida_y0

yt = y+4
y = yt

y2 = y0*3

saida_y2 = anova(aov(y2~cana))
saida_y2

var(y2);var(y0)
25.048*3^2 #Var(y*k) = var(y) * k^2

dados <- data.frame(cbind(cana, y0))
head(dados)


dados2 <- subset(dados, dados$cana == "1" | dados$cana == "2")
dim(dados2)
attach(dados2)
saida_cana1_2 = anova(aov(y0~cana, data = dados2))
saida_cana1_2

bartlett.test(dados2$y0, dados2$cana)
t.test(dados2$y0~dados2$cana, 
       alternative = c("two.sided"),
       paired = F, var.equal = T) #Teste bilateral, dados não pareados com var iguais

saida_cana1_2

(-3.1664)^2


