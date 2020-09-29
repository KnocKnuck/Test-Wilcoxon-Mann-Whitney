setwd("____Your___Path____")
table =read.table('LaborMarket.csv', sep=',',dec='.', header = TRUE )


head(table)
tail(table)
str(table)


"Question 2"
x = table$X90.m
y = table$Gift
plot(x~y)

"Question 3"
"Pour 90 min "
hist(x) 
hist(x[which(table$Gift=='No')])
hist(x[which(table$Gift=='Yes')])

"Pour 180"
hist(table$X180.min)
hist(table$X180.min[which(table$Gift=='Yes')])
hist(table$X180.min[which(table$Gift=='No')])

"270"
hist(table$X270.min)
hist(table$X270.min[which(table$Gift=='No')])
hist(table$X270.min[which(table$Gift=='Yes')])

"360"
hist(table$X360.min)
hist(table$X360.min[which(table$Gift=='No')])
hist(table$X360.min[which(table$Gift=='Yes')])

"Test de loi Normal"
shapiro.test(x[which(table$Gift=='No')])
shapiro.test((x[which(table$Gift=='Yes')]))

shapiro.test(table$X180.min[which(table$Gift=='Yes')])
shapiro.test(table$X180.min[which(table$Gift=='No')])

shapiro.test(table$X270.min[which(table$Gift=='Yes')])
shapiro.test(table$X270.min[which(table$Gift=='No')])

shapiro.test(table$X360.min[which(table$Gift=='Yes')])
shapiro.test(table$X360.min[which(table$Gift=='No')])


"Question 6"
?wilcox.test
wilcox.test(x[which(table$Gift=='Yes')], x[which(table$Gift=='No')], alternative = 'greater')
"Plus productif ceux qui ont reçu un cadeau que ceux qui n'ont pas recu, thanks to p-value = 0.039 basse"


wilcox.test(table$X180.min[which(table$Gift=='Yes')], table$X180.min[which(table$Gift=='No')], alternative = 'greater')

wilcox.test(table$X270.min[which(table$Gift=='Yes')], table$X270.min[which(table$Gift=='No')], alternative = 'greater')

wilcox.test(table$X360.min[which(table$Gift=='Yes')], table$X360.min[which(table$Gift=='No')], alternative = 'greater')


table0 = table[order(table$X90.m),]
table0$Rang = seq(1:19)
W = sum(table0$Rang[which(table0$Gift=='No')])
U = sum(table0$Rang[which(table0$Gift=='Yes')])

        
