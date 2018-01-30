load("2110/rdas/2gram-entidades-hora-erro-not-null-new-test.Rda")

parte1 <- subset(maFinal, select = -c(drunk, drink, bacardi, emoticonPos))
parte2 <- subset(maFinal, select = c(drunk, drink, bacardi, emoticonPos))

library(rowr)
library(RWeka)

maFinalTwo <- cbind.fill(parte1, parte2)

save(maFinalTwo, file="mardigras/2gram-entidades-hora-erro-not-null-new-test_two.Rda")


View(maFinalTwo)
colnames(maFinalTwo)
