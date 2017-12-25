load("2110/rdas/2gram-entidades-hora-erro.Rda")

colnames(maFinal)

maFinal[grep("X.food.and.drink.beverages.alcoholic.beverages.cocktails.and.beer", colnames(maFinal)), ]

maFinal$X.food.and.drink.beverages.alcoholic.beverages.cocktails.and.beer
