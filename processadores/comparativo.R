load("importantes/experimentos0108/exps_0208_1.Rdata")
load("importantes/experimentos0108/exps_0208_2.Rdata")

library(mlbench)
library(caret)

#exp16

resamps <- resamples(list("3-GRAM, #, Emoticon, CV (Baseline)" = fit,
                          "Baseline, NLP, Sentimen (W + #), Erros, Conjunções, Palavroes" = fit11,
                          "Baseline, Substantivos, Adjetivos, Advérbios, Verbos" = fit12,
                          "Baseline, NLP, Sentimen (W + #), Erros, Conjunções, Palavroes, Substantivos, Adjetivos, Advérbios, Verbos" = fit13,
                          "Baseline, Erros, Conjunções, Substantivos, Adjetivos, Advérbios, Verbos" = fit14,
                          "Baseline, NLP, Sentimen (W + #), Erros" = fit15,
                          "Baseline, NVZ, NLP, Sentimen (W + #), Erros, Conjunções, Palavroes, Substantivos, Adjetivos, Advérbios, Verbos" = fit16
                          )
                    )

resamps <- resamples(list("1" = fit,
                          "2" = fit11,
                          "3" = fit12,
                          "4" = fit13,
                          "5" = fit14,
                          "6" = fit15,
                          "7" = fit16
)
)

summary(resamps)

bwplot(resamps, layout = c(2, 2))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")

trellis.par.set(theme1)
xyplot(resamps, what = "BlandAltman")

difValues <- diff(resamps)
trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))

dotplot(difValues)
