SELECT (
 SELECT count(t1.id)
 FROM tweets t1
 WHERE t1.textoParserRisadaEmoticom LIKE ("%beer%")
 AND t1.q1 = 1
) /
(
 SELECT count(t1.id)
 FROM tweets t1
 WHERE t1.textoParserRisadaEmoticom LIKE ("%beer%")
 AND t1.q1 = 0
) as indice
FROM tweets t
ORDER by t.indice DESC
LIMIT 1