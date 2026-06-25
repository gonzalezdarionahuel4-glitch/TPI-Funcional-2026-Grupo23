data Color = EnVerde | EnAmarillo | EnRojo deriving (Show, Eq)

-- ========================================================
-- FUNCIÓN: transicion
-- NATURALEZA: Pura 
-- ESTRATEGIA: Pattern Matching (El equivalente a cond/case en Lisp)
-- IMPACTO: No destructiva 
-- ========================================================

transicion :: Color -> String -> (Color, String)
transicion EnRojo "verde"    = (EnRojo, "cambiar-a-verde")
transicion EnVerde "amarillo" = (EnVerde, "cambiar-a-amarillo")
transicion EnAmarillo "rojo"  = (EnAmarillo, "cambiar-a-rojo")
transicion color _           = (color, "accion-por-defecto")

-- ========================================================
-- FUNCIÓN: miTimer
-- NATURALEZA: Pura
-- ESTRATEGIA: Guards
-- IMPACTO: No destructiva
-- ========================================================

miTimer :: Int -> Color
miTimer tiempo
    | t <= 120  = EnVerde
    | t <= 126  = EnAmarillo
    | otherwise = EnRojo
    where t = mod tiempo 216




