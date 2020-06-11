{-
Nombre: Garozzo, Marcio Nicolas David
Legajo: 168061-4
-}

module Lib where
import Text.Show.Functions

laVerdad = True


{-
Completá tus datos al inicio del archivo y hacé el primer commit & push.
Toda la solución del parcial debe estar en este archivo.
Recordá ir haciendo commit & push a medida que resuelvas el parcial.
-}
{-
==========================================================PARCIAL========================================================== 
Una empresa de turismo localizada en una isla que nadie sabe dónde está nos pide construir un software que los ayude a sacar estadísticas 
de los tours que ofrece a sus clientes: los turistas…
De cada turista nos interesa:
Sus niveles de cansancio y stress
Si está viajando solo
Los idiomas que habla -}
{- 
==========================================================PUNTO 1========================================================== 
Crear un modelo para los turistas y crear los siguientes tres ejemplos:
Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress. -}

data Turista = UnTurista {
    nivelDeCansancio :: Int,
    nivelDeStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving (Show, Eq)

type Idioma = String

ana :: Turista
ana = UnTurista 0 21 False ["español"]

beto :: Turista
beto = UnTurista 15 15 True ["aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["aleman", "catalan"]
{- 
==========================================================PUNTO 2========================================================== 
Modelar las siguientes excursiones de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. -}
type Excursion = Turista->Turista
--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = reducirCansancio 5 turista
                   | otherwise = reducirStress 1 turista

modificarCansancio :: (Int -> Int -> Int) -> Int -> Turista -> Turista
modificarCansancio f unidades turista = turista{nivelDeCansancio = f (nivelDeCansancio turista) unidades}

aumentarCansancio :: Int->Turista->Turista
aumentarCansancio unidades = modificarCansancio (+) unidades

reducirCansancio :: Int->Turista->Turista
reducirCansancio unidades = modificarCansancio (-) unidades

modificarStress :: (Int -> Int -> Int) -> Int -> Turista -> Turista
modificarStress f unidades turista = turista{nivelDeStress = f (nivelDeStress turista) unidades}

aumentarStress :: Int->Turista->Turista
aumentarStress unidades = modificarStress (+) unidades

reducirStress :: Int->Turista->Turista
reducirStress unidades = modificarStress (-) unidades

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 

apreciarElementoPaisaje :: String->Excursion

apreciarElementoPaisaje elemento = reducirStress $ length elemento

--Salir con gente que habla un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.

salirConGenteQueHabla :: Idioma->Excursion
salirConGenteQueHabla idioma = continuarViajeAcompañado . aprenderIdioma idioma

aprenderIdioma :: Idioma->Turista->Turista
aprenderIdioma idioma turista = turista {idiomas= ((:) idioma.idiomas) turista}

continuarViajeAcompañado :: Turista->Turista
continuarViajeAcompañado turista = turista {viajaSolo = False}


--Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminada. 
--  El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.

caminarMinutos :: Int->Excursion
caminarMinutos minutos = aumentarCansancio (intensidadCaminata minutos).reducirStress (intensidadCaminata minutos)

intensidadCaminata :: Int->Int
intensidadCaminata minutos = minutos`div`4

--Paseo en barco: depende de cómo esté la marea
--si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
--si está moderada, no pasa nada.
--si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
type Marea = String

paseoEnBarco :: Marea->Excursion
paseoEnBarco "fuerte" = aumentarCansancio 10 . aumentarStress 6
paseoEnBarco "moderada" = id
paseoEnBarco "tranquila" = salirConGenteQueHabla "aleman".apreciarElementoPaisaje "mar".caminarMinutos 10

{-Además:
Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress. -}

realizarExcursion :: Turista->Excursion->Turista
realizarExcursion turista excursion = (reducirStress (porcentajeDeStress 10 turista).excursion) turista

porcentajeDeStress :: Int->Turista->Int
porcentajeDeStress porcentaje = (`div` 100).(*porcentaje).nivelDeStress

{- 
==========================================================PUNTO 2 B========================================================== 
Dada la función -}
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2
{- 
Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice 
después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
Por ejemplo, si “stress” es la función que me da el stress de un turista:
> deltaExcursionSegun stress ana irALaPlaya
-3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20) -}

deltaExcursionSegun :: (Turista->Int)->Turista->Excursion->Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (realizarExcursion turista excursion) turista

{- 
==========================================================PUNTO 2 C========================================================== 
Usar la función anterior para resolver cada uno de estos puntos:-}

--Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.

excursionEducativa :: Turista->Excursion->Bool
excursionEducativa turista = (>0).deltaExcursionSegun (length.idiomas) turista

--Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.

excursionDesestresante :: Turista->Excursion->Bool
excursionDesestresante turista = (<=(-3)).deltaExcursionSegun nivelDeStress turista


{- 
==========================================================PUNTO 3========================================================== 
Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones. -}
type Tour = [Excursion]

{- 
Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, 
          y finaliza con una salida con gente local que habla "melmacquiano". -}

completo :: Tour
completo = [caminarMinutos 20, apreciarElementoPaisaje "cascada", irALaPlaya.caminarMinutos 40, salirConGenteQueHabla "melmacquiano"]
{- 
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
        Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, 
        luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas. -}

ladoB :: Excursion->Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminarMinutos 120]

{- 
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", 
            sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, 
            y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino. -}

islaVecina :: Marea->Tour
islaVecina "fuerte" = [paseoEnBarco "fuerte", apreciarElementoPaisaje "lago", paseoEnBarco "fuerte"]
islaVecina marea = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

{- 
==========================================================PUNTO 3 A========================================================== 
Hacer que un turista haga un tour. Esto implica primero pagar, lo que aumenta el stress tantas unidades como excursiones tenga el tour, y luego realizar las excursiones en orden. -}

realizarTour :: Tour->Turista->Turista
realizarTour tour turista =foldl realizarExcursion (pagarTour tour turista) tour

pagarTour :: Tour->Turista->Turista
pagarTour tour = aumentarStress $ length tour

{- 
==========================================================PUNTO 3 B========================================================== 
Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. 
Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla. -}

existeTourConvincente :: Turista->[Tour]->Bool
existeTourConvincente turista = any $ tourConvincente turista

tourConvincente :: Turista->Tour->Bool
tourConvincente turista = any $ excursionConvincente turista
 
excursionConvincente :: Turista->Excursion->Bool
excursionConvincente turista excursion= (not.viajaSolo.realizarExcursion turista) excursion && excursionDesestresante turista excursion

{- 
==========================================================PUNTO 3 C========================================================== 
Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista
a quienes les resultó convincente el tour. La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour. -}

efectividadTour :: Tour->[Turista]->Int
efectividadTour tour = sum . map (espiritualidad tour)

espiritualidad :: Tour->Turista->Int
espiritualidad tour turista = abs $ deltaTourSegun nivelDeCansancio tour turista + deltaTourSegun nivelDeStress tour turista

deltaTourSegun :: (Turista->Int)->Tour->Turista->Int
deltaTourSegun indice tour turista = deltaSegun indice (realizarTour tour turista) turista

{- 
==========================================================PUNTO 4========================================================== 
Implementar y contestar en modo de comentarios o pruebas por consola
Construir un tour donde se visiten infinitas playas.
-}
tourInfinitoDePlayas :: Tour
tourInfinitoDePlayas = repeat irALaPlaya

-- ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
--Ana:
{-*Lib Lib> tourConvincente ana tourInfinitoDePlayas
True -}
--Beto:
{- *Lib Lib> tourConvincente beto tourInfinitoDePlayas
Interrupted. -}

{-------------------------------RESPUESTA:
Estos resultados diferentes se debe al metodo de evaluacion que posee Haskell (Evaluacion Diferida / Lazy evaluation), 
en donde el lenguaje no busca procesar todo lo que le pasamos, si no lo que necesita.
En el caso de Ana, ya el primer elemento de la lista cumple con la condicion para que sea tour convincente (exista una excursion convincente). Es un algoritmo que converge
En cambio, en Beto, el programa quedará procesando hasta que lo interrumpamos buscando un elemento que cumpla la condicion. Diverge.
-}


-- ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

--------------------------------RESPUESTA:
{- No existiria ningun caso donde se pueda conocer la efectividad de este tour infinito, 
   porque nunca terminariamos de reducir la lista de tours en un solo turista para calcular su espiritualidad 
   (en otras palabras, nunca terminariamos de procesar realizarTour) 
   La justificacion es por el mismo concepto explicado en la respuesta anterior: Lazy evaluation.
   Al tratarse de un algoritmo divergente por la naturalidad de la lista, no obtenemos respuesta-}
