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

{- 
modificar :: (Turista->Int)->(Int -> Int -> Int) -> Int -> Turista -> Turista
modificar parametro f unidades turista = turista {parametro = (f (parametro turista) unidades)} -}   -- Queria hacer una abstraccion de este estilo pero no me sale, no se si se puede lo que quiero.

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
salirConGenteQueHabla idioma = aprenderIdioma idioma

aprenderIdioma :: Idioma->Turista->Turista
aprenderIdioma idioma turista = turista {idiomas= ((:) idioma.idiomas) turista}

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
{- 
realizarExcursion :: Turista->Excursion->Turista
realizarExcursion turista excursion = (reducirStress porcentajeDeStress).excursion turista -}

porcentajeDeStress :: Int->Turista->Int
porcentajeDeStress porcentaje = (*porcentaje).nivelDeStress