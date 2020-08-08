import Text.Show.Functions()

data Serie = Serie {
    nombreSerie :: String,
    actores :: [Actor],
    presupuestoAnual :: Int,
    temporadasEstimadas :: Int,
    raitingPromedio :: Float,
    estaCancelada :: Bool
} deriving (Eq,Show)

mapNombreSerie :: (String -> String) -> Serie -> Serie
mapNombreSerie unaFuncion unaSerie = unaSerie {nombreSerie = (unaFuncion.nombreSerie) unaSerie}

mapActores :: ([Actor] -> [Actor]) -> Serie -> Serie
mapActores unaFuncion unaSerie = unaSerie {actores = (unaFuncion.actores) unaSerie}

mapPresupuestoAnual :: (Int -> Int) -> Serie -> Serie
mapPresupuestoAnual unaFuncion unaSerie = unaSerie {presupuestoAnual = (unaFuncion.presupuestoAnual) unaSerie}

mapTemporadasEstimadas :: (Int -> Int) -> Serie -> Serie
mapTemporadasEstimadas unaFuncion unaSerie = unaSerie {temporadasEstimadas = (unaFuncion.temporadasEstimadas) unaSerie}

mapRaitingPromedio :: (Float -> Float) -> Serie -> Serie
mapRaitingPromedio unaFuncion unaSerie = unaSerie {raitingPromedio = (unaFuncion.raitingPromedio) unaSerie}

mapEstaCancelada :: (Bool -> Bool) -> Serie -> Serie
mapEstaCancelada unaFuncion unaSerie = unaSerie {estaCancelada = (unaFuncion.estaCancelada) unaSerie}

type Restriccion = String

data Actor = Actor {
    nombreActor :: String,
    sueldoPretendido :: Int,
    restricciones :: [Restriccion]
} deriving (Eq,Show)

john :: Actor
john = Actor {
    nombreActor = "Johnny Deep",
    sueldoPretendido = 20000000,
    restricciones = []
}

helena :: Actor
helena = Actor {
    nombreActor = "Helena Bonham Carter",
    sueldoPretendido = 15000000,
    restricciones = []
}

sueldoAcotres :: [Actor] -> Int
sueldoAcotres unosActores = sum.(map sueldoPretendido) $ unosActores

estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = presupuestoAnual unaSerie < sueldoAcotres (actores unaSerie)

tieneRestricciones :: Actor -> Bool
tieneRestricciones unaActor = (length.restricciones $ unaActor) > 0

actoresConRestriccionMayorA1 :: [Actor] -> [Int]
actoresConRestriccionMayorA1 unosActores = filter (> 1) (cantidadRestriccionesPorActor unosActores)

cantidadRestriccionesPorActor :: [Actor] -> [Int]
cantidadRestriccionesPorActor unosActores = (map length).(map restricciones) $ unosActores

esProbelmatica :: Serie -> Bool
esProbelmatica unaSerie = length (actoresConRestriccionMayorA1.actores $ unaSerie) > 3

type Productor = Serie -> Serie

conFavoritismo :: Actor -> Actor -> Productor
conFavoritismo actor1 actor2 unaSerie = mapActores (const(reemplazarActoresPricipales actor1 actor2 unaSerie)) unaSerie

reemplazarActoresPricipales :: Actor -> Actor -> Serie -> [Actor]
reemplazarActoresPricipales actor1 actor2 unaSerie = actor1 : actor2 : (tail.tail.actores $ unaSerie)

timBurton :: Productor
timBurton unaSerie = conFavoritismo john helena unaSerie

gatopardeitor :: Productor
gatopardeitor unaSerie = unaSerie

estireitor :: Productor
estireitor unaSerie = mapTemporadasEstimadas (*2) unaSerie

despereitor :: Productor
despereitor unaSerie = estireitor.timBurton $ unaSerie

canceleitor :: Float -> Productor
canceleitor unRaiting unaSerie 
    | (estaEnRojo unaSerie) || (raitingBajoDe unRaiting unaSerie) = mapEstaCancelada (const True) unaSerie
    | otherwise = unaSerie

raitingBajoDe :: Float -> Serie -> Bool
raitingBajoDe unRaiting unaSerie = (raitingPromedio unaSerie) < unRaiting

type Indice = Serie -> Int

indiceActores :: Indice
indiceActores unaSerie = length.actores $ unaSerie

bienestar :: Serie -> Int
bienestar unaSerie 
    | estaCancelada unaSerie = 0
    | otherwise = (bienestarSegunActores unaSerie) + (bienestarSegunTemporadas unaSerie)

bienestarSegunActores :: Indice
bienestarSegunActores unaSerie
    | (indiceActores unaSerie) < 10 = 3
    | otherwise = 10 - (length(filter tieneRestricciones (actores unaSerie))) 

bienestarSegunTemporadas :: Indice
bienestarSegunTemporadas unaSerie
    | (temporadasEstimadas unaSerie) > 4 = 5
    | otherwise = (10 - (temporadasEstimadas unaSerie)) * 2

sonBeneficiosos :: [Productor] -> Serie -> Bool
sonBeneficiosos unosProductores unaSerie = (bienestar (foldr ($) unaSerie unosProductores)) > 4

esControvertida :: Serie-> Bool
esControvertida unaSerie = cadaActorCobraMasQueElSiguiente (actores unaSerie)

cadaActorCobraMasQueElSiguiente :: [Actor] -> Bool
cadaActorCobraMasQueElSiguiente [] = False
cadaActorCobraMasQueElSiguiente (x:xs) 
    | (sueldoPretendido x) > (sueldoPretendido.head $ xs) = cadaActorCobraMasQueElSiguiente xs
    | otherwise = False

friends :: Serie
friends = Serie {
    nombreSerie = "Friends",
    actores = [helena,helena],
    presupuestoAnual = 50000000,
    temporadasEstimadas = 3,
    raitingPromedio = 4,
    estaCancelada = False
}