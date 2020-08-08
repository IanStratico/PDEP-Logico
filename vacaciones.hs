import Text.Show.Functions()

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Eq,Show)

yo :: Turista
yo = Turista {
    cansancio = 50,
    stress = 20,
    viajaSolo = True,
    idiomas = ["Castellano","Ingles"]
}

ana :: Turista
ana = Turista {
    cansancio = 0,
    stress = 21,
    viajaSolo = False,
    idiomas = ["Espaniol"]
}

beto :: Turista
beto = Turista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}

cati :: Turista
cati = Turista{
    cansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomas = ["Aleman","Catalan"]
}

mapCansancio :: (Int -> Int) -> Turista -> Turista
mapCansancio unaFuncion unTurista = unTurista {cansancio = (unaFuncion.cansancio) unTurista} 

mapStress :: (Int -> Int) -> Turista -> Turista
mapStress unaFuncion unTurista = unTurista {stress = (unaFuncion.stress) unTurista}

mapViajaSolo :: (Bool -> Bool) -> Turista -> Turista
mapViajaSolo unaFuncion unTurista = unTurista {viajaSolo = (unaFuncion.viajaSolo) unTurista}

mapIdiomas :: ([String] -> [String]) -> Turista -> Turista
mapIdiomas unaFuncion unTurista = unTurista {idiomas = (unaFuncion.idiomas) unTurista}

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = mapCansancio (+(-5)) unTurista
    | otherwise = mapStress (+(-1)) unTurista

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje unTurista = mapStress (+(-(length paisaje))) unTurista

salirAHablar :: String -> Excursion
salirAHablar idioma unTurista = ((mapIdiomas (++ [idioma])).mapViajaSolo (const True)) unTurista

caminar :: Int -> Excursion
caminar minutos unTurista = ((mapCansancio (+ intensidadCaminata minutos)).(mapStress (+(-(intensidadCaminata minutos))))) unTurista

intensidadCaminata :: Int -> Int
intensidadCaminata unNumero = unNumero `div` 4    

paseoEnBarco :: String -> Excursion
paseoEnBarco estadoMarea unTurista = comoEstaLaMarea estadoMarea unTurista

comoEstaLaMarea :: String -> Excursion
comoEstaLaMarea estadoMarea unTurista
    | estadoMarea == "Fuerte" = ((mapCansancio (+10)).(mapStress (+6))) unTurista
    | estadoMarea == "Tranquila" = ((caminar 10).(apreciarPaisaje "Mar").(salirAHablar "Aleman")) unTurista
    | otherwise = unTurista 

hacerExcurcion :: Turista -> Excursion -> Turista
hacerExcurcion unTurista unaExcurcion = ((mapStress (+(-(porcentaje (stress unTurista * 10))))).(unaExcurcion)) unTurista

porcentaje :: Int -> Int
porcentaje unNumero = unNumero `div` 100

type Indice = Turista -> Int

deltaExcurcionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcurcionSegun unIndice unTurista unaExcurcion = unIndice unTurista - unIndice (hacerExcurcion unTurista unaExcurcion) 

indiceIdiomas :: Indice
indiceIdiomas unTurista = length.idiomas $ unTurista

esEducativaUnaExcursion :: Turista -> Excursion -> Bool
esEducativaUnaExcursion unTurista unaExcurcion = (deltaExcurcionSegun indiceIdiomas unTurista unaExcurcion) < 0

esDesestresanteUnaExcursion :: Turista -> Excursion -> Bool
esDesestresanteUnaExcursion unTurista unaExcurcion = (deltaExcurcionSegun stress unTurista unaExcurcion) >= 3

type Tour = [Excursion]

completo :: Tour
completo = [(caminar 20),(apreciarPaisaje "cascada"),(caminar 40),irALaPlaya,(salirAHablar "melmaquiano")]

ladoB :: Excursion -> Tour
ladoB unaExcurcion = [(paseoEnBarco "Tranquila"),unaExcurcion,(caminar 120)]

islaVecina :: String -> Tour
islaVecina estadoMarea = [(paseoEnBarco estadoMarea),(mareaEnIsla estadoMarea),(paseoEnBarco estadoMarea)]

mareaEnIsla :: String -> Excursion
mareaEnIsla estadoMarea 
    | estadoMarea == "Fuerte" = apreciarPaisaje "lago"
    | otherwise = irALaPlaya

hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista unTour = foldl hacerExcurcion (mapStress (+(length unTour)) unTurista) unTour

esConvenienteUnTour :: Turista -> [Tour] -> Bool
esConvenienteUnTour unTurista unosTour = any (conviene unTurista) unosTour

conviene :: Turista -> Tour -> Bool
conviene unTurista unTour = (esDesestresanteUnTour unTurista unTour) && (tourDaAcompaniamiento unTurista unTour)
 
esDesestresanteUnTour :: Turista -> Tour -> Bool
esDesestresanteUnTour unTurista unTour = any (esDesestresanteUnaExcursion unTurista) unTour

tourDaAcompaniamiento :: Turista -> Tour -> Bool
tourDaAcompaniamiento unTurista unTour = any (excursioDaAcompaniamiento unTurista) unTour

excursioDaAcompaniamiento :: Turista -> Excursion -> Bool
excursioDaAcompaniamiento unTurista unaExcurcion = (deltaExcurcionSegun indiceSoledad unTurista unaExcurcion) >= 0

indiceSoledad :: Indice
indiceSoledad unTurista
    | viajaSolo unTurista == True = 1
    | otherwise = 0

-- efectividadUnTour :: Tour -> [Turista] -> Int
-- efectividadUnTour unTour unosTuristas = sum (map (espiritualidad unTour) (filter lesResultoConveniente unosTuristas))

espiritualidad :: Tour -> Indice
espiritualidad unTour unTurista = (sum.map (deltaExcurcionSegun stress unTurista) $ unTour) + (sum.map (deltaExcurcionSegun cansancio unTurista) $ unTour)

-- lesResultoConveniente :: Tour -> [Turista] -> Bool
-- lesResultoConveniente unTour unosTuristas = 

inficitasPlayas :: Tour
inficitasPlayas = irALaPlaya : inficitasPlayas 

-- no porque nunca terminaria de evaluarce el tour

-- no porque no se puede saber si les fue conveniente a los turistas hacerlo