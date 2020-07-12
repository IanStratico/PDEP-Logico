import Text.Show.Functions()

type Artefacto = (String,Int)

data Heroe = Heroe {
    nombre :: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto]
} deriving Show

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto unaFuncion unHeroe = unHeroe {epiteto = (unaFuncion.epiteto) unHeroe}

mapReconocimiento :: (Int -> Int) -> Heroe -> Heroe
mapReconocimiento unaFuncion unHeroe = unHeroe {reconocimiento = (unaFuncion.reconocimiento) unHeroe}

mapArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefactos unaFuncion unHeroe = unHeroe {artefactos = (unaFuncion.artefactos) unHeroe}

hercules :: Heroe
hercules = Heroe {
    nombre = "hercules",
    epiteto = "Gurardian del Olimpo",
    reconocimiento = 700,
    artefactos = [pistola,relampagoDeZeus]
}

pistola :: Artefacto
pistola = ("Pistola", 1000)

relampagoDeZeus :: Artefacto
relampagoDeZeus = ("Relampago de Zeus", 500)

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = ("Lanza del Olimpo", 100)

xhipos :: Artefacto
xhipos = ("Xhipos", 50)


paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe
    | (reconocimiento unHeroe) > 1000 = cambiarEpiteto "El mitico" unHeroe
    | (reconocimiento unHeroe) >= 500 = (cambiarEpiteto "El magnifico").agregarArtefacto lanzaDelOlimpo $ unHeroe
    | (reconocimiento unHeroe) > 100 = (cambiarEpiteto "Hoplita").agregarArtefacto xhipos $ unHeroe 
    | otherwise = unHeroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto = mapEpiteto (const unEpiteto)

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unaartefacto unHeroe = mapArtefactos (unaartefacto:) unHeroe

ganarReconocimientoDeArtefacto :: Artefacto -> Heroe -> Heroe
ganarReconocimientoDeArtefacto unArtefacto unHeroe = mapReconocimiento (+ (snd unArtefacto)) unHeroe

encontrarUnArtefacto :: Artefacto -> Heroe -> Heroe
encontrarUnArtefacto unArtefacto unHeroe  = (agregarArtefacto unArtefacto).ganarReconocimientoDeArtefacto unArtefacto $ unHeroe

agregarReconocimiento :: Int -> Heroe -> Heroe
agregarReconocimiento unReconocimiento = mapReconocimiento (+ unReconocimiento) unHeroe

triplicarRarezaDeArtefactos :: 
triplicarRarezaDeArtefactos  = mapArtefactos (map *3   