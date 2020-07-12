import Text.Show.Functions()

data Cancion = Cancion {
    titulo :: String,
    genero :: String,
    duracion :: Int
} deriving Show

type Efecto = Cancion -> Cancion

data Artista = Artista {
    nombre :: String,
    canciones :: [Cancion],
    efectoPreferido :: Efecto
}deriving Show

mapDuracion :: (Int -> Int) -> Cancion -> Cancion
mapDuracion unaFuncion unaCancion = unaCancion {duracion = (unaFuncion.duracion) unaCancion }

mapTitulo :: (String -> String) -> Cancion -> Cancion
mapTitulo unaFuncion unaCancion = unaCancion {titulo = (unaFuncion.titulo) unaCancion }

mapGenero :: (String -> String) -> Cancion -> Cancion
mapGenero unaFuncion unaCancion = unaCancion {genero = (unaFuncion.genero) unaCancion }

acortar :: Efecto
acortar unaCancion = unaCancion {duracion = (duracion unaCancion) - 60}

acortar' :: Efecto
acortar' = mapDuracion (+ 60)

remixar :: Efecto
remixar unaCancion = unaCancion {duracion = (duracion unaCancion) *2 , titulo = (titulo unaCancion) ++ " remix", genero = "remixeado"}

remixar' :: Efecto
remixar' = (mapDuracion (*2)).(mapTitulo (++ "reimx")).(mapGenero (const "remixeado"))

esAcustico :: Cancion -> Bool
esAcustico unaCancion = (genero unaCancion) == "acustico"

acustizar :: Int -> Efecto
acustizar unTiempo unaCancion 
    | (not.esAcustico) unaCancion = unaCancion {genero = "acustico" , duracion = unTiempo}
    | otherwise = unaCancion

metaEfecto :: Cancion -> [Efecto] -> Cancion
metaEfecto unaCancion unosEfectos = foldr ($) unaCancion unosEfectos

cafeParaDos :: Cancion
cafeParaDos = Cancion {
    titulo = "Cafe para dos",
    genero = "rock melancolico",
    duracion = 146
}

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion {
    titulo = "Fui hasta ahi",
    genero = "rock",
    duracion = 279
}

losEscarabajos :: Artista
losEscarabajos = Artista {
    nombre = "Los escarabajos",
    canciones = [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera],
    efectoPreferido = acortar
}

adela :: Artista
adela = Artista {
    nombre = "Adela",
    canciones = [teAcordas, unPibeComoVos, daleMechaALaLluvia],
    efectoPreferido = remixar
}

elTigrejoaco :: Artista
elTigrejoaco = Artista {
    nombre = "El tigre Joaco",
    canciones = [],
    efectoPreferido = (acustizar 6)
}

rocketRaccoon :: Cancion
rocketRaccoon = Cancion {
    titulo = "Rocket Raccoon",
    genero = "metalica",
    duracion = 1
}
mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = Cancion {
    titulo = "Mientras mi bateria festeja",
    genero = "a",
    duracion = 1
}
tomateDeMadera :: Cancion
tomateDeMadera = Cancion {
    titulo = "Tomate de madera",
    genero = "a",
    duracion = 1
}
teAcordas :: Cancion
teAcordas = Cancion {
    titulo = "Te acordas?",
    genero = "rock",
    duracion = 1
}
unPibeComoVos :: Cancion
unPibeComoVos = Cancion {
    titulo = "Un pibe como vos",
    genero = "a",
    duracion = 1
}
daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = Cancion {
    titulo = "Dale mecha a la lluvia",
    genero = "a",
    duracion = 1
}

cancionCorta :: Cancion -> Bool
cancionCorta unaCancion = (duracion unaCancion) <= 150

vistazo :: Artista -> [Cancion]
vistazo unArtista = ((take 3).(filter cancionCorta)) (canciones unArtista)

perteneceAlGenero :: String -> Cancion -> Bool
perteneceAlGenero unGenero unaCancion = (genero unaCancion) == unGenero

cancionesArtistas :: [Artista] -> [Cancion]
cancionesArtistas unosArtistas = concatMap canciones unosArtistas

playList :: String -> [Artista] -> [Cancion]
playList unGenero unosArtistas = filter (perteneceAlGenero unGenero) (cancionesArtistas unosArtistas)

efectoACanciones :: Artista -> [Cancion]
efectoACanciones unArtista = map (efectoPreferido unArtista) (canciones unArtista)

hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = unArtista {canciones = efectoACanciones unArtista}

tieneGustoHomogeneo :: [Cancion] -> Bool
tieneGustoHomogeneo [] = True
tieneGustoHomogeneo (x:xs) = (genero x) == ((genero.head) xs) && tieneGustoHomogeneo xs

efectoArtistas :: [Artista] -> [Efecto]
efectoArtistas unosArtistas = map efectoPreferido unosArtistas

efectoBanda :: [Efecto] -> Efecto
efectoBanda unosEfectos = foldl1 (.) unosEfectos

formarBanda :: String -> [Artista] -> Artista
formarBanda unNombre unosArtistas = Artista {
    nombre = unNombre,
    canciones = cancionesArtistas unosArtistas ,
    efectoPreferido = (efectoBanda.efectoArtistas) unosArtistas
}

generoSuperador :: [Cancion] -> String
generoSuperador unasCanciones 
    | elem "rock" (map genero unasCanciones) = "rock progresivo"
    | otherwise = maximum (map genero unasCanciones) ++ " progresivo"

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion {
    titulo = concatMap titulo (canciones unArtista),
    duracion = sum (map duracion (canciones unArtista)),
    genero = generoSuperador (canciones unArtista)
}

{-
PARTE D

1) No puede hacerce dj porque nunca terminaria de aplicar su efecto a una lista de canciones infinita

2) Si porque Haskell trabaja con lazy evaluation y solo buscaria las primeras 3 canciones de la lista que cumplan con la condicion de
la funcion vistazo, en lugar de evaluar toda la lista y luego dar una respuesta.

3) no porque si tuviese canciones infinitas, el titulo de su obra maestra nunca se podria terminar de definir, lo mismo pasa con su duracion.


-}