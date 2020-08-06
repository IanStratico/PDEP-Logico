import Text.Show.Functions()

data Jugador = Jugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
}deriving (Eq,Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Int,
    precisionJugador :: Int
}deriving (Eq, Show)

bart :: Jugador
bart = Jugador{
    nombre = "Bart",
    padre = "Homero",
    habilidad = habilidadBart
} 

habilidadBart :: Habilidad
habilidadBart = Habilidad {
    fuerzaJugador = 25,
    precisionJugador = 60
}

todd :: Jugador
todd = Jugador {
    nombre ="Todd",
    padre = "Ned",
    habilidad = habilidadTodd
}

habilidadTodd :: Habilidad
habilidadTodd = Habilidad {
    fuerzaJugador = 15,
    precisionJugador = 80
}

rafa :: Jugador
rafa = Jugador {
    nombre = "Rafa",
    padre = "Gorgory",
    habilidad = habilidadRafa
}

habilidadRafa :: Habilidad
habilidadRafa = Habilidad {
    fuerzaJugador = 10,
    precisionJugador = 1
}

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
}deriving (Eq, Show)

mapVelocidad :: (Int -> Int) -> Tiro -> Tiro
mapVelocidad unaFuncion unTiro = unTiro {velocidad = (unaFuncion.velocidad) unTiro}

mapPrecision :: (Int -> Int) -> Tiro -> Tiro
mapPrecision unaFuncion unTiro = unTiro {precision = (unaFuncion.precision) unTiro}

mapAltura :: (Int -> Int) -> Tiro -> Tiro
mapAltura unaFuncion unTiro = unTiro {altura = (unaFuncion.altura) unTiro}

between :: Int -> Int -> Int -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Puntos = Int

type Palo = Habilidad -> Tiro

plutter :: Palo
plutter unaHabilidad = Tiro {
    velocidad = 10,
    precision = (*) 2 (precisionJugador unaHabilidad),
    altura = 0
}

madera :: Palo
madera unaHabilidad = Tiro {
    velocidad = 100,
    precision = div (precisionJugador unaHabilidad) 2,
    altura = 5
}

hierro :: Int -> Palo
hierro numero unaHabilidad = Tiro {
    velocidad = (*) numero (fuerzaJugador unaHabilidad),
    precision = div (precisionJugador unaHabilidad) numero,
    altura = alturaHierro numero
}

alturaHierro :: Int -> Int
alturaHierro unNumero 
    | unNumero >= 3 = unNumero-3
    | otherwise = 0

palos :: [Palo]
palos = [plutter,madera] ++ map hierro [1..10] 

golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo (habilidad unJugador)

-- type Obstaculo = Tiro -> Tiro

data Obstaculo = Obstaculo {
    condicionParaSuperarse :: (Tiro -> Bool),
    efectoPostTiro  :: (Tiro -> Tiro)
}

-- superarObstaculoSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Tiro -> Tiro
-- superarObstaculoSi condicion efecto unTiro 
--     | condicion unTiro = efecto unTiro
--     | otherwise = fallarObstaculo unTiro

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro 
    | (condicionParaSuperarse unObstaculo) unTiro = (efectoPostTiro unObstaculo) unTiro
    | otherwise = fallarObstaculo unTiro

fallarObstaculo :: Tiro -> Tiro
fallarObstaculo unTiro = ((mapAltura (const 0)).(mapPrecision (const 0)).(mapVelocidad (const 0))) $ unTiro

condicionTunelConRampita :: Tiro -> Bool
condicionTunelConRampita unTiro = (precision unTiro) > 90 && (altura unTiro) == 0

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita unTiro = ((mapAltura (const 0)).(mapVelocidad (*2)).(mapPrecision (const 100))) $ unTiro

condicionLaguna :: Tiro -> Bool
condicionLaguna unTiro = (velocidad unTiro) > 80 && between 1 5 (altura unTiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoDeLaLaguna unTiro = unTiro {altura = div (altura unTiro) largoDeLaLaguna}

condicionHoyo :: Tiro -> Bool
condicionHoyo unTiro = between 5 20 (velocidad unTiro) && (precision unTiro) > 95 && (altura unTiro) == 0

efectoHoyo :: Tiro -> Tiro
efectoHoyo unTiro = fallarObstaculo unTiro

tunelConRampita :: Obstaculo 
tunelConRampita  = Obstaculo{
  condicionParaSuperarse = condicionTunelConRampita,
  efectoPostTiro = efectoTunelConRampita  
}  

laguna :: Int -> Obstaculo
laguna largoDeLaLaguna = Obstaculo {
    condicionParaSuperarse = condicionLaguna ,
    efectoPostTiro = efectoLaguna largoDeLaLaguna
}    

hoyo :: Obstaculo
hoyo  = Obstaculo {
    condicionParaSuperarse = condicionHoyo,
    efectoPostTiro = efectoHoyo
}

tiroFinalSuperaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
tiroFinalSuperaObstaculo  unJugador unObstaculo unPalo = (condicionParaSuperarse unObstaculo) (golpe unJugador unPalo) 

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (tiroFinalSuperaObstaculo unJugador unObstaculo) palos

-- tiroAplicadoAMuchosObstaculos :: Tiro -> [Obstaculo] -> Tiro
-- tiroAplicadoAMuchosObstaculos unTiro unosObstaculos = foldr intentarSuperarObstaculo unTiro unosObstaculos

cantidadObstaculosSuperados :: Tiro -> [Obstaculo] -> Int
cantidadObstaculosSuperados _ [] = 0
cantidadObstaculosSuperados unTiro (x:xs) 
    | (condicionParaSuperarse x) unTiro = 1 + cantidadObstaculosSuperados ((efectoPostTiro x) unTiro) xs 
    | otherwise = 0

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador unosObstaculos = maximoSegun (flip cantidadObstaculosSuperados unosObstaculos.golpe unJugador) palos

-- padresPerdedores :: [(Jugador,Int)] -> [String]
-- padresPerdedores puntosPorJugador = map (padre.fst) ((tail.ordenarPuntos) puntosPorJugador)

-- ordenarPuntos :: [(Jugador,Int)] -> [(Jugador,Int)]
-- ordenarPuntos [] = []
-- ordenarPuntos (x:xs) = mayorSegun snd x (head xs) : ordenarPuntos xs

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador).puntosGanados). filter (/= puntosDeUnJugador)) puntosDeTorneo