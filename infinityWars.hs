import Text.Show.Functions()
import Data.List

type Derrotas = (String, Int)

data Personaje = Personaje {
    nombre :: String,
    cantidadDePoder :: Int,
    derrotas :: [Derrotas],
    equipamiento :: String
} deriving Show

thanos :: Personaje
thanos = Personaje {
    nombre = "Thanos",
    cantidadDePoder = 100,
    derrotas = [("IronMan",2019)],
    equipamiento = ""
}

ironMan :: Personaje
ironMan = Personaje {
    nombre = "IronMan",
    cantidadDePoder = 600,
    derrotas = [("Hijo de Thanos",2018)],
    equipamiento = ""
}

thor :: Personaje
thor = Personaje {
    nombre = "Thor",
    cantidadDePoder = 200,
    derrotas = [("Thanos",2002)],
    equipamiento = ""
}

cantidadDePersonajesEntrenando :: [Personaje] -> Int
cantidadDePersonajesEntrenando unosPersonajes = length unosPersonajes
 
poderDeUnosPersonajes :: [Personaje] -> [Int]
poderDeUnosPersonajes unosPersonajes = map cantidadDePoder unosPersonajes

entrenamiento :: [Personaje] ->  [Int]
entrenamiento unosPersonajes = map (*(cantidadDePersonajesEntrenando unosPersonajes)) (poderDeUnosPersonajes unosPersonajes)

esDigno :: Personaje -> Bool
esDigno unPersonaje = (cantidadDePoder unPersonaje) >500 && elem "Hijo de Thanos" (derrotasDeUnPersonaje unPersonaje)

derrotasDeUnPersonaje :: Personaje -> [String]
derrotasDeUnPersonaje unPersonaje = map fst (derrotas unPersonaje)

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos unosPersonajes = filter esDigno unosPersonajes

pelea :: Int -> Personaje -> Personaje -> Personaje
pelea unAnio unPersonaje otroPersonaje 
    | ganaElPrimero unPersonaje otroPersonaje = agregarDerrota unAnio otroPersonaje unPersonaje
    | otherwise = agregarDerrota unAnio unPersonaje otroPersonaje

ganaElPrimero :: Personaje -> Personaje -> Bool
ganaElPrimero ganador perdedor = (cantidadDePoder ganador) > (cantidadDePoder perdedor) 

agregarDerrota :: Int -> Personaje -> Personaje -> Personaje
agregarDerrota unAnio perdedor ganador = ganador {derrotas = derrotas ganador ++ [(nombre perdedor, unAnio)]} 

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil unAnio algunosPersonajes otrosPersoanjes = zipWith (pelea unAnio) algunosPersonajes otrosPersoanjes

cantidadDeDerrotasMayorA5 :: Personaje -> Bool
cantidadDeDerrotasMayorA5 unPersonaje = (length (derrotas unPersonaje)) > 5

escudo :: Personaje -> Personaje
escudo unPersonaje 
    | cantidadDeDerrotasMayorA5 unPersonaje = unPersonaje {cantidadDePoder = cantidadDePoder unPersonaje - 100}
    | otherwise = unPersonaje {cantidadDePoder = cantidadDePoder unPersonaje + 50}

trajeMecanizado :: Int -> Personaje -> Personaje
trajeMecanizado version unPersonaje = unPersonaje {nombre = "Iron " ++ (nombre unPersonaje) ++ " V" ++ (show version) }

esUnPersonaje :: Personaje -> String -> Bool
esUnPersonaje unPersonaje tieneQueser = (nombre unPersonaje) == tieneQueser

stormBreaker :: Personaje -> Personaje
stormBreaker unPersonaje
    | esUnPersonaje unPersonaje "Thor" = unPersonaje {nombre = (nombre unPersonaje) ++ " dios del trueno", derrotas = []}
    | otherwise = unPersonaje

