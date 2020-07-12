import Text.Show.Functions()
import Data.List

type Derrotas = (String, Int)

data Personaje = {
    nombre :: String,
    cantidadDePoder :: Int,
    derrotas :: [Derrotas]

} deriving Show

cantidadDePersonajesEntrenando :: [Personaje] -> Int
cantidadDePersonajesEntrenando unosPersonajes = length unosPersonajes

cambioCantidadDePoder :: Personaje -> Personaje
cambioCantidadDePoder unPersonaje = 

poderDeUnosPersojanes :: [Personaje] -> [Int]
poderDeUnosPersojanes unosPersonajes = 

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unosPersonajes = map *cantidadDePersonajesEntrenando (cantidadDePoder unosPersonajes)