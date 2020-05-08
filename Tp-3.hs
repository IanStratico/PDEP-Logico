import Data.List
import Text.Show.Functions()

data  Participante = Participante { 
nombre               :: String,
cantidadDeDinero     :: Int,
tacticaDeJuego       :: Tactica,
propiedadesCompradas :: [Propiedad],
accionesDelJuego     :: [Accion]
}  deriving Show

type Accion = (Participante -> Participante)
type Tactica = String 
type Propiedad = (String,Int)

carolina :: Participante
carolina = Participante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAccionistas]

manuel :: Participante
manuel = Participante "Manuel" 500 "Oferente Singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Participante -> Participante
pasarPorElBanco unparticipante = unparticipante {tacticaDeJuego = "Coprador compulsivo", cantidadDeDinero = cantidadDeDinero unparticipante + 40}

gritar :: Participante -> Participante
gritar unparticipante = unparticipante {nombre = (("AHHHH" ++).nombre) unparticipante }

enojarse :: Participante -> Participante
enojarse unparticipante = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante + 50, accionesDelJuego = accionesDelJuego unparticipante ++ [gritar] }

pagarAccionistas :: Participante -> Participante
pagarAccionistas  unparticipante
    | (esAccionista.tacticaDeJuego) unparticipante = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante + 200} 
    | otherwise = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante - 100}

esAccionista :: Tactica -> Bool
esAccionista "Accionista" = True
esAccionista _ = False

esOferenteSingular :: Tactica -> Bool
esOferenteSingular "Oferente Singular" = True
esOferenteSingular _ = False

subastar :: Participante -> Propiedad -> Participante
subastar unparticipante unapropiedad
    | (esAccionista.tacticaDeJuego) unparticipante  = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante - (snd unapropiedad), propiedadesCompradas = propiedadesCompradas unparticipante ++ [unapropiedad] }
    | (esOferenteSingular.tacticaDeJuego) unparticipante = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante - (snd unapropiedad), propiedadesCompradas = propiedadesCompradas unparticipante ++ [unapropiedad] }
    | otherwise = unparticipante

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = ((<150).snd) unaPropiedad

contarPropideadesBaratas :: Participante -> Int
contarPropideadesBaratas unparticipante = length(filter esPropiedadBarata (propiedadesCompradas unparticipante))

contarPropideadesCaras :: Participante -> Int
contarPropideadesCaras unparticipante = length(filter (not.esPropiedadBarata) (propiedadesCompradas unparticipante))

cobrarAlquileres :: Participante -> Participante
cobrarAlquileres unparticipante = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante + (10*(contarPropideadesBaratas unparticipante)) + (20*(contarPropideadesCaras unparticipante))}
