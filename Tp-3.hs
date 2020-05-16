import Data.List
import Text.Show.Functions()

data  Participante = Participante { 
nombre               :: Nombre,
cantidadDeDinero     :: Int,
tacticaDeJuego       :: Tactica,
propiedadesCompradas :: [Propiedad],
accionesDelJuego     :: [Accion]
}  deriving Show

type Accion = (Participante -> Participante) 
type Tactica = String 
type Propiedad = (String,Int)
type Nombre = String

carolina :: Participante
carolina = Participante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAccionistas]

manuel :: Participante
manuel = Participante "Manuel" 500 "Oferente Singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Participante -> Participante
pasarPorElBanco unparticipante = cambiarTactica "Comprador compulsivo" (cambiarDinero 40 unparticipante)

gritar :: Participante -> Participante 
gritar unparticipante = cambiarNombre unparticipante "AHHHH"

enojarse :: Participante -> Participante
enojarse unparticipante = agregarAccion (cambiarDinero 50 unparticipante) [gritar]

pagarAccionistas :: Participante -> Participante
pagarAccionistas  unparticipante
    | (esAccionista.tacticaDeJuego) unparticipante = cambiarDinero 200 unparticipante 
    | otherwise = cambiarDinero (0-100) unparticipante

esAccionista :: Tactica -> Bool
esAccionista "Accionista" = True
esAccionista _ = False

esOferenteSingular :: Tactica -> Bool
esOferenteSingular "Oferente Singular" = True
esOferenteSingular _ = False

subastar :: Participante -> Propiedad -> Participante
subastar unparticipante unapropiedad
    | (esAccionista.tacticaDeJuego) unparticipante || (esOferenteSingular.tacticaDeJuego) unparticipante  = cambiarDinero (0-(snd unapropiedad)) (agregarPropiedad unparticipante [unapropiedad])
    | otherwise = unparticipante

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = ((<150).snd) unaPropiedad

contarPropideadesBaratas :: Participante -> Int
contarPropideadesBaratas unparticipante = length(filter esPropiedadBarata (propiedadesCompradas unparticipante))

contarPropideadesCaras :: Participante -> Int
contarPropideadesCaras unparticipante = length(filter (not.esPropiedadBarata) (propiedadesCompradas unparticipante))

cobrarAlquileres :: Participante -> Participante
cobrarAlquileres unparticipante = cambiarDinero ((10*(contarPropideadesBaratas unparticipante)) + (20*(contarPropideadesCaras unparticipante))) unparticipante

cambiarDinero :: Int -> Participante -> Participante
cambiarDinero unNumero unparticipante  = unparticipante {cantidadDeDinero = cantidadDeDinero unparticipante + unNumero}

cambiarTactica :: Tactica -> Participante -> Participante
cambiarTactica unaTactica unparticipante = unparticipante {tacticaDeJuego = unaTactica }

cambiarNombre :: Participante -> Nombre -> Participante
cambiarNombre unparticipante unNombre = unparticipante {nombre = ((unNombre ++).nombre)unparticipante}

agregarPropiedad :: Participante -> [Propiedad] -> Participante
agregarPropiedad unparticipante listaDePropiedades = unparticipante {propiedadesCompradas = propiedadesCompradas unparticipante ++ listaDePropiedades}

agregarAccion :: Participante -> [Accion] -> Participante
agregarAccion unparticipante listaDeAcciones = unparticipante {accionesDelJuego = accionesDelJuego unparticipante ++ listaDeAcciones}

hacerBerrinchePor :: Propiedad -> Participante -> Participante
hacerBerrinchePor unaPropiedad unparticipante = agregarAccion (cambiarDinero 10 unparticipante) [gritar]

dejarDeHacerBerrinche :: Propiedad -> Participante -> Participante
dejarDeHacerBerrinche unaPropiedad unparticipante 
    | elJugadorTieneLaPropiedad unaPropiedad unparticipante = borrarAccionGritar unparticipante
    | otherwise = unparticipante

esGritar :: Accion -> Bool
esGritar gritar = False

borrarAccionGritar :: Participante -> Participante
borrarAccionGritar unparticipante  = unparticipante {accionesDelJuego = filter esGritar (accionesDelJuego unparticipante)}

elJugadorTieneLaPropiedad :: Propiedad -> Participante -> Bool
elJugadorTieneLaPropiedad unaPropiedad unparticipante = elem unaPropiedad (propiedadesCompradas unparticipante)    

ultimaRonda :: Participante -> [Accion]
ultimaRonda unparticipante = accionesDelJuego unparticipante

juegoFinal :: Participante -> Participante -> Participante
juegoFinal primerPrticipante segundoParticipante = compararDinero primerPrticipante segundoParticipante

compararDinero :: Participante-> Participante -> Participante
compararDinero primerPrticipante segundoParticipante  
    | (cantidadDeDinero primerPrticipante) < (cantidadDeDinero segundoParticipante) = segundoParticipante
    | (cantidadDeDinero primerPrticipante) > (cantidadDeDinero segundoParticipante) = primerPrticipante