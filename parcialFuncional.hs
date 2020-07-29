import Text.Show.Functions()

data Persona = Persona {
    nombre :: String,
    direccion :: String,
    dinero :: Int,
    comidaFavorita :: Comida,
    cupones :: [Cupon]
}deriving Show

mapDinero :: (Int -> Int) -> Persona -> Persona
mapDinero unaFuncion unaPersona = unaPersona {dinero = (unaFuncion.dinero) unaPersona }

mapComida :: (Comida -> Comida) -> Persona -> Persona
mapComida unaFuncion unaPersona = unaPersona {comidaFavorita = (unaFuncion.comidaFavorita) unaPersona }

data Comida = Comida {
    nombreComida :: String,
    costo :: Int,
    ingredientes :: [String]
} deriving Show

mapCosto :: (Int -> Int) -> Comida -> Comida
mapCosto unaFuncion unaComida = unaComida {costo = (unaFuncion.costo) unaComida }

mapNombre :: (String -> String) -> Comida -> Comida
mapNombre unaFuncion unaComida = unaComida {nombreComida = (unaFuncion.nombreComida) unaComida }

mapIngredientes :: ([String] -> [String]) -> Comida -> Comida
mapIngredientes unaFuncion unaComida = unaComida {ingredientes = (unaFuncion.ingredientes) unaComida }

paula :: Persona
paula = Persona {
    nombre = "paula",
    direccion = "Thames 1585",
    dinero = 3600,
    comidaFavorita = hamburguesaDeluxe,
    cupones = []
}

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida {
    nombreComida = "Hamburguesa Deluxe",
    costo = 350,
    ingredientes = ["Pan","Lechuga","Tomate","Panceta","Queso","Huevo firto"]
}

yo :: Persona
yo = Persona {
    nombre = "Ian",
    direccion = "Capana 4240",
    dinero = 5000,
    comidaFavorita = cuartoDeLibra,
    cupones = [semanaVegana,findeVegetariano]
}

cuartoDeLibra :: Comida
cuartoDeLibra = Comida {
    nombreComida = "Cuarto de libra",
    costo = 300,
    ingredientes = ["Pan","Queso","Ketchup","cebolla"]
}

menosDe200 :: Comida -> Bool
menosDe200 unaComida = (costo unaComida) < 200

alcanzaDinero :: Comida -> Persona -> Bool
alcanzaDinero unaComida unaPersona = (dinero unaPersona) >= (costo unaComida)

restarCostoComida :: Comida -> Persona -> Persona
restarCostoComida unaComida = mapDinero (subtract (costo unaComida))

comprar :: Comida -> Persona -> Persona
comprar unaComida unaPersona 
    | menosDe200 unaComida && alcanzaDinero unaComida unaPersona = (mapDinero (subtract (costo unaComida))).mapComida (const unaComida) $ unaPersona
    | alcanzaDinero unaComida unaPersona = restarCostoComida unaComida unaPersona
    | otherwise = unaPersona

carritoDeCompras :: [Comida] -> Int
carritoDeCompras = (+100).sum.(map (costo))

type Cupon = Comida -> Comida

noVegano :: [String]
noVegano = ["Carne","Huevos","Queso"]

noEsVegana :: String -> Bool
noEsVegana ingrediente = elem ingrediente noVegano 

semanaVegana :: Cupon
semanaVegana unaComida
    | any noEsVegana (ingredientes unaComida) = unaComida
    | otherwise = mapCosto (div 2) unaComida

esoNoEsCocaPapi :: String -> Cupon
esoNoEsCocaPapi unaBebida = (mapNombre (++ " Party")).(mapIngredientes (unaBebida:))

sinTaccis :: Cupon
sinTaccis  = mapIngredientes (map (++ "libre de gluten")) -- CORREGIDO

findeVegetariano :: Cupon
findeVegetariano unaComida  
    | elem "Carne" (ingredientes unaComida) = unaComida
    | otherwise = mapCosto ((div 10).(*3)) unaComida

menosDe10 :: String -> Bool
menosDe10 unaPalabra = length unaPalabra < 10

dameMenosDe10 :: [String] -> [String]
dameMenosDe10 unosIngredientes = filter menosDe10 unosIngredientes

largaDistancia :: Cupon
largaDistancia = (mapCosto (+50)).(mapIngredientes (dameMenosDe10))

comprarConCupones :: Persona -> Comida -> Comida
comprarConCupones unaPersona unaComida = foldr ($) unaComida (cupones unaPersona)

vocales :: [Char]
vocales = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'] -- puedo hacer vocales = "aeiouAEIOU"

noEsVocal :: Char -> Bool
noEsVocal letra = notElem letra vocales

borrarRepetidos :: [String] -> [String]
borrarRepetidos [] = []
borrarRepetidos (x:xs) = x : borrarRepetidos (filter (/= x) xs)

superComida :: [Comida] -> Comida
superComida unasComidas = Comida {
    nombreComida = filter noEsVocal (concatMap nombreComida unasComidas),
    costo = sum (map costo unasComidas),
    ingredientes = borrarRepetidos (concatMap ingredientes unasComidas)
}

menorA400 :: Comida -> Bool
menorA400 unaComida = (costo unaComida) < 400

--compraDeluxe :: [Comida] -> Comida
--compraDeluxe unasComidas = superComida (map (costo *2) (map (filter menorA400)  unasComidas))