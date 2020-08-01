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
    costo = 100,
    ingredientes = ["Pan","Queso","Ketchup","cebolla"]
}

menosDe200 :: Comida -> Bool
menosDe200 unaComida = (costo unaComida) < 200

alcanzaDinero :: Comida -> Persona -> Bool
alcanzaDinero unaComida unaPersona = (dinero unaPersona) >= (costo unaComida)

restarCostoComida :: Comida -> Persona -> Persona
restarCostoComida unaComida = mapDinero (subtract (costo unaComida)) -- Uso la funcion subtract para restarle algo a un numero, no mde deja poner (-)

--comprar :: Comida -> Persona -> Persona
--comprar unaComida unaPersona 
--    | menosDe200 unaComida && alcanzaDinero unaComida unaPersona = (mapDinero (subtract (costo unaComida))).mapComida (const unaComida) $ unaPersona
--    | alcanzaDinero unaComida unaPersona = restarCostoComida unaComida unaPersona
--    | otherwise = unaPersona

comprar :: Comida -> Persona -> Persona -- hice una sola gurda y delegue en otra funcion el caso de que la comida cueste menos de 200
comprar unaComida unaPersona
    | alcanzaDinero unaComida unaPersona = (ponerComoFavoritaSiEsBarata unaComida).(restarCostoComida unaComida) $ unaPersona 
    | otherwise = unaPersona

ponerComoFavoritaSiEsBarata :: Comida -> Persona -> Persona
ponerComoFavoritaSiEsBarata unaComida unaPersona
    | menosDe200 unaComida = mapComida (const unaComida) $ unaPersona
    | otherwise = unaPersona

--carritoDeCompras :: [Comida] -> Int
--carritoDeCompras = (+100).sum.(map (costo))

carritoDeCompras :: [Comida] -> Persona -> Persona -- el objetivo de carrito de compras era que una persona compre muchas comidas y ademas se le agreguen 100 al precio final
carritoDeCompras unasComidas unaPersona = mapDinero (subtract 100) (foldr comprar unaPersona unasComidas)

type Cupon = Comida -> Comida

noVegano :: [String]
noVegano = ["Carne","Huevos","Queso"]

noEsVegano :: String -> Bool
noEsVegano ingrediente = elem ingrediente noVegano 

comidaVegana :: Comida -> Bool
comidaVegana unaComida = not(any noEsVegano (ingredientes unaComida))

--semanaVegana :: Cupon
--semanaVegana unaComida
--    | any noEsVegano (ingredientes unaComida) = unaComida
--    | otherwise = mapCosto (div 2) unaComida

semanaVegana :: Cupon
semanaVegana unaComida = descuentoSi comidaVegana unaComida 2

descuentoSi :: (Comida -> Bool) -> Comida -> Int -> Comida --  funcion de orden superior por si hay que hacer mas descuetnos
descuentoSi unaFuncion unaComida unDescuento 
    | unaFuncion unaComida = mapCosto (unDescuento *) unaComida
    | otherwise = unaComida

esoNoEsCocaPapi :: String -> Cupon
esoNoEsCocaPapi unaBebida = (mapNombre (++ " Party")).(mapIngredientes (unaBebida:))

sinTaccis :: Cupon
sinTaccis  = mapIngredientes (map (++ "libre de gluten")) -- CORREGIDO

comidaVegetariana :: Comida -> Bool
comidaVegetariana unaComida = not(elem "Carne" (ingredientes unaComida))

findeVegetariano :: Cupon
findeVegetariano unaComida = descuentoSi comidaVegetariana unaComida 3

--findeVegetariano :: Cupon
--findeVegetariano unaComida  
--    | elem "Carne" (ingredientes unaComida) = unaComida
--    | otherwise = mapCosto ((div 10).(*3)) unaComida

menosDe10 :: String -> Bool
menosDe10 unaPalabra = length unaPalabra < 10

dameMenosDe10 :: [String] -> [String]
dameMenosDe10 unosIngredientes = filter menosDe10 unosIngredientes

largaDistancia :: Cupon
largaDistancia = (mapCosto (+50)).(mapIngredientes (dameMenosDe10))

comprarConCupones :: Persona -> Comida -> Persona -- le cambie el tipo de lo que devolvia para poder hacer que haga la compra de la comida con cupones 
comprarConCupones unaPersona unaComida = comprar (foldr ($) unaComida (cupones unaPersona)) unaPersona

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