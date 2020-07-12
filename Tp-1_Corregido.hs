module Library where
import PdePreludat
import Test.Hspec

take :: Int -> String
drop :: Int -> String
head :: String
elem :: Char -> String
reverse :: String

productoCorriente :: String -> Bool
productoCorriente nombreDelProducto = (head nombreDelProducto == 'a') || (head nombreDelProducto == 'e') || (head nombreDelProducto == 'i') || (head nombreDelProducto == 'o') || (head nombreDelProducto == 'u')
-- Fijate que estas repitiendo la logica de obtener la primer letra del nombre del producto en cada caso del OR.
-- Podes crear una nueva funcion que te permita evitar eso, por ejemplo una que te diga si una letra es o no vocal

productoXL :: String -> String
productoXL nombreDelProducto = nombreDelProducto ++ "XL"

productoCodiciado :: String -> Bool
productoCodiciado nombreDelProducto = PdePreludat.length nombreDelProducto > 10

productoDeLujo :: String -> Bool
productoDeLujo nombreDelProducto = (PdePreludat.elem 'x' nombreDelProducto) || (PdePreludat.elem 'z' nombreDelProducto)

productoDeElite :: String -> Bool
productoDeElite nombreDelProducto = (productoDeLujo nombreDelProducto) && (productoCodiciado nombreDelProducto) && not(productoCorriente nombreDelProducto)

descodiciarProducto :: String -> String
descodiciarProducto nombreDelProducto = PdePreludat.take 10 nombreDelProducto

entregaSensilla :: String -> Bool
entregaSensilla diaDeEntrega = PdePreludat.even (PdePreludat.length diaDeEntrega)
--sencilla x_x

aplicarDescuento :: Num a => a -> a -> a
aplicarDescuento precio descuento =  (Prelude.-) precio descuento

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoDeEnvio =  (Prelude.+) precio costoDeEnvio

precioTotal :: Num a => a -> a -> a -> a -> a
precioTotal precio descuento costoDeEnvio cantidad = (Prelude.*) (aplicarCostoDeEnvio (aplicarDescuento precio descuento) costoDeEnvio) cantidad