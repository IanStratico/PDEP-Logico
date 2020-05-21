type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show


-----   
-----   PUNTO 1
-----


largoDePatente :: Auto -> Int
largoDePatente unAuto = (length.patente) unAuto

compararPatente :: (String -> Bool) -> Auto -> Bool
compararPatente condicion unAuto = (condicion.patente) unAuto

calculoPatental :: Auto -> Int
calculoPatental unAuto
    |   (last.patente) unAuto == '4'    =   ( (* 3000) . largoDePatente ) unAuto
    |   otherwise                       =   20000

costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto
    |   largoDePatente unAuto == 7                                          =   12500
    |   compararPatente (>"DJ") unAuto && compararPatente (<"NC") unAuto    =   calculoPatental unAuto
    |   otherwise                                                           =   15000


ejemplo1:: Auto
ejemplo1 = Auto {
    patente = "DFH029",
    desgasteLlantas = [0.2,0.7,0.4,0.1],
    rpm = 1500,
    temperaturaAgua = 90,
    ultimoArreglo = (1,1,2012)
}

ejemplo2 :: Auto
ejemplo2 = Auto {
    patente = "DRH029",
    desgasteLlantas = [0.51,0.1,0.4,0.6],
    rpm = 1500,
    temperaturaAgua = 90,
    ultimoArreglo = (1,1,2020)
}


-----   
-----   PUNTO 2
-----

desgastePrimeraLlanta :: Auto -> Float
desgastePrimeraLlanta = head.desgasteLlantas 

autoPeligroso :: Auto -> Bool
autoPeligroso  = ((>0.5).desgastePrimeraLlanta) 

anioUltimoArreglo :: Fecha -> Int
anioUltimoArreglo (_,_,anio) = anio

cuandoSeArreglo :: Auto -> Int
cuandoSeArreglo = anioUltimoArreglo.ultimoArreglo

necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).cuandoSeArreglo