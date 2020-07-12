import Data.List

biblioteca = [("El visitante","Stephen King",592),("shingeki no Kyojin 1","Hajime Isayama", 40),("shingeki no Kyojin 3","Hajime Isayama", 40),("shingeki no Kyojin 127","Hajime Isayama", 40),("Fundacion","Isaac Asimov",230),("Sandman 5","Neil Gaiman", 35),("Sandman 10","Neil Gaiman", 35),("Sandman 12","Neil Gaiman", 35),("Eragon","Christopher Paolin",544),("Eldest", "Christopher Paolin", 704), ("Brisignr","Christopher Paolin",700),("Legado","Christopher Paolin",811)]

vocal :: Char -> Bool
vocal letra = (letra == 'a') || (letra == 'e') || (letra == 'i') || (letra == 'o') || (letra == 'u')

terceroDeUpla :: (String,String,Int) -> Int
terceroDeUpla (_,_,c) = c

primeroDeUpla :: (String,String,Int) -> String
primeroDeUpla (a,_,_) = a

promedioDeHojas :: Int
promedioDeHojas  = div (sum(map terceroDeUpla biblioteca)) (genericLength biblioteca)



lecturaObligatoria :: (String,String,Int) -> String
lecturaObligatoria (a,b,_) | b == "Stephen King" = "Lectura Obligatoria"
                           | a == "Fundacion" = "Lectura Obligatoria"
                           | b == "Christopher Paolin" = "Lectura Obligatoria"

fantasiosa :: (String,String,Int) -> String
fantasiosa (_,b,_) | b == "Christopher Paolin" = "Lectura Fantasiosa"
                   | b == "Neil Gaiman" = "Lectura Fantasiosa"

bibliotecaLigera :: Bool
bibliotecaLigera = all (<=40) (map terceroDeUpla biblioteca)

nombreDeLaBiblioteca ::  String
nombreDeLaBiblioteca = filter (not.(vocal)) (concat (map primeroDeUpla biblioteca))