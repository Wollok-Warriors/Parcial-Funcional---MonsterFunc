-- Utilizo sinónimos de tipo
type Grito = (String, Int, Bool) 

-- Del niño conocemos su nombre, su edad y su altura
type Ninio = (String, Int, Float)

-- tipo para un monstruo: recibe un niño y devuelve su grito
type Monstruo = Ninio -> Grito

-- | Obtener la energía que produce un grito
energiaDeGrito :: Grito -> Int
energiaDeGrito (onomatopeya, intensidad, mojo)
  | mojo      = nivelTerror * intensidad ^ 2
  | otherwise = 3 * nivelTerror + intensidad
  where
    nivelTerror = length onomatopeya

-- Sullivan: "AAAGH" con tantas A como letras del nombre, intensidad = 20 / edad, moja la cama si edad < 3
sullivan :: Monstruo
sullivan (nombreN, edad, _) = (replicate (length nombreN) 'A' ++ "GH", intensidad, mojaLaCama) -- Nota: replicate crea una lista de una longitud especifica, donde todos sus elementos son el mismo
  where
    intensidad = 20 `div` edad
    mojaLaCama= edad < 3

-- Randall Boggs: "¡Mamadera!", intensidad = número de vocales en el nombre, moja si altura entre 0.8 y 1.2
randallBoggs :: Monstruo
randallBoggs (nombreN, _, altura) = ("¡Mamadera!", intensidad, mojaLaCama)
  where
    intensidad = length $ filter (`elem` "aeiouAEIOU") nombreN
    mojaLaCama = altura >= 0.8 && altura <= 1.2

-- Chuck Norris (monstruo): siempre todo el abecedario, 1000 de intensidad, siempre moja la cama
chuckNorrisMonstruo :: Monstruo
chuckNorrisMonstruo _ = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", 1000, True)

-- | Función que recibe una lista de funciones y un elemento. Devuelve la lista resultante de aplicar las funciones al elemento
pam :: [a -> b] -> a -> [b]
pam listaDeFunciones elemento = map ($ elemento) listaDeFunciones -- $ es una función que permite aplicar un conjunto de parámetros a una función 
-- Se usa $ elemento porque elemento son varias funciones

-- | Obtener el grito que cada monstruo le arranca a un niño 
gritosDeMonstruos :: [Monstruo] -> Ninio -> [Grito]
gritosDeMonstruos = pam -- [Monstruo] es una lista de funciones 

-- Los monstruos a veces trabajan en equipo, por lo que van varios a la casa de un niño y todos lo asustan. 
-- | Obtener el conjunto de gritos que logra el equipo.
gritos :: Ninio -> [Monstruo] -> [Grito]
gritos ninio monstruos = gritosDeMonstruos monstruos ninio
