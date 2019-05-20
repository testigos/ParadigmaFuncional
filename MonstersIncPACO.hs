import Text.Show.Functions

data Grito = Grito {
    onomatopeya::String,
    intensidad::Int,
    mojoLaCama::Bool
} deriving (Show)

data Chico = Chico {
    nombre::String,
    edad::Int,
    altura::Float
} deriving (Show)

energiaDeGrito::Grito->Int
energiaDeGrito grito 
    | mojoLaCama grito = (length.onomatopeya) grito * (intensidad grito) ^ 2
    | otherwise = 3 * (length.onomatopeya) grito + (intensidad grito)

sullivan::Chico->Grito
sullivan (Chico nom edad alt) = Grito ((repetir (length nom) 'A') ++ "GH") (div 20 edad) (edad < 3) 

repetir::Int->a->[a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

randall::Chico->Grito
randall (Chico nom edad alt) = Grito ("Mamadera!") (cantVocales nom) (alt < 1.2 && alt > 0.8)

cantVocales::String->Int
cantVocales = (length.filter (esVocal))

vocales = ['a','e','i','o','u']

esVocal::Char->Bool
esVocal letra = elem letra vocales

chuck::Chico->Grito
chuck (Chico nom edad alt) = Grito (['a'..'z']) 1000 True

osito::Chico->Grito
osito (Chico nom edad alt) = Grito "uf" edad False

pam::[(a->b)]->a->[b]
pam funciones elemento = map (flip ($) elemento) funciones

gritos::[(Chico->Grito)]->Chico->[Grito]
gritos monstruos chico = pam monstruos chico

produccionEnergeticaGritos::[(Chico->Grito)]->[Chico]->Int
produccionEnergeticaGritos monstruos = (sum.map (energiaDeGrito).concat.map (gritos monstruos))