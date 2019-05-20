import Text.Show.Functions
import Data.Char

--1

data Persona = UnaPersona {
    habilidades::[String],
    esBueno::Bool
} deriving (Show)

pablo = UnaPersona {
    habilidades = ["hablar", "sonreir"],
    esBueno = True
}

data PowerRanger = UnRanger {
    color::String,
    habilidadesP::[String],
    nivelDePelea::Int
} deriving (Show)

rojo = UnRanger {
    color = "rojo",
    habilidadesP = ["matar"],
    nivelDePelea = 6
}

azul = UnRanger {
    color = "azul",
    habilidadesP = ["matar"],
    nivelDePelea = 9
}

blanco = UnRanger {
    color = "blanco",
    habilidadesP = ["matar"],
    nivelDePelea = 7
}

--2
convertirEnPowerRanger::String->Persona->PowerRanger
convertirEnPowerRanger color (UnaPersona hab _) = UnRanger color (hacerSuper hab) ((sum.map (length)) hab)

habilidadesSuper::[String]->[String]
hacerSuper hab = map( ("super"++).primeraLetraMayuscula) hab

primeraLetraMayuscula::String->String
primeraLetraMayuscula (x:xs) = (toUpper x):xs

--3
formarEquipoRanger::[String]->[Persona]->[PowerRanger]
formarEquipoRanger colores = ((zipWith (convertirEnPowerRanger) colores).filter (esBueno))

--4

findOrElse::(a->Bool)->a->[a]->a
findOrElse condicion valor lista | any (condicion) lista = (head.filter (condicion)) lista
                                 | otherwise = valor

rangerLider::[PowerRanger]->(PowerRanger->Bool)->PowerRanger
rangerLider rangers condicion = findOrElse condicion (head rangers) rangers

queSeaRojo::PowerRanger->Bool
queSeaRojo = ((=="rojo").color)
--5

maximumBy::Ord b=>[a]->(a->b)->a
maximumBy lista funcion = foldl1 (comparacion funcion) lista

comparacion::Ord b => (a->b)->a->a->a
comparacion criterio a b | (>) (criterio a) (criterio b) = a
                         | otherwise = b

rangerMasPoderoso::[PowerRanger]->PowerRanger
rangerMasPoderoso rangers = maximumBy rangers (nivelDePelea)

--mayorNivelDePelea
--6
rangerHabilidoso::PowerRanger->Bool
rangerHabilidoso = ((>5).length.habilidadesP)

--7

repetir::String->String
repetir x = x ++ repetir x

--a
alfa5 = UnRanger {
    color = "metalico",
    habilidadesP = ["reparar", "decir "++(repetir "ay ")],
    nivelDePelea = 0
}

--8
data ChicasSuperpoderosa = UnaChica {
    colorC::String,
    cantPelo::Int
}

cualEsElLider::[a]->(a->Bool)->a
cualEsElLider lista funcion = findOrElse funcion (head lista) lista

chicaLider::ChicasSuperpoderosa->Bool
chicaLider = (=="rojo").colorC

--rangerLider::PowerRanger->Bool
--rangerLider = (=="rojo").color

--FIN PARCIAL