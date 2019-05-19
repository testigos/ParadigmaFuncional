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

--2
convertirEnPowerRanger::String->Persona->PowerRanger
convertirEnPowerRanger color (UnaPersona hab _) = UnRanger color (hacerSuper hab) ((sum.map (length)) hab)

hacerSuper::[String]->[String]
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



--6
rangerHabilidoso::PowerRanger->Bool
rangerHabilidoso = ((>5).length.habilidadesP)

--7

repetir::String->[String]
repetir x = x:repetir x

--a
alfa5 = UnRanger {
    color = "metalico",
    habilidadesP = ["reparar"]++(repetir "ay"),
    nivelDePelea = 0
}