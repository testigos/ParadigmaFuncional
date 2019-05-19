import Text.Show.Functions
import Data.List

data Vigilante = Vigilante {
    nombre::String,
    habilidades::[String],
    anioAparicion::Int
} deriving (Show)

instance Eq Vigilante where
    vigilante1 == vigilante2 = ((nombre vigilante1) == (nombre vigilante2)) && ((anioAparicion vigilante1) == (anioAparicion vigilante2))

data Agente = Agente {
    nombree::String,
    agrupacion::String
} deriving (Show)

comediante = Vigilante {
    nombre = "El Comediante",
    habilidades = ["Fuerza"],
    anioAparicion = 1942
}

buho1 = Vigilante {
    nombre = "Buho Nocturno",
    habilidades = ["Lucha","Ingenierismo"],
    anioAparicion = 1963
}

rorschach = Vigilante {
    nombre = "Rorschach",
    habilidades = ["Perseverancia","Deduccion","Sigilo"],
    anioAparicion = 1964
}

seda1 = Vigilante {
    nombre = "Espectro de Seda",
    habilidades = ["Lucha","Sigilo","Fuerza"],
    anioAparicion = 1962
}

ozimandias = Vigilante {
    nombre = "Ozimandias",
    habilidades = ["Inteligencia","Mas inteligencia aun"],
    anioAparicion = 1968
}

buho2 = Vigilante {
    nombre = "Buho Nocturno",
    habilidades = ["Lucha","Inteligencia","Fuerza"],
    anioAparicion = 1939
}

seda2 = Vigilante {
    nombre = "Espectro de Seda",
    habilidades = ["Lucha","Sigilo"],
    anioAparicion = 1940
}

drman = Vigilante {
    nombre = "Dr. Manhattan",
    habilidades = ["Manipulacion de la materia"],
    anioAparicion = 1954
}

jack = Agente {
    nombree = "Jack Bauer",
    agrupacion = "24"
}

comediantee = Agente {
    nombree = "El Comediante",
    agrupacion = "Watchmen"
}

drmann = Agente {
    nombree = "Dr. Manhattan",
    agrupacion = "Watchmen"
}

liam = Agente {
    nombree = "Liam Neeson",
    agrupacion = "Taken"
}

algunosVigilantes = [comediante,seda1,seda2,buho1,buho2,ozimandias,rorschach,drman]

algunosAgentes = [jack,comediantee,drmann,liam]

destruccionNY::[Vigilante]->[Vigilante]
destruccionNY vigilantes = filter (niRorschachNiDrMan) vigilantes

niRorschachNiDrMan::Vigilante->Bool
niRorschachNiDrMan (Vigilante nom hab an) = nom /= "Dr. Manhattan" && nom /= "Rorschach"

muerteVigilante::[Vigilante]->[Vigilante]
muerteVigilante vigilantes = filter (nombresDistintos vigilantes) vigilantes

nombresDistintos::[Vigilante]->Vigilante->Bool
nombresDistintos vigilantes vigilante = all (/=(nombre vigilante)) (map nombre vigilantes)

guerraVietnam::[Vigilante]->[Vigilante]
guerraVietnam vigilantes = map (agregarCinismo algunosAgentes) vigilantes

agregarCinismo::[Agente]->Vigilante->Vigilante
agregarCinismo agentes (Vigilante nom hab an) 
    | any (==nom) (map nombree agentes) = Vigilante nom (hab ++ ["Cinismo"]) an
    | otherwise = Vigilante nom hab an

accidenteLaboratorio::Int->Vigilante
accidenteLaboratorio anio = Vigilante "Dr. Manhattan" ["Manipulacion de la materia"] anio

actaDeKeene::[Vigilante]->[Vigilante]
actaDeKeene vigilantes = filter (eliminarVigilantesViejos vigilantes) vigilantes

eliminarVigilantesViejos::[Vigilante]->Vigilante->Bool
eliminarVigilantesViejos vigilantes vigilante = (not.any (comparacionNombreAnio vigilante)) vigilantes

comparacionNombreAnio::Vigilante->Vigilante->Bool
comparacionNombreAnio vigilante1 vigilante2 = (nombre vigilante1 == nombre vigilante2) && (anioAparicion vigilante1 < anioAparicion vigilante2)

grandesHeroes::Ord b => (Vigilante->a)->(Vigilante->b)->([Vigilante]->[Vigilante])->[Vigilante]->a
grandesHeroes queQuiere deQuien luegoDeQue vigilantes = (queQuiere.(caracteristicaVigilante deQuien).luegoDeQue) vigilantes

caracteristicaVigilante::Ord a => (Vigilante->a)->[Vigilante]->Vigilante
caracteristicaVigilante criterio vigilantes = foldl1 (caracteristicaVigilante2 criterio) vigilantes

caracteristicaVigilante2::Ord a => (Vigilante->a)->Vigilante->Vigilante->Vigilante
caracteristicaVigilante2 criterio vigilante1 vigilante2
    | (criterio vigilante1) > (criterio vigilante2) = vigilante1
    | otherwise = vigilante2

--grandesHeroes (nombre) (length.habilidades) (destruccionNY) [comediante,seda1,seda2,buho1,buho2,ozimandias,rorschach,drman] --PUNTO1
--grandesHeroes (head.habilidades) (length.nombre) (guerraVietnam) [comediante,seda1,seda2,buho1,buho2,ozimandias,rorschach,drman] --PUNTO2