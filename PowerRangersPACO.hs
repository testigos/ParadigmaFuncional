import Text.Show.Functions

data Persona = Persona {
    habilidadesP::[String],
    esBuena::Bool
} deriving (Show)

data PowerRanger = PowerRanger {
    colorRanger::String,
    habilidadesR::[String],
    nivel::Int
} deriving (Show)

instance Eq PowerRanger where
    p1 == p2 = (colorRanger p1) == (colorRanger p2) && (nivel p1) == (nivel p2)

instance Ord PowerRanger where
    p1 <= p2 = (nivel p1) <= (nivel p2)

power1 = PowerRanger {
    colorRanger = "rojo",
    habilidadesR = ["fuerza","agilidad"],
    nivel = 10
}

persona1 = Persona {
    habilidadesP = ["comer","dormir"],
    esBuena = True
}

jason = Persona {
    habilidadesP = ["a"],
    esBuena = True
}

skull = Persona {
    habilidadesP = ["d"],
    esBuena = False
}

kimberly = Persona {
    habilidadesP = ["as"],
    esBuena = True
}

bulk = Persona {
    habilidadesP = ["asd"],
    esBuena = False
}

convertirEnPowerRanger::String->Persona->PowerRanger
convertirEnPowerRanger color persona = PowerRanger color ((habilidadesPotenciadas.habilidadesP) persona) ((cantLetras.habilidadesP) persona)

habilidadesPotenciadas::[String]->[String]
habilidadesPotenciadas habilidades = map ("super"++) habilidades

cantLetras::[String]->Int
cantLetras habilidades = foldl1 (+) (map (length) habilidades)

formarEquipoRanger::[String]->[Persona]->[PowerRanger]
formarEquipoRanger colores personas = zipWith (convertirEnPowerRanger) colores (filter (esBuena) personas)

findOrElse::Eq a => (a->Bool)->a->[a]->a
findOrElse condicion valor lista 
    | any (condicion) lista = (head.filter (condicion)) lista
    | otherwise = valor

maximumBy::Ord a => (b->a)->[b]->a
maximumBy funcion = (maximum.map (funcion))

rangerMasPoderoso::[PowerRanger]->PowerRanger
rangerMasPoderoso = maximum

rangerHabilidoso::PowerRanger->Bool
rangerHabilidoso = ((>=5).length.habilidadesR)

alfa5 = PowerRanger {
    colorRanger = "metalico",
    habilidadesR = ["reparar cosas","decir " ++ (decirAy)],
    nivel = 0
}

decirAy = "ay " ++ decirAy

--rangerLider [alfa5,power1] TERMINA
--rangerHabilidoso alfa5 TERMINA
--rangerMasPoderoso [alfa5,power1] TERMINA
--TODAS TERMINAN ???????

data Super = Super {
    colorChica::String,
    cantPelo::Int
} deriving (Show)
    
lider::(a->String)->[a]->a
lider colorChicaORanger equipo
    | elem "rojo" (map (colorChicaORanger) equipo) = head (filter ((=="rojo").colorChicaORanger) equipo)
    | otherwise = head equipo

--C'EST FINI