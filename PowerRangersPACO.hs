import Text.Show.Functions

data Persona = Persona {
    habilidadesP::[String],
    esBuena::Bool
} deriving (Show)

data PowerRanger = PowerRanger {
    color::String,
    habilidadesR::[String],
    nivel::Int
} deriving (Show)

power1 = PowerRanger {
    color = "rojo",
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

findOrElse