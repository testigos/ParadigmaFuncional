import Text.Show.Functions

--A
type Deseo = (Chico->Chico)

data Chico = Chico {
    nombre::String,
    edad::Int,
    habilidades::[String],
    deseos::[Deseo]
} deriving (Show)

timmy = Chico {
    nombre = "Timmy",
    edad = 12,
    habilidades= ["cocinar", "manejar"],
    deseos = [serMayor]
}

mick = Chico {
    nombre = "Mick",
    edad = 19,
    habilidades = ["hablar", "comer", "enamorar"],
    deseos = [aprenderHabilidades ["jugar", "sentir"]]
}

julio = Chico {
    nombre = "Julio",
    edad = 14,
    habilidades = ["boludear", "hacer pis", "perder"],
    deseos = [serMayor]
}

aprenderHabilidades::[String]->Chico->Chico
aprenderHabilidades capacidades (Chico nom ed hab des) = Chico nom ed (hab++capacidades) des

needForSpeed = ["jugar need for speed " ++ (show x) | x<-[0..]]

serGrosoEnNeedForSpeed::Chico->Chico
serGrosoEnNeedForSpeed = (aprenderHabilidades needForSpeed)

serMayor::Chico->Chico
serMayor = (criterioMaduracion mayorDeEdad) 

cumplirUnDeseo::Chico->Deseo->Chico
cumplirUnDeseo chico deseo = deseo chico

wanda::Chico->Chico
wanda chico = ((criterioMaduracion hacerMadurar).(flip cumplirUnDeseo (head(deseos chico)))) chico

cosmo::Chico->Chico
cosmo = (criterioMaduracion desmadurar)

muffinMagico::Chico->Chico
muffinMagico chico = foldl cumplirUnDeseo chico (deseos chico)

criterioMaduracion::(Int->Int)->Chico->Chico
criterioMaduracion funcion (Chico nom ed hab des) = Chico nom (funcion ed) hab des

desmadurar::Int->Int
desmadurar edad = div edad 2

mayorDeEdad::Int->Int
mayorDeEdad edad = 18

hacerMadurar::Int->Int
hacerMadurar = (+1)

--B

tieneHabilidad::String->Chico->Bool
tieneHabilidad unaHabilidad unChico = elem (unaHabilidad) (habilidades unChico)

esSuperMaduro::Chico->Bool
esSuperMaduro chico = tieneHabilidad "manejar" chico && esMayorDeEdad chico 

esMayorDeEdad::Chico->Bool
esMayorDeEdad = ((>=18).edad)

type Condicion = (Chico->Bool)

data Chica = Chica {
    nombreA::String,
    condicion::(Condicion)
}

bla = Chica {
    nombreA = "bla",
    condicion = (tieneHabilidad "manejar")
}

quienConquistaA::Chica->[Chico]->Chico
quienConquistaA chica chicos = pretendiente (condicion chica) chicos

pretendiente::Condicion->[Chico]->Chico
pretendiente condicion [chico]  = chico
pretendiente condicion (chico:restoChicos) | condicion chico = chico
                                           | otherwise = pretendiente condicion restoChicos

--C

habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

infractoresDeDaRules::[Chico]->[String]
infractoresDeDaRules = (listaNombresChicos.filter (tieneHabilidadesProhibidas))

tieneHabilidadesProhibidas::Chico->Bool
tieneHabilidadesProhibidas = (any (comparacion habilidadesProhibidas).(take 5).habilidades)

comparacion::[String]->String->Bool
comparacion habProhibidas hab = elem (hab) habProhibidas

listaNombresChicos::[Chico]->[String]
listaNombresChicos = map (nombre)