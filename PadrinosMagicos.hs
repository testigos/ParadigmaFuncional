import Text.Show.Functions

type Deseo = Chico->Chico

data Chico = Chico {
    nombre1::String,
    edad::Float,
    habilidades::[String],
    deseos::[Deseo]
} deriving (Show)

instance Eq Chico where
    chico1 == chico2 = (nombre1 chico1) == (nombre1 chico2)

data Chica = Chica {
    nombre2::String,
    condicion::Chico->Bool
}

timmy = Chico {
    nombre1 = "Timmy",
    edad = 10,
    habilidades = ["mirar television","jugar en la pc"],
    deseos = [(cambiarEdad serMayor)]
} 

trixie = Chica {
    nombre2 = "Trixie Tang",
    condicion = noEsTimmy
}

vicky = Chica {
    nombre2 = "Vicky",
    condicion = (tieneHabilidad "ser un supermodelo noruego")
}

aprenderHabilidades::[String]->Chico->Chico
aprenderHabilidades habilidades (Chico nom edad hab des) = Chico nom edad (hab ++ habilidades) des

jugarTodosLosNeedForSpeed = ["jugar a need for speed " ++ (show x) | x <- [0..]] 

serGrosoEnNeedForSpeed::Chico->Chico
serGrosoEnNeedForSpeed (Chico nom edad hab des) = Chico nom edad (hab ++ jugarTodosLosNeedForSpeed) des

serMayor::Float->Float
serMayor x = 18

cosmo::Float->Float
cosmo x = x / 2

wanda2::Float->Float
wanda2 x = x + 1

wanda::Chico->Chico
wanda chico = cambiarEdad wanda2 (cumplirPrimerDeseo chico)

cumplirPrimerDeseo::Chico->Chico
cumplirPrimerDeseo chico = head (deseos chico) chico

cambiarEdad::(Float->Float)->Chico->Chico
cambiarEdad criterio (Chico nom edad hab des) = Chico nom (criterio edad) hab des

muffinMagico::Chico->Chico
muffinMagico chico = foldl (flip ($)) chico (deseos chico)

tieneHabilidad::String->Chico->Bool
tieneHabilidad hab chico = any (==hab) (habilidades chico)

esSuperMaduro::Chico->Bool
esSuperMaduro chico = (edad chico >= 18) && (tieneHabilidad "sabe manejar" chico)

noEsTimmy::Chico->Bool
noEsTimmy chico = (nombre1 chico) /= "Timmy"

quienConquistaA::Chica->[Chico]->Chico
quienConquistaA chica pretendientes 
    | (filter (cumpleCondicion (condicion chica)) pretendientes) /= [] = head (filter (cumpleCondicion (condicion chica)) pretendientes)
    | otherwise = last pretendientes

cumpleCondicion::(Chico->Bool)->Chico->Bool
cumpleCondicion condicion chico = condicion chico

infractoresDeDaRules::[Chico]->[String]
infractoresDeDaRules chicos = map (nombre1) (filter (tienenDeseoProhibido) chicos)

tienenDeseoProhibido::Chico->Bool
tienenDeseoProhibido chico = tieneHabilidadesProhibidas (muffinMagico chico)

habilidadesProhibidas = ["enamorar","dominar el mundo","matar"]

tieneHabilidadesProhibidas::Chico->Bool
tieneHabilidadesProhibidas chico = any (tieneProhibidas5 chico) habilidadesProhibidas

tieneProhibidas5 chico habilidad = elem habilidad (take 5 (habilidades chico))