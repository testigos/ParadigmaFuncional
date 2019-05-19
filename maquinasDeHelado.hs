import Text.Show.Functions

data Helado = UnHelado {
    gusto::String,
    temperatura::Int,
    proporcionDeAgua::Float
} deriving (Show)

cajon1 = UnCajon {
    fruta = "frutilla",
    kilos = 10
}

cajon2 = UnCajon {
    fruta = "limon",
    kilos = 20
}

cajon3 = UnCajon {
    fruta = "papaya",
    kilos = 56
}

mandarina = UnHelado {
    gusto = "mandarina",
    temperatura = -10,
    proporcionDeAgua = 0.3
}

limon = UnHelado {
    gusto = "limon",
    temperatura = -20,
    proporcionDeAgua = 0.6
}

tramontana = UnHelado {
    gusto = "tramontana",
    temperatura = -10,
    proporcionDeAgua = 0.8
}

--EJERCICIO 1

heladoSalioMal::Helado->Bool
heladoSalioMal helado = temperatura helado > 0 || proporcionDeAguaMal helado

evaluarProporcionDeAgua::(Float->Bool)->Helado->Bool
evaluarProporcionDeAgua comparacion helado = (comparacion.proporcionDeAgua) helado

proporcionDeAguaMal::Helado->Bool
proporcionDeAguaMal helado | gusto helado == "frutilla" = evaluarProporcionDeAgua (/=0.4) helado
                           | gusto helado == "durazno" = evaluarProporcionDeAgua (<=0.2) helado || evaluarProporcionDeAgua (/=0.6) helado
                           | letrasDelGusto helado < 8 = evaluarProporcionDeAgua (/=((/10).letrasDelGusto) helado) helado
                           | otherwise = evaluarProporcionDeAgua (/=((/10).cantVocales) helado) helado

letrasDelGusto::Helado->Float
letrasDelGusto = (convertirAFloatPalabra.gusto)

cantVocales::Helado->Float
cantVocales = (convertirAFloatPalabra.filter (sonVocales vocales).gusto)

vocales = ['a', 'e', 'i', 'o', 'u']

sonVocales::[Char]->Char->Bool
sonVocales vocales letra = elem letra vocales

convertirAFloatPalabra::[Char]->Float
convertirAFloatPalabra = (fromIntegral.length)

--MAQUINAS (EJERCICIO 2)

data CajonDeFruta = UnCajon {
    fruta::String,
    kilos::Float
}

data BidonDeAgua = UnBidon {
    litros::Float,
    temp::Int
}

heladera::Int->Helado->Helado
heladera n (UnHelado gus temp prop) = UnHelado gus (temp+n) prop

batidora::BidonDeAgua->CajonDeFruta->Helado
batidora (UnBidon litros temp) (UnCajon fruta kilos) = UnHelado fruta temp (litros/kilos)

exprimidora::CajonDeFruta->CajonDeFruta
exprimidora (UnCajon fruta kilos) = UnCajon fruta (kilos/2)

mixturadora::Helado->Helado->Helado
mixturadora (UnHelado nom temp prop) (UnHelado nom2 temp2 prop2) = UnHelado (nom ++ "-" ++ nom2) (min temp temp2) ((/) prop prop2)

dispenser::Float->BidonDeAgua
dispenser cantAgua = UnBidon cantAgua 0

--COMBINACIONES DE MAQUINAS

cintaTransportadora maquina1 maquina2 maquina3 = (maquina3.maquina2) maquina1

cintaUnificadora maquina1 maquina2 maquina3 = maquina1 maquina2 maquina3

{-EJEMPLOS

cintaUnificadora batidora (exprimidora cajon1) (dispenser 5)
UnHelado {gusto = "frutilla", temperatura = 0, proporcionDeAgua = 1.0}

cintaUnificadora mixturadora (heladera (-20) mandarina) (heladera (-30) limon)
UnHelado {gusto = "mandarina-limon", temperatura = -50, proporcionDeAgua = 0.5}

cintaTransportadora (cintaUnificadora batidora (dispenser 5) (exprimidora cajon1)) (heladera 0) (heladera (-15))
UnHelado {gusto = "frutilla", temperatura = -15, proporcionDeAgua = 1.0}

-}
--5 EN SERIE

produccionEnSerie:: [CajonDeFruta]->BidonDeAgua->[Helado]
produccionEnSerie cajonesDeFruta bidon = filter (heladoSalidoBien) (map (batidora bidon) cajonesDeFruta)

heladoSalidoBien::Helado->Bool
heladoSalidoBien = (not.heladoSalioMal)