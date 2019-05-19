import Text.Show.Functions

data Helado = Helado {
    sabor::String,
    temperatura::Int,
    propAguaFruta::Float
} deriving (Show)

data Cajon = Cajon {
    fruta::String,
    peso::Float
} deriving (Show)

data Bidon = Bidon {
    litros::Float,
    temp::Int
}

cajon1 = Cajon {
    fruta = "frutilla",
    peso = 10
}

cajon2 = Cajon {
    fruta = "limon",
    peso = 20
}

cajon3 = Cajon {
    fruta = "papaya",
    peso = 56
}

mandarina = Helado {
    sabor = "mandarina",
    temperatura = -10,
    propAguaFruta = 0.3
}

limon = Helado {
    sabor = "limon",
    temperatura = -20,
    propAguaFruta = 0.6
}

tramontana = Helado {
    sabor = "tramontana",
    temperatura = -10,
    propAguaFruta = 0.8
}

heladoSalioMal::Helado->Bool
heladoSalioMal helado = noCongelado helado || proporcionNoCorrecta helado

heladoSalioBien::Helado->Bool
heladoSalioBien helado = not (heladoSalioMal helado)

noCongelado::Helado->Bool
noCongelado (Helado sab temp prop) = temp > 0

proporcionNoCorrecta::Helado->Bool
proporcionNoCorrecta (Helado sab temp prop) 
    | sab == "frutilla" = prop /= 0.4
    | sab == "durazno" = prop < 0.2 && prop > 0.6
    | length sab > 8 = prop /= (fromIntegral (cantVocales sab)) / 10
    | otherwise = prop /= (fromIntegral (length sab)) / 10

cantVocales::String->Int
cantVocales palabra = length (filter (esVocal) palabra)

esVocal::Char->Bool
esVocal letra = any (==letra) ['a','e','i','o','u']

heladera::Int->Helado->Helado
heladera grados (Helado sab temp prop) = Helado sab grados prop

batidora::Cajon->Bidon->Helado
batidora (Cajon fruta peso) (Bidon litros temp) = Helado fruta temp (litros / peso)

exprimidora::Cajon->Cajon
exprimidora (Cajon fruta peso) = Cajon fruta (peso / 2)

mixturadora::Helado->Helado->Helado
mixturadora (Helado sab1 temp1 prop1) (Helado sab2 temp2 prop2) = Helado (sab1 ++ "-" ++ sab2) (min temp1 temp2) ((prop1 + prop2) / 2)

dispenser::Float->Bidon
dispenser cant = Bidon cant 0

cintaTransportadora maquinaA maquinaB maquinaC = (maquinaC.maquinaB) maquinaA

cintaUnificadora maquinaA maquinaB maquinaC = maquinaA maquinaB maquinaC

--A partir de un cajon de 10kg de frutillas y 5 litros de agua, utilizando correctamente la
--exprimidora, el dispenser y la batidora
--cintaUnificadora batidora (exprimidora cajon1) (dispenser 5)

--Tomando dos helados cualquiera, enfriarlos a ambos y luego mixturarlos
--cintaUnificadora mixturadora (heladera (-20) mandarina) (heladera (-30) limon)

--cintaTransportadora (cintaUnificadora batidora (exprimidora cajon1) (dispenser 5)) (heladera 0) (heladera (-15))

--(heladera (-15) (heladera 0 (cintaUnificadora batidora (exprimidora cajon1) (dispenser 5))))

prodEnSerie::[Cajon]->Bidon->[Helado]
prodEnSerie cajones bidon = filter (heladoSalioBien) (map (flip batidora bidon) cajones)