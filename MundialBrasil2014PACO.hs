import Text.Show.Functions

data Jugador = Jugador {
    nombreJ::String,
    edad::Int,
    promGol::Float,
    habilidad::Int,
    cansancio::Float
} deriving (Show)

instance Eq Jugador where
    jug1 == jug2 = (nombreJ jug1) == (nombreJ jug2)

instance Ord Jugador where
    jug1 <= jug2 = (cansancio jug1) <= (cansancio jug2)

data Equipo = Equipo {
    nombreE::String,
    grupo::Char,
    jugadores::[Jugador]
} deriving (Show)

martin = Jugador {
    nombreJ = "Martin",
    edad = 26,
    promGol = 0.0,
    habilidad = 50,
    cansancio = 35.0
}

juan = Jugador {
    nombreJ = "Juancho",
    edad = 30,
    promGol = 0.2,
    habilidad = 50,
    cansancio = 40.0
}

maxi = Jugador {
    nombreJ = "Maxi Lopez",
    edad = 27,
    promGol = 0.4,
    habilidad = 68,
    cansancio = 30.0
}

jonathan = Jugador {
    nombreJ = "Chueco",
    edad = 20,
    promGol = 1.5,
    habilidad = 80,
    cansancio = 99.0
}

lean = Jugador {
    nombreJ = "Hacha",
    edad = 23,
    promGol = 0.01,
    habilidad = 50,
    cansancio = 35.0
}

brian = Jugador {
    nombreJ = "Panadero",
    edad = 21,
    promGol = 5,
    habilidad = 80,
    cansancio = 15.0
}

garcia = Jugador {
    nombreJ = "Sargento",
    edad = 30,
    promGol = 1,
    habilidad = 80,
    cansancio = 13.0
}

messi = Jugador {
    nombreJ = "Pulga",
    edad = 26,
    promGol = 10,
    habilidad = 99,
    cansancio = 43.0
}

aguero = Jugador {
    nombreJ = "Aguero",
    edad = 24,
    promGol = 5,
    habilidad = 90,
    cansancio = 5.0
}

equipo1 = Equipo {
    nombreE = "Lo Que Vale Es El Intento",
    grupo = 'F',
    jugadores = [martin,juan,maxi]
}

losDeSiempre = Equipo {
    nombreE = "Los De Siempre",
    grupo = 'F',
    jugadores = [jonathan,lean,brian]
}

restoDelMundo = Equipo {
    nombreE = "Resto Del Mundo",
    grupo = 'A',
    jugadores = [garcia,messi,aguero]
}

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 

mvp::Equipo->[Jugador]
mvp equipo = filter (esMVP) (jugadores equipo)

esMVP::Jugador->Bool
esMVP (Jugador nom edad prom hab can) = hab > 75 && prom > 0

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero::Equipo->Bool
tieneFarandulero equipo = any (esFarandulero) (jugadores equipo)

esFarandulero::Jugador->Bool
esFarandulero jugador = elem (nombreJ jugador) jugadoresFaranduleros

figuritasDificiles::[Equipo]->Char->[String]
figuritasDificiles equipos gru = (map (nombreJ).filter (esDificil).concat.map (jugadores).(filtrarPorGrupo gru)) equipos

filtrarPorGrupo::Char->[Equipo]->[Equipo]
filtrarPorGrupo gru equipos = filter ((==gru).grupo) equipos

esDificil::Jugador->Bool
esDificil jugador = esMVP jugador && esJoven jugador && (not.esFarandulero) jugador

esJoven::Jugador->Bool
esJoven jugador = (edad jugador) < 27

jugarPartido::Equipo->Equipo
jugarPartido (Equipo nom grup jug) = Equipo nom grup (map (modificarJugador) jug)

modificarJugador::Jugador->Jugador
modificarJugador jugador 
    | (not.esFarandulero) jugador && esMVP jugador && esJoven jugador = jugador {nombreJ = (nombreJ jugador), edad = (edad jugador), promGol = (promGol jugador), habilidad = (habilidad jugador), cansancio = 50}
    | esJoven jugador = jugador {nombreJ = (nombreJ jugador), edad = (edad jugador), promGol = (promGol jugador), habilidad = (habilidad jugador), cansancio = (cansancio jugador) + (cansancio jugador) * 10 / 100}
    | esMVP jugador && (not.esJoven) jugador = jugador {nombreJ = (nombreJ jugador), edad = (edad jugador), promGol = (promGol jugador), habilidad = (habilidad jugador), cansancio = (cansancio jugador) + 20}
    | otherwise = jugador {nombreJ = (nombreJ jugador), edad = (edad jugador), promGol = (promGol jugador), habilidad = (habilidad jugador), cansancio = (cansancio jugador) * 2}

competir::Equipo->Equipo->Equipo
competir eq1 eq2 
    | (sum.map (promGol).seleccionarTitulares) eq1 > (sum.map (promGol).seleccionarTitulares) eq2 = jugarPartido eq1
    | otherwise = jugarPartido eq2

seleccionarTitulares::Equipo->[Jugador]
seleccionarTitulares equipo = take 11 (quickSort (<) (jugadores equipo))

campeon::[Equipo]->Equipo
campeon equipos = foldl1 competir equipos

elGroso::[Equipo]->String
elGroso equipos = (nombreJ.head.filter (esMVP).jugadores.campeon) equipos