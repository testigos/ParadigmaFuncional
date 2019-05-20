data Jugador = Jugador {
    nombreJ::String,
    edad::Int,
    promGol::Float,
    habilidad::Int,
    cansancio::Float
} deriving (Show)

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

--EJERCICIO 1

figurasEquipo::Equipo->[Jugador]
figurasEquipo = (filter criterioFigura.jugadores)

criterioFigura::Jugador->Bool
criterioFigura jugador = ((>75).habilidad) jugador  && ((>0).promGol) jugador

--EJERCICIO 2

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero::Equipo->Bool
tieneFarandulero = (any (comparacionFarandulera jugadoresFaranduleros).jugadores)

comparacionFarandulera::[String]->Jugador->Bool
comparacionFarandulera faranduleros jugador = any (== (nombreJ jugador)) faranduleros

{- EJERCICIO 3
3) Naturalmente, en todo mundial existe un álbum de figuritas de la compañía Panini. Esta vez se nos pidió que dados una serie de equipos y un 
grupo específico (A,B,C,D,E o F), le digamos los nombres de los jugadores que tendrían que ser las figuritas difíciles (y hasta a veces brillantes).
Para cumplir la condición de ser difícil, el jugador tiene que cumplir simultáneamente:
Ser figura
Ser joven (menor a 27 años)
No ser farandulero.
-}
figuritasDificiles::Char->[Equipo]->[Jugador]
figuritasDificiles grupoFig = (filter (losDificiles).concat.map jugadores.filter ((==grupoFig).grupo)) 

losDificiles::Jugador->Bool
losDificiles jugador = criterioFigura jugador && (not.(comparacionFarandulera jugadoresFaranduleros)) jugador && esJoven jugador

esJoven::Jugador->Bool
esJoven = ((<27).edad)

{- EJERCICIO 4

Se conoce que tras un partido del mundial, los jugadores se cansan debido al extenso calendario que tuvieron en el año y a que 
muchos ya están prontos a su retiro. Definir la función jugarPartido, la cual dado un equpo modifique a sus jugadores según los 
siguientes criterios:
Si el jugador no es farandulero, es joven y figura, su cansancio pasa a ser 50.
Para el resto de los jugadores jóvenes, su cansancio aumenta un 10%.
Si el jugador no es joven y es figura del equipo se incrementa en 20 unidades su cansancio.
En cualquier otro caso, el cansancio se duplica.

-}
jugarPartido::Equipo->Equipo
jugarPartido (Equipo nom grup jug) = Equipo nom grup (map modificarCansancio jug)

modificarCansancio::Jugador->Jugador
modificarCansancio jugador | losDificiles jugador = cambiarCansancio (hacer50) jugador
                     | esJoven jugador = cambiarCansancio (\x->x+x*0.1) jugador
                     | criterioFigura jugador = cambiarCansancio (+20) jugador
                     | otherwise = cambiarCansancio (2*) jugador

cambiarCansancio::(Float->Float)->Jugador->Jugador
cambiarCansancio funcion (Jugador nom ed promG hab cans) = Jugador nom ed promG hab (funcion cans)

hacer50::Float->Float
hacer50 x = 50

--incrementar10::Float->Float
--incrementar10 x = x + x*0.1

{- EJERCICIO 5

5) Empezó el mundial y los partidos se empiezan a jugar. ¿Cómo saber quién gana en cada partido? Cuando se enfrentan 2 equipos, 
se seleccionan los primeros 11 jugadores (por equipo) que menos cansados están y se suma su promedio de gol. El que sume un mejor 
promedio gana el partido. 
Se pide entonces, dados dos equipos, devolver al ganador del partido, con sus jugadores modificados por haber jugado el partido. 
-}

ganadorPartido::Equipo->Equipo->Equipo
ganadorPartido equipo1 equipo2 = max equipo1 equipo2

mayorPromedioDeGol::Equipo->Float
mayorPromedioDeGol = (sum.map promGol.take 11.(quickSort (<=)).jugadores)

instance Ord Equipo where
    (<=) e1 e2 = mayorPromedioDeGol e1 <= mayorPromedioDeGol e2

instance Eq Equipo where
    (==) e1 e2 = mayorPromedioDeGol e1 == mayorPromedioDeGol e2

instance Ord Jugador where
    (<=) j1 j2 = cansancio j1 <= cansancio j2
    
instance Eq Jugador where
    (==) j1 j2 = cansancio j1 == cansancio j2
{-
6) Sabiendo ya cómo se decide el ganador de un partido, ahora queremos saber, a partir de un grupo de equipos, qué equipo se 
consagrará campeón del torneo.
¿Cómo se juegan los partidos? 
El primero juega contra el segundo → Ganador1
Ganador1 juega contra tercer equipo → Ganador2
Ganador2 juega contra cuarto equipo → Ganador3
….
Y así hasta que el ganador del último partido se consagra campeón.
Dar 2 resoluciones diferentes al ejercicio
-}

campeon::[Equipo]->Equipo
campeon equipos = foldl1 ganadorPartido equipos

--campeonVersion2::[Equipo]->Equipo
--campeonVersion2 []= []
--campeonVersion2 (x:y:zs) = (gandorPartido x y)

{-
7) Los días pasaron, las vuvuzelas se escucharon, una nueva Larissa Riquelme se hizo conocida, y el pulpo Paul volvió a 
acertar en los resultados. Después de un gran mundial se quiere saber quién va a ser elegido como el mejor de todos para 
entregarle el premio y ser reconocido en todo el mundo como “EL GROSO”. Para ello se ingresa una lista de equipos, y del 
equipo elegido ganador (el campeón), se quiere saber el nombre del primer jugador que cumpla la condición de ser figura 
(en todo equipo hay 1 por lo menos).

-}

elGroso::[Equipo]->String
elGroso = (nombreJ.head.figurasEquipo.campeon)