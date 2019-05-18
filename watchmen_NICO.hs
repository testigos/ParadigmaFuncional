
type Vigilante = (String,[String],Int)

algunosVigilantes::[Vigilante]
algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968), ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]

type Agente = (String,String)
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]


nombre::Vigilante->String
nombre (nom,_,_) = nom

habilidades::Vigilante->[String]
habilidades (_,hab,_) = hab

anioAparicion::Vigilante->Int
anioAparicion (_,_,anio) = anio

--EVENTOS
destruccionDelNiuShork::[Vigilante]->[Vigilante]
destruccionDelNiuShork vigilantes = ((filtrarVigilantes comparacionNombre "Rorschcach").(filtrarVigilantes comparacionNombre "Doctorr Manhattan")) vigilantes

muerteVigilante::Vigilante->[Vigilante]->[Vigilante]
muerteVigilante vigilante vigilantes = filtrarVigilantes comparacionNombre (nombre vigilante) vigilantes

guerraDeVietnam::[Agente]->[Vigilante]->[Vigilante]
guerraDeVietnam agentes vigilantes = (map (agregarHabilidad "Cinismo").filter (sonAgentes agentes)) vigilantes

agregarHabilidad::String->Vigilante->Vigilante
agregarHabilidad habilidad (a,habs,c) = (a,(habs++[habilidad]),c)

sonAgentes::[Agente]->Vigilante->Bool
sonAgentes agentes vigilante = (any (comparacionNombre (nombre vigilante)).listaNombresAgentes) agentes

listaNombresAgentes::[Agente]->[String]
listaNombresAgentes agentes = map (fst) agentes

accidenteDeLaboratorio::Int->Vigilante
accidenteDeLaboratorio anio = ("Doctor Manhattan", ["manipulacion de la materia a nivel atomico"], anio)

actaDeKeene::[Vigilante]->[Vigilante]
actaDeKeene vigilantes = filter (eliminarVigilantesViejos vigilantes) vigilantes

eliminarVigilantesViejos::[Vigilante]->Vigilante->Bool
eliminarVigilantesViejos vigilantes vigilante = (not.any (comparacionNombreAnio vigilante)) vigilantes

comparacionNombreAnio::Vigilante->Vigilante->Bool
comparacionNombreAnio vigilante1 vigilante2 = (nombre vigilante1 == nombre vigilante2) && (anioAparicion vigilante1 < anioAparicion vigilante2) 

filtrarVigilantes::(String->String->Bool)->String->[Vigilante]->[Vigilante]
filtrarVigilantes comparacion filtrarSegun vigilantes = filter ((comparacion filtrarSegun).nombre) vigilantes

comparacionNombre::String->String->Bool
comparacionNombre nom1 nom2 = nom1 /= nom2

--2 GRANDES HEROES

grandesHeroes::([Vigilante]->[Vigilante])->(Vigilante->Int)->(Vigilante->a)->[Vigilante]->a
grandesHeroes eventoPrevio criterioDelGranHeroe criterioADevolver = (criterioADevolver.foldl1 (compararVigilantes (comparacionSegunCriterio criterioDelGranHeroe)).eventoPrevio)

compararVigilantes::(Vigilante->Vigilante->Bool)->Vigilante->Vigilante->Vigilante
compararVigilantes criterio vig1 vig2 | criterio vig1 vig2 = vig1
                                      | otherwise = vig2

comparacionSegunCriterio::Ord a=>(Vigilante->a)->Vigilante->Vigilante->Bool
comparacionSegunCriterio criterio vig1 vig2 = (>) (criterio vig1) (criterio vig2)

nombreDelSalvador::Vigilante->Int
nombreDelSalvador = (length.nombre)

elElegido::Vigilante->Int
elElegido = (length.habilidades)

patriarca::Vigilante->Int
patriarca = ((2019-).anioAparicion)
