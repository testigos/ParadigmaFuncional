{-
Candidato se busca
La aplicación que en estos días desean todas las fuerzas políticas para armar su fórmula presidencial.
Conocemos cierta información de los dirigentes políticos, tales como su nombre, su imagen positiva, sus habilidades, su cargo actual y el 
partido politico al que pertenece.
data Dirigente = UnDirigente { …
dirigentes = [ … ]
Los partidos politicos arman alianzas electorales con otros partidos, y juntos determinan su cantidato a presidente y vice.
Ejemplos de alianzas electorales:
cambiemos = ["pro","ucr","cc"]
frenteDeIzquierda = ["po","pcr"]
...
Hay diferentes modos de construccion política, por ejemplo:
El dirigente con mayor cargo actual como presidente (determinar de alguna manera la jerarquía de cargos actuales, por ejemplo, que el 
cargo de presidente es más que el de gobernador y que el de gobernador es más que el de concejal) y cualquiera que tenga más de 20 puntos
 de imagen positiva como vice.
El dirigente de mayor imagen positiva como presidente y cualquier gobernador que entre sus habilidades tenga "honestidad" como vice.
El dirigente de mayor imagen positiva como vice y el que tiene más habilidades como presidente.
1) Hacer la función que permita, a partir de una alianza electoral, un modo de construcción política y una lista con dirigentes de cualquier partido,
   obtener cómo queda conformada la fórmula, eligiendo los candidatos entre los dirigentes que pertenzcan a los diferentes partidos que conforman 
   dicha alianza electoral.
2) Definir los modos de construcción politica descriptos e inventar uno nuevo a elección, aprovechando funciones ya definidas.
3) Completar datos de ejemplo y mostrar algunas consulta posibles, donde la misma alianza con diferentes modos de construcción obtiene 
diferentes fórmulas.
4) Justificar si fue útil el concepto de aplicacíon parcial y composición.
5) Explicar qué sucede si la lista de dirigentes es infinita: Si es posible, encontrar al menos una función de las desarrolladas que 
   permita obtener respuesta y otra que no. Justificar.
Notas:
Puede suceder que una misma persona quede como candidata a presidente y a vice, no se requiere validarlo.
Puede suceder que la alianza política no encuentre ningún dirigente para alguno de los lugares de la fórmula y se produzca un error.
Si hay más de un dirigente que cumple con los requisitos buscados (tener el mejor cargo, ser goberandor honesto, tener mayor imagen positiva, etc.)
cualquiera de ellos puede ser el elegido para ocupar su lugar en la fórmula.
-}

import Text.Show.Functions

type Dirigentes = [Dirigente]
type AlianzaPolitica = [String]
type Formula = (Dirigente,Dirigente)

data Dirigente = UnDirigente {
    nombre::String,
    imagenPositiva::Int,
    habilidades::[String],
    cargoActual::String,
    partidoPolitico::String
} deriving (Show)

--DIRIGENTES
macri = UnDirigente {
    nombre = "Macri",
    imagenPositiva = 30,
    habilidades = ["ganarle a Cristina", "honestidad"],
    cargoActual = "Presidente",
    partidoPolitico = "pro"
}

michetti = UnDirigente {
    nombre = "Michetti",
    imagenPositiva = 20,
    habilidades = ["discutir", "equivocarse en entrevistas"],
    cargoActual = "Vicepresidente",
    partidoPolitico = "pro"
}

fernandez = UnDirigente {
    nombre = "Fernandez",
    imagenPositiva = 20,
    habilidades = ["discutir", "equivocarse en entrevistas"],
    cargoActual = "Diputado",
    partidoPolitico = "pj"
}

cristina = UnDirigente {
    nombre = "Cristina",
    imagenPositiva = 0,
    habilidades = ["hablar indefinidamente","carisma"],
    cargoActual = "Senador",
    partidoPolitico = "kirchnerismo"
}

carrio = UnDirigente {
    nombre = "Carrio",
    imagenPositiva = 40,
    habilidades = ["hablar indefinidamente","honestidad","carisma"],
    cargoActual = "Senador",
    partidoPolitico = "ucr"
}

nacca = UnDirigente {
    nombre = "Naccachian",
    imagenPositiva = 50,
    habilidades = ["saber programar","gustarle matematica"],
    cargoActual = "Alumno",
    partidoPolitico = "pro"
}

massa = UnDirigente {
    nombre = "Massa",
    imagenPositiva = 50,
    habilidades = ["cambiar de partido","tratar de gustarle a todos"],
    cargoActual = "Diputado",
    partidoPolitico = "pj"
}

--ALIANZAS Y LISTA DE DIRIGENTES
cambiemos::AlianzaPolitica
cambiemos = ["pro","ucr","cc"]

partidoJusticialista::AlianzaPolitica
partidoJusticialista = ["pj","kirchnerismo"]

dirigentes::Dirigentes
dirigentes = [massa,nacca,macri,michetti,carrio,fernandez,cristina]

--1) GENERAR FORMULA

generarFormula::AlianzaPolitica->(Dirigentes->Formula)->[Dirigente]->Formula
generarFormula alianzaPolitica modoDeConstruccionPolitica = (modoDeConstruccionPolitica.(dirigentesDeLaAlianza alianzaPolitica)) 

dirigentesDeLaAlianza::AlianzaPolitica->Dirigentes->Dirigentes
dirigentesDeLaAlianza alianzaPolitica = (filtradoSegun (criterioAlianza alianzaPolitica))

criterioAlianza::AlianzaPolitica->Dirigente->Bool
criterioAlianza alianzaPolitica = ((flip pertenece alianzaPolitica).partidoPolitico)

--2) MODO DE CONSTRUCCION POLITICA: En esta funcion se toman todos los dirigentes de la lista, debido a que esta funcion es unicamente utilizada por
--                                  generarFormula que envía como parametro a esta funcion la lista ya filtrada por alianza.
modoDeConstruccionPolitica::(Dirigentes->Dirigente)->(Dirigentes->Dirigente)->Dirigentes->Formula
modoDeConstruccionPolitica criterioDeEleccionPresidente criterioDeEleccionVice dirigentes = (criterioDeEleccionPresidente dirigentes, criterioDeEleccionVice dirigentes)

--MODOS DE CONSULTA (Explicitados para simplificar la prueba de generarFormula)

mayorCargomayorIPos::(Dirigentes->Formula)
mayorCargomayorIPos = (modoDeConstruccionPolitica (mayorDirigenteSegun criterioCargos) (primeroFiltradoSegun criterioMas20ImagenPositiva))

mayorIPosmasHonesto::(Dirigentes->Formula)
mayorIPosmasHonesto = (modoDeConstruccionPolitica (mayorDirigenteSegun imagenPositiva) (primeroFiltradoSegun (criterioTieneHabilidad "honestidad")))

masHabmayorIPos::(Dirigentes->Formula)
masHabmayorIPos = (modoDeConstruccionPolitica (mayorDirigenteSegun cantHabilidades) (mayorDirigenteSegun imagenPositiva))

modoInventado::(Dirigentes->Formula)
modoInventado = (modoDeConstruccionPolitica (primeroFiltradoSegun (criterioTieneHabilidad "carisma")) (mayorDirigenteSegun cantHabilidades))

{-
3)
EJEMPLOS DE RESULTADOS
>generarFormula cambiemos mayorCargomayorIPos dirigentes
 (UnDirigente {nombre = "Macri", imagenPositiva = 30, habilidades = ["ganarle a Cristina","honestidad"], cargoActual = "Presidente", 
 partidoPolitico = "pro"},UnDirigente {nombre = "Naccachian", imagenPositiva = 50, habilidades = ["saber programar","gustarle matematica"], 
 cargoActual = "Alumno", partidoPolitico = "pro"})
>generarFormula cambiemos mayorIPosmasHonesto dirigentes
  (UnDirigente {nombre = "Naccachian", imagenPositiva = 50, habilidades = ["saber programar","gustarle matematica"], cargoActual = "Alumno", 
  partidoPolitico = "pro"},UnDirigente {nombre = "Macri", imagenPositiva = 30, habilidades = ["ganarle a Cristina","honestidad"], 
  cargoActual = "Presidente", partidoPolitico = "pro"})
>generarFormula cambiemos masHabmayorIPos dirigentes
  (UnDirigente {nombre = "Carrio", imagenPositiva = 40, habilidades = ["hablar indefinidamente","honestidad","carisma"], cargoActual = "Senador",
   partidoPolitico = "ucr"},UnDirigente {nombre = "Naccachian", imagenPositiva = 50, habilidades = ["saber programar","gustarle matematica"], 
   cargoActual = "Alumno", partidoPolitico = "pro"})
>generarFormula cambiemos modoInventado dirigentes
  (UnDirigente {nombre = "Carrio", imagenPositiva = 40, habilidades = ["hablar indefinidamente","honestidad","carisma"], cargoActual = "Senador", 
  partidoPolitico = "ucr"},UnDirigente {nombre = "Carrio", imagenPositiva = 40, habilidades = ["hablar indefinidamente","honestidad","carisma"], 
  cargoActual = "Senador", partidoPolitico = "ucr"})
Un ejemplo con otra alianza,
>generarFormula partidoJusticialista mayorCargomayorIPos dirigentes
  (UnDirigente {nombre = "Cristina", imagenPositiva = 0, habilidades = ["hablar indefinidamente","carisma"], cargoActual = "Senador", 
  partidoPolitico = "kirchnerismo"},UnDirigente {nombre = "Massa", imagenPositiva = 50, habilidades = ["cambiar de partido","tratar de gustarle a todos"], 
  cargoActual = "Diputado", partidoPolitico = "pj"})
-}

--FUNCIONES NECESARIAS PARA LOS CRITERIOS DE LOS MODOS DE CONSTRUCCION POLITICA

primeroFiltradoSegun::(Dirigente->Bool)->Dirigentes->Dirigente
primeroFiltradoSegun criterio = (head.filtradoSegun criterio)

filtradoSegun::(Dirigente->Bool)->Dirigentes->Dirigentes
filtradoSegun criterio = (filter criterio)

criterioTieneHabilidad::String->Dirigente->Bool
criterioTieneHabilidad habilidad = ((pertenece habilidad).habilidades)

criterioMas20ImagenPositiva::Dirigente->Bool
criterioMas20ImagenPositiva = ((>20).imagenPositiva)

pertenece::String->[String]->Bool
pertenece palabra = elem palabra

mayorDirigenteSegun::Ord a => (Dirigente->a)->Dirigentes->Dirigente
mayorDirigenteSegun criterio = (foldl1 (comparacionSegun criterio))

comparacionSegun:: Ord b => (Dirigente->b)->Dirigente->Dirigente->Dirigente
comparacionSegun criterio dir1 dir2 | (>) (criterio dir1) (criterio dir2) = dir1
                                    | otherwise = dir2

cantHabilidades::Dirigente->Int
cantHabilidades = (length.habilidades)

criterioCargos::Dirigente->Int
criterioCargos = (valorCargo.cargoActual)

valorCargo::String->Int
valorCargo "Presidente" = 10
valorCargo "Vicepresidente" = 9
valorCargo "Senador" = 8
valorCargo "Diputado" = 7
valorCargo "Gobernador" = 6
valorCargo x = 0

{-
4) El concepto de aplicacion fue muy útil gracias a que me posibilitó mandarle algun parámetro a una funcion. Gracias a aplicacion superior,
lo anterior mencionado, generaba una funcion que luego se utilizaba luego en funciones de orden superior. Por ejemplo en la funcion mayorSegunCriterio
utilizo aplicacion parcial para mandarle a la funcion comparacionSegun el criterio necesario para que esta pueda comparar entre dos dirigentes.
La utilidad de la composicion en este ejercicio otorga una mayor claridad a la hora de escribir las funciones, ya que permite evitar
una gran cantidad de parentesis. Tambien me permitió abstraerme mejor por ejemplo para la funcion filtrarSegun se necesita un criterio y una lista de
dirigentes. Gracias a la composición pude hacer criterios que tomando al mismo dirigente evaluan distintas cosas del mismo por ejemplo sus habilidades o su
imagen positiva. Si en cambio no hubiese tenido esta posibilidad hubiese tenido que hacer dos filters distintos ya que uno evaluaria una lista de strings
y el otro una lista de enteros
5) 
Si la lista de dirigentes es infinita el programa va a evaluar hasta que encuentro uno que encuentre cada condicion. Si no lo encuentra esto va a seguir
evaluandose infitiamente, que es lo que sucedería con cada uno de los modos de construcción política.
En su gran mayoria las funciones no van a devoler un resultado por ejemplo mayorSegunCriterio con criterio de los cargos va a infinitamente comparar a los
dirigentes segun su cargo, sin nunca retornar un resultado.
Entre las funciones desarrolladas no hay ninguna que tome una lista de dirigentes infinita y pueda obtener un resultado.
-}
