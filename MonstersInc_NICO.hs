import Text.Show.Functions

--EJERCICIO 1

data Grito = UnGrito {
    onomatopeya::String,
    intensidad::Int,
    mojoLaCama::Bool
}deriving (Show)

energiaDeUnGrito::Grito->Int
energiaDeUnGrito grito | mojoLaCama grito = nivelTerror grito * (intensidad grito)^2
                       | otherwise = ((3*).nivelTerror) grito
                                
nivelTerror::Grito->Int
nivelTerror = length.onomatopeya

--EJERCICIO 2

data Chico = UnChico {
    nombre::String,
    edad::Int,
    altura::Float
} deriving (Show)

type Monstruo = (Chico->Grito)
type Monstruos = [(Chico->Grito)]

sullivan::Chico->Grito
sullivan chico = UnGrito ((nombreGrito.nombre) chico) ((intensidadGrito.edad) chico) ((mojarDelGrito.edad) chico)
                      where nombreGrito nombre = ((++"GH").concat.take (length nombre).(iterate (++"A"))) []
                            intensidadGrito = (div 20)
                            mojarDelGrito = (<3)

rangallBoggs::Chico->Grito
rangallBoggs chico = UnGrito "¡Mamadera!" ((intensidadGrito.nombre) chico) ((mojarDelGrito.altura) chico)
                      where intensidadGrito = cantVocales
                            mojarDelGrito x = x>0.8 && x<1.2

cantVocales::String->Int
cantVocales = (length.filter (compararVocales ['a','e','i','o','u']))

compararVocales::[Char]->Char->Bool
compararVocales vocales letra = elem (letra) vocales

norris::Chico->Grito
norris chico = UnGrito ['a'..'z'] 1000 True

--EJERCICIO 3

pam::[(a->b)]->a->[b]
pam funciones elemento = map ($ elemento) funciones

--EJERCICIO 4

gritos::Monstruos->Chico->[Grito]
gritos monstruos chico = pam monstruos chico

--EJERCICIO 5

produccionEnergeticaGritos::Monstruos->[Chico]->Int
produccionEnergeticaGritos monstruos = (calculoEnergia.(listaDeGritos monstruos))

calculoEnergia::[Grito]->Int
calculoEnergia = (sum. map intensidad)

listaDeGritos::Monstruos->[Chico]->[Grito]
listaDeGritos monstruos = (concat.map (gritos monstruos))

--EJERCICIO 6

data Risa = UnaRisa {
    duracion::Int,
    intensidadR::Int
} deriving (Show)

energiaRisa::Risa->Int
energiaRisa (UnaRisa d i) = d^i

type Comediante = (Chico->Risa)
type Comediantes = [Comediante]

capusotto::Chico->Risa
capusotto (UnChico _ e _) = UnaRisa (duracionRisa e) (duracionRisa e)
                     where duracionRisa = (*2)

produccionEnergeticaRisa::Comediantes->[Chico]->Int
produccionEnergeticaRisa comediantes = (calculoEnergiaRisa.(listaDeRisas comediantes))
                     
calculoEnergiaRisa::[Risa]->Int
calculoEnergiaRisa = (sum. map intensidadR)
                     
listaDeRisas::Comediantes->[Chico]->[Risa]
listaDeRisas comediantes = (concat.map (risas comediantes))

risas::Comediantes->Chico->[Risa]
risas comediantes chico = pam comediantes chico

--EJERCICIO 7

data Productor = Monstruo | Comediante