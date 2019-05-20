import Text.Show.Functions

type Requisito = Propiedad -> Bool
type Busqueda = [Requisito]

data Propiedad = Propiedad {
    ambientes::Int, 
    superficie::Int, 
    precio::Float, 
    barrio::String
} deriving (Show)

data Usuario = Usuario {
    mail::String, 
    busqueda::Busqueda 
} deriving (Show)

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = x <= z && z <= y

depto1 = Propiedad {
    ambientes = 3,
    superficie = 80,
    precio = 12500,
    barrio = "Palermo"
}

depto2 = Propiedad {
    ambientes = 1,
    superficie = 45,
    precio = 10000,
    barrio = "Villa  Urquiza"
}

depto3 = Propiedad {
    ambientes = 2,
    superficie = 50,
    precio = 16000,
    barrio = "Palermo"
}

depto4 = Propiedad {
    ambientes = 1,
    superficie = 45,
    precio = 10500,
    barrio = "Recoleta"
}

mayorSegun::Ord b=>(a->b)->a->a->Bool
mayorSegun criterio a b = criterio a > criterio b

--Ejemplo
-- ordenarSegun (mayorSegun length) ["aa","Bbbbs","ada"]
--["Bbbbs","ada","aa"]

--EJERCICIO 2
ubicadoEn::[String]->Requisito
ubicadoEn barrios dpto = any (== (barrio dpto)) barrios

cumpleRango::Ord a =>(Propiedad->a)->a->a->Requisito
cumpleRango funcion min max dpto = between min max (funcion dpto)

--EJERCICIO 3
cumpleBusqueda::Propiedad->Busqueda->Bool
cumpleBusqueda prop requisitos = all ($ prop) requisitos

cumpleBusquedaOrdenados::Busqueda->(Propiedad->Propiedad->Bool)->[Propiedad]->[Propiedad]
cumpleBusquedaOrdenados busqueda criterioOrden = (ordenarSegun (criterioOrden). filter (flip cumpleBusqueda busqueda))

--EJEMPLO
requisitos::Busqueda
requisitos = [(ubicadoEn ["Recoleta","Palermo"]), (cumpleRango ambientes 1 2), (cumpleRango precio 8000 15000)]

propiedades::[Propiedad]
propiedades = [depto1,depto2,depto3,depto4]

{-
cumpleBusquedaOrdenados requisitos (mayorSegun superficie) propiedades
>[Propiedad {ambientes = 1, superficie = 45, precio = 10500.0, barrio = "Recoleta"}]
-}

--EJERCICIO 4

p1 = Usuario {
    mail = "a..asdasd",
    busqueda = requisitos
}

mailsDePersonasInteresadas::Propiedad->[Usuario]->[String]
mailsDePersonasInteresadas dpto = (map mail.interesados dpto)

interesados::Propiedad->[Usuario]->[Usuario]
interesados dpto = (filter ((cumpleBusqueda dpto).busqueda))