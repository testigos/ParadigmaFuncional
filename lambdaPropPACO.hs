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

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = x <= z && y >= z

mayorSegun::Ord b => (a->b)->a->a->Bool 
mayorSegun f x y = (f x) > (f y)

--ordenarSegun (mayorSegun length) ["pepe","aosjdn","as","asdassdasada"]

