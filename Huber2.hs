import Text.Show.Functions

type Fecha = (Int,Int,Int)
data Chofer = UnChofer {
    nombreCh::String,
    kilometraje::Float,
    viajes::[Viaje],
    condicion:: (Viaje->Bool)
} deriving (Show)

data Viaje = UnViaje {
    fecha::Fecha,
    cliente::Cliente,
    costo::Float
} deriving (Show)

data Cliente = UnCliente {
    nombreCl::String,
    dondeVive::String
} deriving (Show)

cualquierViaje::Viaje->Bool
cualquierViaje viaje = True

soloViajesCaros::Viaje->Bool
soloViajesCaros = (>200).costo

soloClientes::Int->Viaje->Bool
soloClientes n = ((>=n).length.nombreCl.cliente)

soloZonasLindas::String->Viaje->Bool
soloZonasLindas zonaProhibida = ((/=zonaProhibida).dondeVive.cliente)

lucas = UnCliente {
    nombreCl = "Lucas",
    dondeVive = "Victoria"
}

daniel = UnChofer {
    nombreCh = "Daniel",
    kilometraje = 23500,
    viajes = [viaje1Daniel],
    condicion = (soloZonasLindas "Olivos")
}

alejandra = UnChofer {
    nombreCh = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = cualquierViaje
}

viaje1Daniel = UnViaje {
    fecha = (20,04,2017),
    cliente = lucas,
    costo = 150
}

nitoInfy = UnChofer {
    nombreCh = "Nito Infy",
    kilometraje = 70000,
    viajes = (repetir viajeInf),
    condicion = (soloClientes 3)
}

viajeInf = UnViaje {
    fecha = (11,03,2017),
    cliente = lucas,
    costo = 50
}

repetir::Viaje->[Viaje]
repetir viaje = viaje : repetir viaje

puedeTomarViaje::Chofer->Viaje->Bool
puedeTomarViaje chofer = (condicion chofer)

liquidacion::Chofer->Float
liquidacion = (sum.map (costo).viajes)

realizarUnViaje::Viaje->[Chofer]->Chofer
realizarUnViaje viaje = ((agregarViaje viaje).foldl1 (compararViajes).filter ((tomarViaje viaje).condicion))

agregarViaje::Viaje->Chofer->Chofer
agregarViaje viaje (UnChofer nom kil viajes cond) = UnChofer nom kil (viajes ++ [viaje]) cond

compararViajes::Chofer->Chofer->Chofer
compararViajes ch1 ch2 | (length.viajes) ch1 > (length.viajes) ch2 = ch2
                       | otherwise = ch1

tomarViaje::Viaje->(Viaje->Bool)->Bool
tomarViaje viaje condicion = condicion viaje

{-
No se puede calcular la liquidacion ya que los costos infinitos.
Si ese viaje se encuentra en la lista lo podrá encontrar, pero si este no se encuentrá va a estar infinitamente buscandolo.
-}

gongNeng:: Ord a=>a->(a->Bool)->(b->a)->[b]->a
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3