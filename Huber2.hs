import Text.Show.Functions

type Fecha = (Int,Int,Int)
data Chofer = UnChofer {
    nombre::String,
    kilometraje::Float,
    viajes::[Viaje],
    condicion:: ()
}

data Viaje = UnViaje {
    fecha::Fecha,
    cliente::Cliente,
    costo::Float
}