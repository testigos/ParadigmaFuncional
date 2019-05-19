import Text.Show.Functions

data Chofer = Chofer {
    nombre::String,
    kilometraje::Int,
    viajes::[Viaje],
    condicion::Viaje->Bool
} deriving (Show)

instance Eq Chofer where
    chofer1 == chofer2 = (nombre chofer1) == (nombre chofer2)

instance Ord Chofer where
    chofer1 <= chofer2 = ((length.viajes) chofer1) <= ((length.viajes) chofer2)

data Viaje = Viaje {
    fecha::Int,
    cliente::Cliente,
    costo::Int
} deriving (Show)

data Cliente = Cliente {
    nombree::String,
    dondeVive::String
} deriving (Show)

lucas = Cliente {
    nombree = "Lucas",
    dondeVive = "Victoria"
}

daniel = Chofer {
    nombre = "Daniel",
    kilometraje = 23500,
    viajes = [viaje1],
    condicion = (noZona  "Olivos")
}

alejandra = Chofer {
    nombre = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = cualquierViaje
}

viaje1 = Viaje {
    fecha = 20042017,
    cliente = lucas,
    costo = 150
}

cualquierViaje::Viaje->Bool
cualquierViaje viaje = True

soloCaros::Viaje->Bool
soloCaros viaje = costo viaje >= 200

masLetras::Int->Viaje->Bool
masLetras n viaje = (length.nombree.cliente) viaje >= n

noZona::String->Viaje->Bool
noZona zona viaje = (dondeVive.cliente) viaje /= zona

puedeTomarlo::Viaje->Chofer->Bool
puedeTomarlo viaje chofer = (condicion chofer) viaje

liquidacion::Chofer->Int
liquidacion (Chofer nom km viajes cond) = foldl1 (+) (map costo viajes)

realizarViaje::Viaje->[Chofer]->Chofer
realizarViaje viaje choferes = ((efectuarViaje viaje).minimum.(filter (puedeTomarlo viaje))) choferes 

efectuarViaje::Viaje->Chofer->Chofer
efectuarViaje viaje (Chofer nom km viajes cond) = Chofer nom km (viajes ++ [viaje]) cond

repetirViaje::Viaje->[Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

infy = Chofer {
    nombre = "Nino Infy",
    kilometraje = 70000,
    viajes = (repetirViaje viaje2),
    condicion = (masLetras 3)
}

viaje2 = Viaje {
    fecha = 11032017,
    cliente = lucas,
    costo = 50
}

--a. NO
--b. SI

viaje3 = Viaje {
    fecha = 02052017,
    cliente = lucas,
    costo = 500
}

gongNeng::Ord a => a->(a->Bool)->(b->a)->[b]->a
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3