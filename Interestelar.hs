import Text.Show.Functions

data Planeta = UnPlaneta {
    nombrePl::String,
    posicion::Posicion,
    tiempo::(Int->Int)
}

type Posicion = (Float, Float, Float)
cX (x,_,_) = x
cY (_,y,_) = y
cZ (_,_,z) = z

data Astronauta = UnAstronauta {
    nombreA::String,
    edad::Int,
    planeta::Planeta
}

--EJERCICIO 1
distancia::Planeta->Planeta->Float
distancia planeta1 planeta2 = sqrt((((cX.posicion) planeta1) - ((cX.posicion) planeta2))^2+ (((cY.posicion) planeta1) - ((cY.posicion) planeta2))^2+(((cZ.posicion) planeta1) - ((cZ.posicion) planeta2))^2)

tiempoSegunVelocidad::Float->Planeta->Planeta->Float
tiempoSegunVelocidad v p1 p2 = (distancia p1 p2)/v

--EJERCICIO 2

--EJERCICIO 3

naveVieja::Int->Planeta->Planeta->Float
naveVieja tquesO planeta1 planeta2 | tques0<6 = tiempoSegunVelocidad 10 p1 p2
                                   | otherwise = tiempoSegunVelocidad 7 p1 p2

naveFuturista::