module Library where
import PdePreludat

data Guantelete = Guantelete {
material :: Material,
gemas :: [Gema]
}deriving (Show)

data Personaje = Personaje {
nombre :: Nombre,
edad :: Edad,
energia :: Energia,
habilidades ::  [Habilidad],
planeta :: Planeta 
}deriving (Show)
--------------------------------------------------
type Universo = [Personaje]
type Gema = Personaje -> Personaje
type Material = String
type Nombre = String
type Edad = Number
type Energia = Number
type Habilidad = String
type Planeta = String

-------------------------------------------

modificarEnergia :: (Number -> Number) -> Personaje -> Personaje
modificarEnergia funcion personaje = personaje{energia= funcion.energia$personaje}

guanteleteFull :: Guantelete
guanteleteFull = Guantelete "uru" []

guanteleteOtro :: Guantelete
guanteleteOtro = Guantelete "banfield" []

ironMan :: Personaje
ironMan = Personaje "Iron man" 56 500 ["Laser","Hola"] "Tierra"

drStrange :: Personaje
drStrange = Personaje "Dr Strange" 64 100 ["Magia"] "Tierra"

viuda :: Personaje
viuda = Personaje "Viuda" 32 100 ["Magia","Hola"] "Tierra"

hulk :: Personaje
hulk = Personaje "hulk" 84 100 ["Romper"] "Tierra"

capitanAmerica :: Personaje
capitanAmerica = Personaje "Capitan America" 42 880 ["Escudo"] "Tierra"



universo1 :: Universo
universo1 = [ironMan,drStrange,viuda,capitanAmerica]


chasquidoDeUnUniverso :: Guantelete -> Universo -> Universo
chasquidoDeUnUniverso guantelete universo | estaCompleto guantelete = take ((length universo `div` 2)) universo
                                          | otherwise = id universo
estaCompleto :: Guantelete -> Bool
estaCompleto guantelete = length (gemas guantelete) == 6 && material guantelete == "uru"

universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex universo = any menorDe45Años universo 

menorDe45Años :: Personaje -> Bool
menorDe45Años = (<45).edad

energiaTotal :: Universo -> Number
energiaTotal = sum.map energia.personajesConMasDeUnaHabilidad

personajesConMasDeUnaHabilidad :: Universo -> [Personaje]
personajesConMasDeUnaHabilidad universo = filter poseeMasDeUnaHabilidad universo

poseeMasDeUnaHabilidad :: Personaje -> Bool
poseeMasDeUnaHabilidad = (>1).length.habilidades


gemaLaMente :: Energia -> Gema
gemaLaMente numeroARestar = modificarEnergia (subtract numeroARestar) 

gemaElAlma :: Habilidad -> Gema
gemaElAlma habilidad = modificarEnergia (subtract 10).quitarHabilidad (mismoNombre habilidad)


quitarHabilidad :: (Habilidad->Bool) -> Personaje -> Personaje
quitarHabilidad criterio personaje = personaje {habilidades = filter criterio (habilidades personaje)}

mismoNombre :: String -> String -> Bool
mismoNombre palabra habilidad = palabra /= habilidad

gemaElEspacio :: Planeta -> Gema
gemaElEspacio planeta = modificarEnergia (subtract 20).moverPlaneta (++ planeta)

moverPlaneta :: (String -> String) -> Personaje -> Personaje
moverPlaneta funcion personaje = personaje {planeta = funcion.planeta $ personaje}

gemaElPoder :: Gema
gemaElPoder = modificarEnergia (*0).modificarHabilidades


modificarHabilidades :: Personaje -> Personaje
modificarHabilidades personaje | length (habilidades personaje) <=2 = vaciarHabilidades personaje
                               | otherwise = personaje

vaciarHabilidades :: Personaje -> Personaje
vaciarHabilidades personaje = personaje{habilidades = []}

gemaElTiempo :: Gema
gemaElTiempo personaje = (modificarEnergia (subtract 50).modificarEdad (subtract ((edad personaje)`div` 2))) personaje
                     
modificarEdad :: (Number -> Number) -> Personaje -> Personaje
modificarEdad funcion personaje = personaje{edad = (max 18).funcion.edad$ personaje}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema


guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "Goma" [gemaElTiempo,gemaElAlma "Usar Mjolnir",gemaLoca (gemaElAlma "Programacion en Haskell")]


utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl usarGemas enemigo gemas


utilizar' :: [Gema] -> Personaje -> [Personaje]
utilizar' gemas enemigo = map (usarGemas enemigo) gemas

usarGemas :: Personaje -> Gema -> Personaje
usarGemas personaje gema = gema personaje


gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = foldl1 (criterioGanador personaje) (gemas guantelete)

criterioGanador:: Personaje -> Gema -> Gema -> Gema
criterioGanador personaje gema1 gema2 | energia (gema1 personaje) < energia (gema2 personaje) = gema1
                                      | otherwise = gema2



infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas gemaElTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- NOTA = 10 (DIEZ)




