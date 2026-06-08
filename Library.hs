module Library where
import PdePreludat

-- PARTE A
data Participante = UnParticipante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    platoDeEspecialidad :: Plato
} 

data Plato = UnPlato {
    dificultad :: Number, --Recordar que va de 0 a 10
    componentes :: Componentes
} deriving (Eq, Show)

type Ingrediente = String
type PesoEnGramos = Number
type Truco = Plato -> Plato
type Componente = (Ingrediente, PesoEnGramos)
type Componentes = [Componente]
type Informacion = Plato -> Bool

-- FUNCIONES AUXILIARES
duplicarComponente::Componente -> Componente
duplicarComponente (ingrediente, peso) = (ingrediente, peso * 2)

modificarDificultad::(Number -> Number) -> Plato -> Plato
modificarDificultad unaFuncion unPlato = unPlato{dificultad = unaFuncion (dificultad unPlato)}

esPesado::Componente -> Bool
esPesado componente = snd componente >= 10

quitarComponentesLivianos::Plato -> Plato
quitarComponentesLivianos unPlato = unPlato{componentes = filter esPesado (componentes unPlato)}

listarIngredientes::Plato -> [Ingrediente]
listarIngredientes unPlato = map fst (componentes unPlato)

contieneIngrediente::Ingrediente->Plato->Bool
contieneIngrediente unIngrediente unPlato = elem unIngrediente (listarIngredientes unPlato)

cantidadDeGramosDe::Ingrediente -> Plato -> Number
cantidadDeGramosDe ingredienteBuscado unPlato = sum . map snd . filter ((==ingredienteBuscado) . fst) $ (componentes unPlato) 

-- TRUCOS PARTE A

{- La primer idea que tuve para la funcion endulzar fue algo así pero era realmente un quilombo pq habia que crear las funciones existeIngrediente, modificarPeso, agregarIngredientes.
 Creo que es mas facil agregar como "Precondición" que el plato no tenga azucar y suponemos que los platos cumplen esta precondicion.
endulzar::Number -> Truco
endulzar cantGramosAzucar unPlato
|existeIngrediente "azucar" (componentes unPlato) == True = modificarPeso (+cantGramosAzucar) "azucar" (componentes unPlato)
|existeIngrediente "azucar" (componentes unPlato) == False = agregarIngredientes("azucar",cantGramosAzucar) unPlato
-} 
endulzar::Number->Truco
endulzar cantidadAzucar unPlato = unPlato {componentes = ("azucar", cantidadAzucar) : componentes unPlato}

-- Lo mismo que para endulzar, se supone un plato sin sal. En primera instancia habia hecho lo mismo que con endulzar (verificar si existe el ingrediente, etc.)
salar::Number->Truco
salar cantidadSal unPlato = unPlato {componentes = ("sal", cantidadSal) : componentes unPlato}

darSabor::Number->Number->Truco
darSabor cantidadSal cantidadAzucar unPlato = endulzar(cantidadAzucar).salar(cantidadSal) $ unPlato

duplicarPorcion::Truco
duplicarPorcion unPlato = unPlato {componentes = map duplicarComponente (componentes unPlato)}

simplificar::Truco
simplificar unPlato
    |esComplejo unPlato = modificarDificultad(const 5).quitarComponentesLivianos $ unPlato
    |otherwise = unPlato

-- Información de los platos
esVegano::Informacion
esVegano unPlato = not (contieneIngrediente "carne" unPlato) && not (contieneIngrediente "huevo" unPlato) && not (contieneIngrediente "lacteo" unPlato)

esSinTacc::Informacion
esSinTacc unPlato =  not(contieneIngrediente "harina" unPlato) 

esComplejo::Informacion
esComplejo unPlato = dificultad unPlato > 7 && length(componentes unPlato) > 5 

noAptoHipertension::Informacion
noAptoHipertension unPlato = cantidadDeGramosDe "sal" unPlato > 2

-- PARTE B
pepeRonccino::Participante
pepeRonccino = UnParticipante {
    nombre = "Pepe Ronccino",
    trucosDeCocina = [darSabor 5 2, simplificar, duplicarPorcion],
    platoDeEspecialidad = platoDeEspecialidadDePepe    
    }
platoDeEspecialidadDePepe::Plato
platoDeEspecialidadDePepe = UnPlato {
    dificultad = 8,
    componentes = [("sal", 3), ("ing1", 100), ("ing2", 100), ("ing3", 100), ("ing4", 100), ("ing5", 100)]
}

-- PARTE C
cocinar::Participante->Plato
cocinar unParticipante = foldl aplicarUnTruco (platoDeEspecialidad unParticipante) (trucosDeCocina unParticipante)

aplicarUnTruco::Plato->Truco->Plato
aplicarUnTruco unPlato unTruco = unTruco unPlato

esMejorQue::Plato->Plato->Bool
esMejorQue plato1 plato2 = (dificultad plato1 > dificultad plato2) && (sumaDePesos plato1 < sumaDePesos plato2)

sumaDePesos::Plato->Number
sumaDePesos unPlato = sum . map snd $ componentes unPlato 

participanteEstrella::[Participante]->Participante
--casoBase
participanteEstrella [participantes] = participantes
--recursivo
participanteEstrella (participante1:participante2:restoDeParticipantes)
    |esMejorQue (cocinar participante1) (cocinar participante2) = participanteEstrella(participante1:restoDeParticipantes)
    |otherwise = participanteEstrella(participante2:restoDeParticipantes)

-- PARTE D
armarComponente::Number->Componente
armarComponente n = ("ingredientes" ++ show n, n)

platinum::Plato
platinum = UnPlato {
    dificultad = 10,
    componentes = map armarComponente[1..]
}

{-
Que pasa si aplicamos los siguientes trucos a Platinum:
- endulzar: Funciona, ya que lo que hace la funcion endulzar es agregar el ingrediente azucar al inicio de la lista infinita de ingredientes
- salar: Funciona, ya que lo que hace la funcion salar es agregar el ingrediente sal al inicio de la lista infinita de ingredientes
- darSabor: Funciona, aplica la funcion salar y endulzar, por lo que agrega ambos ingredientes al inicio de la lista infinita de ingredientes
- duplicarPorcion: No termina, ya que la funcion recorre cada elemento de la lista de ingredientes y duplica su peso, por lo que nunca terminaría de recorrer la lista. Sin embargo, se queda ejecutando la funcion infinitamente.
- simplificar: No termina, ya que nunca terminaria de filtrar los ingredientes pesados de la lista porque nunca terminaría de recorrerla

Que pasa si queremos conocer información del Platinum:
- esVegano: No termina, ya que nunca podria terminar de verificar si contiene algun ingrediente que se llame "carne", "huevo" o "lacteo"
- esSinTacc: No termina, ya que nunca podria terminar de verificar si contiene algun ingrediente que se llame "harina"
- esComplejo: Funciona, ya que una vez que verifica que la dificultad es >7 y la cantidad de componentes del plato es >5, tiraria true en ambos lados del &&
- noAptoHipertension: No termina, ya que tendría que filtrar una lista infinita de ingredientes para ver si tiene sal en el plato

Con respecto a si Platinum es mejor que otro plato, esto va a depender del plato con el que lo estemos comparando, pero en la parte de la función en la que tiene que sumar los pesos, nunca va a terminar de sumar todos los pesos de los ingredientes del platinum. Por lo que no termina.
-}
