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
}

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
esComplejo unPlato = length(componentes unPlato) > 5 && dificultad unPlato > 7

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
