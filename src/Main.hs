-- Tipos de datos

-- Listas

-- [] <- la lista vacia 
-- (x:OtraLista) <- una lista, con cabeza = x y cola = OtraLista
-- Ejemplo
-- (2:[3, 4]) == [2,3,4] es una lista con cabeza = 2 y cola = [3, 4]


largoDeUnaLista [] = 0
largoDeUnaLista (unElemento : otraLista) = 1 + largoDeUnaLista otraLista

-- usando head, sin pattern matching
inicialDeNombre :: [a] -> a
inicialDeNombre nombre = head nombre
-- usando !! para acceder al elemento en la posicion 0
inicialDeNombre2 :: [a] -> a
inicialDeNombre2 nombre = nombre !! 0
-- usando pattern matching en la lista para extraer el primer elem de la lista.
inicialDeNombre3 (primeraLetra : restoDelString) = primeraLetra

-- [1,2,3]

-- largoDeUnaLista [1,2,3]
-- 1 + largoDeUnaLista [2, 3]
-- 1 + 1 + largoDeUnaLista [3]
-- 1 + 1 + 1 + largoDeUnaLista []
-- 1 +  1 + 1 + 0 = 3

agregarUnElemento x lista = x : lista

-- Tuplas 

alumnoIvan :: (String, Integer, [String])
alumnoIvan = ("Ivan", 12616, ["PDP"])

alumnoGuillermo :: (String, Integer, [String], String)
alumnoGuillermo = ("Guillermo", 112233, ["PDP"], "Anteojos")

primero :: (a, b, c) -> a
primero (primerElemento, _, _) = primerElemento
segundo :: (a, b, c) -> b
segundo (_, segundoElemento, _) = segundoElemento
tercero :: (Int, b, c) -> c
tercero (_, _, tercerElemento) = tercerElemento


primerYUltimoElem :: [b] -> (b, b)
primerYUltimoElem lista = (head lista, last lista)


-- tipos de datos definidos por nosotros
-- Primero modelamos un alumno usando tuplas. 
ivanConTuplas :: (String, Integer, [String])
ivanConTuplas = ("Ivan", 12616, ["PDP"])

-- Una forma alternativa es usando data
 -- agregar deriving Show es necesario para poder mostrar este tipo de datos por consola
data Alumno = UnAlumno String Int [String]
  deriving (Show)

ivanConData :: Alumno
ivanConData = UnAlumno "Ivan" 12616 ["PDP"]

nombreDeAlumno :: Alumno -> String
nombreDeAlumno (UnAlumno "Franco" _ _ ) = "Nombre desconocido"
nombreDeAlumno (UnAlumno nombre _ _) = nombre
legajoDeAlumno :: Alumno -> Int
legajoDeAlumno (UnAlumno nombre legajo listaDeMaterias) = legajo
listaDeMateriasAlumno :: Alumno -> [String]
listaDeMateriasAlumno (UnAlumno _ _ lista) = lista

-- Records
-- otra alternativa es modelar el alumno como un record.
data AlumnoRecord = UnAlumnoR -- que es el constructor? Una funcion que construye valores de este tipo.
  { nombre          :: String
  , legajo          :: Int
  , listaDeMaterias :: [String]
  , metodoDeEstudio :: [String] -> Bool
  }

metodoDeEstudio :: [String] -> Bool
metodoDeEstudioIvan materias = True

ivanConRecord = UnAlumnoR { nombre          = "Ivan"
                          , legajo          = 12616
                          , listaDeMaterias = ["PDP"]
                          , metodoDeEstudio = metodoDeEstudioIvan
                          }

aplicarMetodoDeEstudio :: AlumnoRecord -> String -> Bool
aplicarMetodoDeEstudio (UnAlumnoR _ _ _ metodoDeEstudio) materia = metodoDeEstudio [materia]

usaUnMetodoEficiente :: AlumnoRecord -> Bool
usaUnMetodoEficiente alumno = (metodoDeEstudio alumno) (listaDeMaterias alumno)

-- Ejemplo: Tipo de dato Booleanos

data Booleanos = Verdadero | Falso

conjuncion :: Booleanos -> Booleanos -> Booleanos
conjuncion Verdadero Verdadero = Verdadero
conjuncion Falso Falso = Falso
conjuncion Falso Verdadero = Falso
conjuncion Verdadero Falso = Falso

-- Ejemplo: Metodos de pago

data MetodoDePago = Efectivo {cantidad :: Float} 
                  | TarjetaDeDebito {cuentaBancaria :: Int} 
                  | TarjetaDeCredito {duenio::String, numero::Int, saldoDisponible::Float}

tarjetaDeLucas :: MetodoDePago
tarjetaDeLucas = TarjetaDeDebito {cuentaBancaria = 123}

efectivoDeJuan :: MetodoDePago
efectivoDeJuan = Efectivo 2

puedePagar :: MetodoDePago -> Float -> Bool
puedePagar (Efectivo cantidad) precio = cantidad >= precio
puedePagar (TarjetaDeDebito cuentaBancaria) _ = even cuentaBancaria
puedePagar (TarjetaDeCredito _ _ saldoDisponible) cantidad = saldoDisponible >= cantidad


data FormaGeometrica = Circulo {radio :: Float}
                     | Cuadrado {lado:: Float}
                     | Triangulo {lado1::Float, lado::Float, lado3::Float}


-- Tipos de datos recursivos
-- Un tipo de dato lista definido por nosotros.
-- a es una variable de tipo. Puede ser cualquier tipo: Int, String, Bool, etc.
data Lista a = ListaVacia  -- equivalente a []
             | Lista {cabeza :: a, cola :: Lista a} deriving (Show)

lista123 :: Lista Integer -- equivalente a [1, 2, 3]
lista123 = Lista 1 (Lista 2 (Lista 3 ListaVacia))

cantidadDeElementos :: Lista a -> Int
cantidadDeElementos ListaVacia = 0
cantidadDeElementos (Lista _ cola) = 1 + cantidadDeElementos cola

data Opcional a = Un a | Nada deriving (Show)

primerElem :: [a] -> Maybe a 
primerElem [] = Nothing
primerElem (x:_) = Just x

-- Ejemplo arbol binario.

data ArbolBinario =
    Hoja { valor :: Int}
    | Nodo {valor::Int, ladoIzq :: ArbolBinario, ladoDerecho :: ArbolBinario}
    deriving (Show)

-- Creamos una lista con los elementos en el siguiente orden: Raiz, Izq, Der
preorder :: ArbolBinario -> [Int]
preorder (Hoja x) = [x]
preorder nodo = [valor nodo] ++ preorder (ladoIzq nodo) ++ preorder (ladoDerecho nodo)


arbolDeEjemplo :: ArbolBinario
arbolDeEjemplo = Nodo 2 (Hoja 1) (Nodo 3 (Hoja 0) (Hoja 10))