-- Autor: Franco Daneri
-- Numero de estudiante: 260284
-- Autor: Tomas Clavijo
-- Numero de estudiante: 235426

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Prog where

type Var = String

data Exp where { V    :: Var -> Exp;
				 I    :: Int -> Exp;
				(:+)  :: Exp -> Exp -> Exp;
				(:-)  :: Exp -> Exp -> Exp;
				(:*)  :: Exp -> Exp -> Exp ;
				(:&&) :: Exp -> Exp -> Exp;
				(:||) :: Exp -> Exp -> Exp;
				 Not   :: Exp -> Exp;
				(:==) :: Exp -> Exp -> Exp}
			deriving(Eq,Show)

-- Se define que :* tiene mayor precedencia que :+ y :-
infixl 8 :*
infixl 6 :+
infixl 6 :-
-- Se define que :&& tiene mayor precedencia que :||
infixl 7 :&&
infixl 6 :||
-- :== tiene la menor precedencia
infixl 4 :==

type Memoria = [(Var,Int)]

data Prog where {
    Asig  :: [(Var,Exp)] -> Prog;
    (:>)  :: Prog -> Prog -> Prog;
    Cond  :: [(Exp,Prog)] -> Prog;
    While :: Exp -> Prog -> Prog  }
  deriving(Eq,Show)

-- Se define que :> tiene la menor precedencia
infixr 3 :>

-- Memoria y evaluación de expresiones
-- 1 
(@@) :: Var -> Memoria -> Int
(@@) = \v xs -> case xs of {
							[] -> error "La variable no se encuentra en la memoria";
							y : ys -> case y of {
												(i, j) -> case v == i of {
																		True -> j;
																		False -> v @@ ys
											}
						}
}

-- 2
upd :: (Var,Int) -> Memoria -> Memoria
upd = \x xs -> case xs of {
						[] -> x : [];
						y : ys -> case y of {
											(i, j) -> case i == (fst x) of {
																		True -> x : ys;
																		False -> y:(upd x ys)
																	};
										}
					}

-- 3
eval :: Exp -> Memoria -> Int
eval = \exp xs -> case exp of {
								(V exp1) -> case xs of {[] -> exp1 @@ xs ; y : ys -> exp1 @@ xs};
								(I exp1) ->exp1 ;
								(exp1 :+ exp2) -> (eval exp1 xs) + (eval exp2 xs);
								(exp1 :- exp2) -> (eval exp1 xs) - (eval exp2 xs);
								(exp1 :* exp2) -> (eval exp1 xs) * (eval exp2 xs);
								(exp1 :&& exp2) -> case ((eval exp1 xs) == 0) of {False -> case ((eval exp2 xs) == 0) of {False -> 1 ; True -> 0} ; True -> 0};
								(exp1 :|| exp2) -> case ((eval exp1 xs) == 0) of {False -> 1 ; True -> case ((eval exp2 xs) == 0) of {False -> 1 ; True -> 0}};
								(Not exp1) -> case ((eval exp1 xs) == 0) of {True -> 1 ; False -> 0};
								(exp1 :== exp2) -> case ((eval exp1 xs) == (eval exp2 xs)) of{True -> 1; False -> 0};
						};

{-
	Esta funcion recibe por parametro, la lista de variables ya evaluadas y la 
	memoria, con el objetivo de actualizarla en funcion de las mismas.
-}

asignarAux :: [(Var, Int)] -> Memoria -> Memoria
asignarAux = \xs m -> case xs of {
							[] -> m;
							y : ys -> asignarAux ys (upd (y) m);
						}

{-
	Esta funcion recibe la lista de variables con la expresion a evaluar y la memoria, con el objetivo de evaluar las expresiones
	de cada variable en funcion de los datos que haya en la memoria y asi devolver una lista de variables ya evaluadas.
-}

evaluarAux :: [(Var, Exp)] -> Memoria -> [(Var, Int)]
evaluarAux = \xs m -> case xs of {
							[] -> [];
							y : ys -> case y of {
												(i, j) -> (i,(eval j m)) : (evaluarAux ys m)
						};
}

-- Ejecución de programas
-- 4
run :: Prog -> Memoria -> Memoria
run = \p m -> case p of {
							(Asig x) -> asignarAux(evaluarAux x m) m;
							(prog1 :> prog2) -> run prog2 (run prog1 m);
							(Cond x) -> case x of {
													[] -> m;
													y : ys -> case y of {
																		(i, j) -> case (eval i m) == 0 of {True -> (run (Cond ys) m); False -> run (j) (m)}
																		};
												};
							(While exp1 prog1) -> case ((eval exp1 m) == 0) of {True -> m; False -> run (While (exp1) (prog1)) (run prog1 m)};
}

-- Ejemplos de programas

p0 :: Prog
p0 = Asig [("x", I 1)] :> Asig [("x", V "x" :+ I 10)]


p1 :: Prog
p1 = Asig [("x" , I 2), ("y", I 2)]
     :> Cond [(I 0, Asig [("z", I 10)]),
              (I 1, Asig [("z", I 0)])]

p3 :: Prog
p3 = Asig [("x" , I 1), ("y", I 10)] :> While (V "y") (Asig [("x", V "x" :+ I 1)] :> Asig [("y", V "y" :- I 2)])

p4 :: Prog
p4 = Asig [("x" , I 1987), ("y", I 265)] :> Asig [("xx" , I 0), ("yy", I 101)] :> Asig [("xx" , V "z"), ("yyy", I 101346)]

p5 :: Prog
p5 = Asig [("x" , I 1987), ("y", I 265),("y", I 265)]

p7:: Prog
p7 = Cond [(I 0, Asig [("z", I 10)]),(I 0, Asig [("xx", I 456)]),(I 0, Asig [("yyy", I 111)]),(I 0, Asig [("xx", I 3331)])]

p2 :: Prog
p2 = Asig [("x" , I 27), ("y", I 5)] 
     :> While (V "x") (Asig [("y", V "y" :+ I 2)] :> Asig [("x", V "x" :- V "y")])

-- Programas a definir

-- 5
swap:: Prog
swap = Asig [("x", V "y"), ("y", V "x")]


-- 6
fact :: Int -> Prog
fact = \n -> Asig [("fact", I n)] :> Asig [("x", I n :- I 1)]  :> Cond[((V "fact"), Cond[(V "x", (While (V "x") (Asig [("fact", V "fact" :* V "x"), ("x", V "x" :- I 1)])))])] :> Cond[(Not(V "fact"), (Asig [("fact", I 1)]))]

-- 7

par :: Int -> Prog
par = \n -> Asig [("par", I n),("contador", I n)] :> (While (V "par" :&& V "contador") (Asig [("par", V "par" :- I 2),("contador", V "contador" :- I 1)])) :> Cond [(V "par", Asig[("par", I 0)]),(Not(V "par"), Asig[("par", I 1)])]

-- 8
mini :: Int -> Int -> Prog
mini = \m n -> case m < n of {True -> (Asig [("min", I m)]); False -> (Asig [("min", I n)])}

-- 9
fib :: Int -> Prog
fib = \n -> Asig [("n1", I 1),("n2", I 0),("contador", I 1),("fib", I 1)] :> While (Not(V "contador" :== I n)) (Asig [("fib", V "n1" :+ V "n2"),("n1", V "n1" :+ V "n2"), ("n2", V "n1"), ("contador", V "contador" :+ I 1)])   



-- Para probar los programas fact, par, mini y fib recomendamos utilizar las siguientes funciones:

factorial = \n -> pre (n >= 0) "fact: argumento negativo"   ("fact" @@ run (fact n) [])

esPar = \n -> pre (n >= 0) "par: argumento negativo"  ("par" @@ (run (par n) []) /= 0)

minimo = \m n -> pre (m >= 0 && n >= 0) "mini: argumento negativo"   ("min" @@ run (mini m n) [])

fibonacci = \n -> pre (n >= 0) "fib: argumento negativo"   ("fib" @@ run (fib n) [])


-- Se usa pre para poder verificar que los argumentos sean válidos

pre :: Bool -> String -> a -> a 
pre = \b s x -> case b of {False -> error s ; True -> x}