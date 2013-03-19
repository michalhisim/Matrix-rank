-- ------------ Rezie ---------------

type Vektor=[Float] -- lepsi double
type Matice=[Vektor] 

-- Vytvori z jakehokoli vektoru string
vektorNaString :: Vektor -> String
vektorNaString x = concat (map (++" ") (map (show) x))

-- Vytvori z jakekoli matice string; required: vektorNaString
maticeNaString :: Matice -> String
maticeNaString x = concat (map (++"\n") (map (vektorNaString) x))

-- Univerzalni funkce na tisk jakéhokli stringu
tisk :: String -> IO ()
tisk = putStr

-- Tisk matice
tiskMatice :: Matice -> IO ()
tiskMatice = tisk . maticeNaString

-- --------------- operace s radky -----------------

-- overi jestli je radek nulovy
jeNulovy :: Vektor -> Bool
jeNulovy (x:[]) = if (x==0) then True else False
jeNulovy (x:xs) = if (jeNulovy [x]) then (jeNulovy xs) else False

-- nasobeni vektoru konstatntou
nasobVektor :: Vektor -> Float -> Vektor
nasobVektor v k = map (*k) v

-- nasobeni 2 vektoru
nasobVektory :: Vektor -> Vektor -> Vektor
nasobVektory [] [] = []
nasobVektory [] [_] = []
nasobVektory [_] [] = []
nasobVektory (u:us) (v:vs) = (u*v):(nasobVektory us vs)

-- scitani 2 vektoru
sectiVektory :: Vektor -> Vektor -> Vektor
sectiVektory (u:[]) (v:_) = [u+v]
sectiVektory (u:_) (v:[]) = [u+v]
sectiVektory (u:us) (v:vs) = (sectiVektory [u] [v])++(sectiVektory us vs)

-- upraví vektor tak, že první nenulový prvek bude 1
pripravVektor :: Vektor -> Vektor
pripravVektor (v:[]) = nasobVektor (v:[]) (1/v) 
pripravVektor (v:vs) = if (v==0) then (0:(pripravVektor vs)) else nasobVektor (v:vs) (1/v)  

-- Vrátí počet nul na začátku
pocetStartovnichNul :: Vektor -> Int
pocetStartovnichNul (v:[]) = if (v==0) then 1 else 0
pocetStartovnichNul (v:vs) = 0 + (if (pocetStartovnichNul [v])==1 then 1+(pocetStartovnichNul vs) else 0)

-- --------------- operace s maticemi --------------

-- vypusti nulove radky
odstranNuly :: Matice -> Matice
odstranNuly (m:[]) = if (pocetStartovnichNul m) == (length m) then [] else m:[]
odstranNuly (m:ms) = if (odstranNuly [m]) == [] then (odstranNuly ms) else (odstranNuly (m:ms))

-- Vrati hodnost matice - pocet nenulovych radku; todo
hodnost :: Matice -> Int
hodnost (x:[]) = if (jeNulovy x) then 0 else 1
hodnost (x:xs) = (hodnost [x]) + (hodnost xs)

-- Upravi matici na schodovy tvar; todo
schodovyTvar :: Matice -> Matice
schodovyTvar (m:ms) = if (jeSchodovyTvar (m:ms)) then (m:ms) else schodovyTvar (schod(m:ms))

-- Připraví daný řádek tak aby první nenulová hodnota byla 1
pripravMatici :: Matice -> Matice
pripravMatici m = map (pripravVektor) m

-- overi jestli je matice ve schodovem tvaru
jeSchodovyTvar :: Matice -> Bool
jeSchodovyTvar (m:[]) = True
jeSchodovyTvar (m:n:[]) = (pocetStartovnichNul m) < (pocetStartovnichNul n)
jeSchodovyTvar (m:n:ms) = (jeSchodovyTvar (m:n:[])) && (jeSchodovyTvar (n:ms))

-- pokus o vytvoření schodového tvaru
schod :: Matice -> Matice
schod (m:[]) = [(pripravVektor m)]
--schod (m:n:[]) = prenasobPrvnim (pripravMatici (m:n:[]))
schod (m:n:ms) = (head nova):(schod (zbytek nova)) where
          zbytek :: Matice -> Matice
          zbytek (m:ms) = ms
          
          nova :: Matice
          nova = prenasobPrvnim (pripravMatici (m:n:ms))

-- prvnim radkem prenasob ostatni
prenasobPrvnim :: Matice -> Matice
prenasobPrvnim (m:ms) = m:(prictiKMatici (nasobVektor m (-1)) ms)

-- pricte ke kazdemu radku matice vektor
prictiKMatici :: Vektor -> Matice -> Matice
prictiKMatici v m = map (sectiVektory v) m 

-- --------------- Matice ---------------

vektor :: Vektor
vektor = [3,2,1]

matice :: Matice
matice = [[3,2,1],
          [4,5,6],
          [7,5,3]]
      
matice2 :: Matice
matice2 = [[3,2,1,11],
           [4,5,6,12],
           [7,5,3,13],
           [9,5,1,14]]
          
-- neprehazuji se radky
matice3 :: Matice
matice3 = [[1,2,3],
           [2,3,1],
           [3,1,2]] 
           
-- hodnost=2
matice4 :: Matice
matice4 = [[1,2,3],
           [2,4,6],
           [3,1,2]] 
          
          
-- postup: 1) upravit 1. prvek 1. radku na 1 2) upravovat ostatni radky na 0
