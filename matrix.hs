-- ------------ Režie ---------------

type Vektor=[Float] -- lepší by byl double
type Matice=[Vektor] 

-- Převede Int na String
intNaString :: Int -> String
intNaString a = show a

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

-- Zaokrouhlení 
--zaokrouhli :: Float -> Int -> Float 
--zaokrouhli f n = if n==0 then (floor f) else (zaokrouhli (f*10) (n-1))/10
zaokrouhli s n = fromIntegral (round (n * faktor)) / faktor 
   where posun = s - (floor (logBase 10 n)  + 1) 
         faktor = 10 ** fromIntegral posun 

-- --------------- operace s řádky -----------------

-- ověří jestli je řádek nulový
jeNulovy :: Vektor -> Bool
jeNulovy (x:[]) = if (x==0) then True else False
jeNulovy (x:xs) = if (jeNulovy [x]) then (jeNulovy xs) else False

-- násobení vektoru konstatntou
nasobVektor :: Vektor -> Float -> Vektor
nasobVektor v k = map (*k) v

-- zaokrouhlí každý prvek vektoru
zaokrouhliVektor :: Int -> Vektor -> Vektor
zaokrouhliVektor s v = map (zaokrouhli s) v

-- násobení 2 vektoru
nasobVektory :: Vektor -> Vektor -> Vektor
nasobVektory [] [] = []
nasobVektory [] [_] = []
nasobVektory [_] [] = []
nasobVektory (u:us) (v:vs) = (u*v):(nasobVektory us vs)

-- sčítání 2 vektorů
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

-- vypustí nulové řádky
odstranNuly :: Matice -> Matice
odstranNuly (m:[]) = if (pocetStartovnichNul m) == (length m) then [] else m:[]
odstranNuly (m:ms) = if (odstranNuly [m]) == [] then (odstranNuly ms) else (odstranNuly (m:ms))

-- Vrátí hodnost matice - počet nenulových řádku
hodnost :: Matice -> Int
hodnost (x:[]) = if (jeNulovy x) then 0 else 1
hodnost (x:xs) = (hodnost [x]) + (hodnost xs)

-- Upraví matici na schodový tvar
schodovyTvar :: Matice -> Matice
schodovyTvar (m:ms) = if (jeSchodovyTvar (m:ms)) then (m:ms) else schodovyTvar (schod(m:ms))

-- Připraví daný řádek tak aby první nenulová hodnota byla 1
pripravMatici :: Matice -> Matice
pripravMatici m = map (pripravVektor) m

-- Zaokrouhlí každý prvek matice
zaokrouhliMatici :: Int -> Matice -> Matice
zaokrouhliMatici s m = map (zaokrouhliVektor s) m 

-- ověří jestli je matice ve schodovém tvaru
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

-- prvním řádkem přenásob ostatní
prenasobPrvnim :: Matice -> Matice
prenasobPrvnim (m:ms) = m:(prictiKMatici (nasobVektor m (-1)) ms)

-- přičte ke každému řádku matice vektor
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
          
-- nepřehazují se řádky
matice3 :: Matice
matice3 = [[1,2,3],
           [2,3,1],
           [3,1,2]] 
           
-- hodnost=2 (chyba v přesnosti)
matice4 :: Matice
matice4 = [[1,2,3],
           [2,4,6],
           [3,1,2]] 
    
-- Funkce main    
            
main = do (tiskMatice (schodovyTvar matice3))
          tisk (intNaString (hodnost (schodovyTvar matice3)))
