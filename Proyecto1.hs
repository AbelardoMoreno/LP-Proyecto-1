import Data.List

data TipoBomba = Bomba Int Int
data TipoLanzamiento = Lanzamiento TipoBomba Int Int

bomba_kaboom::([TipoLanzamiento],Int,Int) -> [[Int]]
bomba_kaboom (xs, a , b) = llenarMatrix(crearMatrix a b) xs a b

colores_fav::[[Int]]->[Int]
colores_fav xs=reverse(repetidos(reverse(sort(agrupar xs))))

comprension_artistica::[[Int]]->Int
comprension_artistica xs= recorrido xs ((tamanoMatrixX xs)) ((tamanoMatrixY xs)) ((tamanoMatrixX xs-1)) ((tamanoMatrixY xs-1)) 

recorrido::[[Int]]->Int->Int->Int->Int->Int
recorrido xs n m 0 0|xs!!0!!0==0= 0
					|otherwise=1			
recorrido xs n m x 0|xs!!0!!x==0= recorrido xs n m (x-1) (m-1)
					|otherwise=1+recorrido (recorrido2 xs n m x 0 (xs!!0!!x))  n m (x-1) (m-1)
recorrido xs n m x y|xs!!y!!x==0= recorrido xs n m x (y-1)
					|otherwise = 1+ recorrido (recorrido2 xs n m x y (xs!!y!!x)) n m x (y-1)
					
recorrido2::[[Int]]->Int->Int->Int->Int->Int->[[Int]]
recorrido2 xs n m x y valor= recorridoAbajo (recorridoIzquierda (recorridoArriba xs n m x y valor) n m x y valor) n m x y valor
					
recorridoArriba::[[Int]]->Int->Int->Int->Int->Int->[[Int]]
recorridoArriba xs n m x 0 valor=xs
						
recorridoArriba xs n m x y valor|valor==xs!!(y-1)!!x&&(valor)/=0 = recorrido2(agregarPunto3 xs 0 0 x y n m) n m x (y-1) valor				
					            |valor==0= xs
						        |otherwise = agregarPunto3 xs 0 0 x y n m

recorridoIzquierda::[[Int]]->Int->Int->Int->Int->Int->[[Int]]
recorridoIzquierda xs n m 0 y valor= xs
							
recorridoIzquierda xs n m x y valor |valor==xs!!y!!(x-1)&&(valor)/=0= recorrido2(agregarPunto3 xs 0 0 x y n m) n m (x-1) y valor			 
									|(valor)==0= xs
									|otherwise=agregarPunto3 xs 0 0 x y n m
							  
recorridoAbajo::[[Int]]->Int->Int->Int->Int->Int->[[Int]]							
recorridoAbajo xs n m x y valor    |y==(m-1)=xs
								   |valor==xs!!(y+1)!!x&&(valor)/=0= recorrido2(agregarPunto3 xs 0 0 x y n m) n m x (y+1)valor			 
							       |(xs!!y!!x)==0= xs						  
							       |otherwise=agregarPunto3 xs 0 0 x y n m					  

tamanoMatrixX::[[Int]]->Int
tamanoMatrixX xs= tamanoMatrixX2(head(xs))

tamanoMatrixX2::[Int]->Int
tamanoMatrixX2 []=0
tamanoMatrixX2 xs= 1+tamanoMatrixX2(tail xs)

tamanoMatrixY::[[Int]]->Int
tamanoMatrixY []=0
tamanoMatrixY xs= 1+tamanoMatrixY(tail xs) 

crearMatrix:: Int->Int->[[Int]]
crearMatrix 0 0= error "N y M deben ser mayores que 0"
crearMatrix n 0= error "M debe ser mayor que 0"
crearMatrix 0 m= error "N debe ser mayor que 0"
crearMatrix n 1= [replicate n 0]
crearMatrix n m= [replicate n 0] ++ crearMatrix n (m-1)
crearMatrix n m= [replicate n 0] ++ crearMatrix n (m-1)

llenarMatrix:: [[Int]]->[TipoLanzamiento]->Int->Int->[[Int]]
llenarMatrix xs [] n m= xs 
llenarMatrix xs ys n m= llenarMatrix (modificarPos xs (obtenerColor(head ys)) (obtenerCantidadPintura (head ys)) (obtenerPosX (head ys)) (obtenerPosY (head ys)) n m ) (tail ys) n m

obtenerColor::TipoLanzamiento->Int
obtenerColor (Lanzamiento(Bomba n _) _ _)= n

obtenerCantidadPintura::TipoLanzamiento->Int
obtenerCantidadPintura (Lanzamiento(Bomba _ n) _ _)= n 

obtenerPosX::TipoLanzamiento->Int
obtenerPosX (Lanzamiento(Bomba _ _) n _)= n

obtenerPosY::TipoLanzamiento->Int
obtenerPosY (Lanzamiento(Bomba _ _) _ n)= n

modificarPos::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
modificarPos xs color cantidad x y n m=moverAbajo(moverArriba(moverIzquierda(moverDerecha(agregarPunto xs color cantidad x y n m) color cantidad (x+1) y n m) color cantidad (x-1) y n m) color cantidad x (y-1) n m) color cantidad x (y+1) n m

agregarPunto::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
agregarPunto xs color cantidad x 0 n m= [agregarPunto2 (head xs) x n color cantidad] ++ (tail xs) 
agregarPunto xs color cantidad x y n m|y<m= [(head xs)]++ agregarPunto (tail xs) color cantidad x (y-1) n m
									  |y>=m= error "Posicion de lanzamiento fuera de rango"
									  
agregarPunto2::[Int]->Int->Int->Int->Int->[Int]
agregarPunto2 xs 0 n color cantidad = [head xs+color] ++ tail xs
agregarPunto2 xs x n color cantidad |x<n= [head xs]++agregarPunto2 (tail xs) (x-1) n color cantidad
									|x>=n = error "Posicion de lanzamiento fuera de rango" 	
									
moverDerecha::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
moverDerecha xs color 0 x y n m= xs
moverDerecha xs color cantidad x y n m |x<n= moverDerecha (agregarPunto xs color cantidad x y n m) color (cantidad-1) (x+1) y n m
									   |x>=n= xs
		
moverIzquierda::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
moverIzquierda xs color 0 x y n m= xs
moverIzquierda xs color cantidad x y n m |x>=0= moverIzquierda (agregarPunto xs color cantidad x y n m) color (cantidad-1) (x-1) y n m
									     |x<0= xs				   

moverArriba::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
moverArriba xs color 0 x y n m= xs
moverArriba xs color cantidad x y n m |y>=0= moverArriba (agregarPunto xs color cantidad x y n m) color (cantidad-1) x (y-1) n m
									  |y<0= xs

moverAbajo::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
moverAbajo xs color 0 x y n m= xs
moverAbajo xs color cantidad x y n m |y<m= moverAbajo (agregarPunto xs color cantidad x y n m) color (cantidad-1) x (y+1) n m
									  |y>=m= xs											  

repetidos::[(Int,Int)]->[Int]
repetidos [] =[]
repetidos (x:y:xs)|fst(x)== fst(y)= [snd(x)]++repetidos(y:xs) 
			      |otherwise= [snd(x)]
repetidos(x:[])=[snd(x)]
			       
agrupar::[[Int]]->[(Int,Int)]
agrupar xs=map (\l@(x:xs) -> (length l,x)) . group . sort $ eliminarCeros(qsort(concat xs))
									  
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where
        lesser  = [ y | y <- xs, y < p ]
        greater = [ y | y <- xs, y >= p ]
		
eliminarCeros::[Int]->[Int]
eliminarCeros []=[]
eliminarCeros (x:xs) |x==0= eliminarCeros xs
				     |x>0= (x:xs)
					 
agregarPunto3::[[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]
agregarPunto3 xs color cantidad x 0 n m= [agregarPunto4 (head xs) x n color cantidad] ++ (tail xs) 
agregarPunto3 xs color cantidad x y n m|y<m= [(head xs)]++ agregarPunto3 (tail xs) color cantidad x (y-1) n m
									  |y>=m= error "Posicion de lanzamiento fuera de rango"
									  
agregarPunto4::[Int]->Int->Int->Int->Int->[Int]
agregarPunto4 xs 0 n color cantidad = [head xs*0] ++ tail xs
agregarPunto4 xs x n color cantidad |x<n= [head xs]++agregarPunto4 (tail xs) (x-1) n color cantidad
									|x>=n = error "Posicion de lanzamiento fuera de rango" 	
					 