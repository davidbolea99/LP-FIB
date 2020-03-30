

-- Per fer aquest exercici, oblideu que Functor (via Applicative) és un requeriment
-- de Monad (de fet, fa poc que ho és al std de Haskell).
-- L'objectiu és demostrar que tota Monad és un Functor.
-- Per a fer-ho:

-- 1. Implementeu les operacions de Functor (fmap) en termes de les operacions de
-- Monad (return i >>=). Per fer-ho, recordeu els tipus de les classes:

        class Functor f where
            fmap   :: (a -> b) -> (f a -> f b)

        class Monad m where
            return :: a -> m a
            >>=    :: m a -> (a -> m b) -> m b  -- (bind)

-- Segurament us anirà bé expressar la vostra implementació amb notació do, però
-- passeu-la a funcional per fer el següent apartat.




-- NOTACION DO

        class Functor f where
            fmap :: (a -> b) -> (f a -> f b)
            fmap funcion sujeto = do
                    x <- sujeto			-- desempaqueta el valor
					return (funcion x)  -- calcula y empaqueta

-- NOTACION FUNCIONAL

        class Functor f where
			fmap :: (a -> b) -> (f a -> f b) 		-- DATO: x es un valor encapsulado
			fmap f x = x >>= \x -> (return f x)		-- version con lambda
			fmap f x = x >>= \x -> (return . f) x 	-- version equivalente de composicion
			fmap f x = x >>= (return . f)			-- version sin lambda
			fmap f = >>= (return . f)				-- version sin parametro (implicito)





-- 2. Demostreu que la vostra implementació compleix les lleis de Functor, suposant
-- que es donen les de Monad.

        -- M1:     ret x >>= f = f x
        -- M2:    v >>= return = v
        -- M3: (v >>= f) >>= g = v >>= (\x -> f x >> g)

-- Per fer-ho, recordeu les lleis de Functor:

--         F1:         fmap id = id
--         F2:  fmap (g1 . g2) = fmap g1 . fmap g2

-- F1

fmap id c 	= c >>= (return . id)	-- sustitucion
			= c >>= return			-- por definicion de identidad
			= c						-- M2:    v >>= return = v

=> fmap id = id -- Donete


-- F2

fmap (g1 . g2)	= 




-- F1
fmap id = >>= (return id)
fmap id (Functor hola) = (Functor hola) >>= 	-- me da un hola
fmap id (Functor hola) = return (id hola) 		-- id hola = hola
fmap id (Functor hola) = return hola			-- return hola = Functor hola
fmap id (Functor hola) = (Functor hola)			-- correcto

--F2
fmap (f . g) = >>= (return (f . g))

fmap (f . g) (Functor hola) = (Functor hola) >>= 	-- me da un hola
fmap (f . g) (Functor hola) = return (f (g hola)) 	-- (f . g) hola = f(g(hola))
fmap (f . g) (Functor hola) = return f(g(hola))			-- return f(g(hola)) = Functor f(g(hola))
fmap (f . g) (Functor hola) = Functor f(g(hola))	-- correcto

fmap f . fmap g
fmap f (Functor hola) = (Functor hola) >>=
fmap f (Functor hola) = return (f hola)
fmap f (Functor hola) = Functor f(hola)	   

