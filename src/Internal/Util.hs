module Internal.Util where

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x
