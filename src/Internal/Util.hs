module Internal.Util where

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

triple :: (t -> a) -> (t -> b) -> (t -> c) -> t -> (a, b, c)
triple f g h x = (f x, g x, h x)

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x
