{-|
Universidad Sim&#243;n Bol&#237;var

Departamento de Computaci&#243;n y Tecnolog&#237;a de la Informaci&#243;n

Programaci&#243;n Funcional Avanzada

CI-4251

Autores:

  Victor De Ponte, 05-38087

  Germán Jaber,    06-39749


M&#243;dulo:
  'RTree'

    Define la estructura de datos /RTree/. Esta estructura es utilizada para el
almacenamiento de data geoespacial, especialmente en las bases de datos. Es una
estructura híbrida entre el R-Tree y el B+-Tree. Utiliza la distancia de Hilbert
del punto medio de los rectángulos almacenados para establecer una relación de
orden entre los mismos.
-}
-- module RTree
--        (

--        ) where

-- -- | Función 'dequeue'. Desencola el siguiente elemento en la cola de
-- --   prioridades, y devuelve una tupla con dicho elemento como primera
-- --   componente, y la /PQueue/ sin el elemento como segunda componente.
-- dequeue :: (Ord a, Eq a) =>
--            PQueue a           -- ^ /PQueue/ a desencolar
--            -> ( a , PQueue a) -- ^ tupla con el elemento desencolado, y la
--                               --   /PQueue/ sin el elemento.
-- dequeue q = deleteMinimum q

import Data.Sequence as S
import Data.Foldable as F
import Data.Bits

data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) } deriving (Eq, Show)

type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

data RTree = RTree Int Int HRTree

data NodeInfo = NI {mbr :: MBR, tree :: HRTree, lhv :: LHV}
data LeafInfo = LI {rec :: Rectangle, hv :: LHV}
data HRTree = Node (Seq NodeInfo)
            | Leaf (Seq LeafInfo)

isRectangle :: Rectangle -> Bool
isRectangle r = snd (ul r) > snd (ll r)
                && snd (ur r) >  snd (lr r)
                && fst (ll r) <  fst (lr r)
                && fst (ul r) <  fst (ur r)
                && snd (ll r) == snd (lr r)
                && snd (ul r) == snd (ur r)
                && fst (ll r) == fst (ul r)
                && fst (lr r) == fst (ur r)

overlapped :: Rectangle -> Rectangle -> Bool
overlapped r1 r2 = not $
                   fst (ul r1) > fst (lr r2)
                   || fst (lr r1) < fst (ul r2)
                   || snd (ul r1) < snd (lr r2)
                   || snd (lr r1) > snd (ul r2)

centroid :: Rectangle -> Point
centroid r = (((fst (ul r)) + (fst (lr r))) `div` 2,
              ((snd (ul r)) + (snd (lr r))) `div` 2
             )

hilbertDistance :: (Bits a, Ord a) => Int -> (a,a) -> a
hilbertDistance d (x,y)
    | x < 0 || x >= 1 `shiftL` d = error "x bounds"
    | y < 0 || y >= 1 `shiftL` d = error "y bounds"
    | otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
    where dist 0 _ result _ _ = result
          dist side area result x y =
              case (compare x side, compare y side) of
              (LT, LT) -> step result y x
              (LT, _)  -> step (result + area) x (y - side)
              (_, LT)  -> step (result + area * 3) (side - y - 1)
                          (side * 2 - x - 1)
              (_, _)   -> step (result + area * 2) (x - side) (y - side)
              where step = dist (side `shiftR` 1) (area `shiftR` 2)

hilbert :: Rectangle -> Int
hilbert r = hilbertDistance 65536 $ centroid r

instance Ord Rectangle where
  r1 < r2 = hilbert r1 < hilbert r2

{-Api del RTree-}

newRTree :: Int -> Int -> RTree
newRTree cl cn = RTree cl cn $ Leaf empty

search :: RTree -> Rectangle -> Maybe [ Rectangle ]
search (RTree cl cn t) r = case (toList $ auxSearch r t) of
  ls@(x:_) -> Just ls
  []       -> Nothing
  where
    auxSearch :: Rectangle -> HRTree -> Seq Rectangle
    auxSearch r (Node n) = fromList $ F.concatMap
                           (\ ni -> toList $ auxSearch r (tree ni)) $
                           S.filter verifyOverlap n
    auxSearch r (Leaf l) = S.filter (overlapped r) $ fmap rec l
    verifyOverlap ni = overlapped r (mbr ni)


-- insert :: RTree -> Rectangle -> Either e RTree
-- insert (RTree cl cn t) r = case (isRectangle)
  
--   case (insert' t r) of
--   Left e     -> throwError e
--   Right tree -> Right (RTree cl cn tree)
--   where
--     insert' :: HRTree -> Rectangle -> Either e HRTree
--     insert' (Leaf sli) r = a
--     insert' (Node sni) r = a


--delete :: tree -> Rectangle -> Either e tree
