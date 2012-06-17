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

-- ESTO ES UN EJEMPLO DE DOCUMENTACIÓN EN HADDOC
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
import Control.Monad.Error

data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) } deriving (Eq, Show)

type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

data RTree = RTree Int Int HRTree deriving (Show)

data HRTree = Node {tree::(Seq HRTree), mbr :: MBR, lhv :: LHV}
            | Leaf {recs::(Seq Rectangle), lmbr :: MBR, llhv :: LHV}
            deriving(Show)

data Crumb = Crumb MBR LHV (Seq HRTree) (Seq HRTree)

type Zipper = ( HRTree , (Seq Crumb) )

instance Ord Rectangle where
  r1 < r2 = hilbert r1 < hilbert r2

emptyRectangle :: Rectangle
emptyRectangle = R (0,0) (0,0) (0,0) (0,0)

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

{-Api del RTree-}

data RTreeError = RepeatedRectangleInsertion Rectangle
                | NonExistentRectangleDeletion Rectangle
                | UnknownError String

instance Error RTreeError where
  noMsg = UnknownError "unknown error"
  strMsg = UnknownError

instance Show RTreeError where
  show (RepeatedRectangleInsertion r) =
    "Repeated rectangle insertion\n\n" ++ (show r) ++ "\n\n"
  show (NonExistentRectangleDeletion r) =
    "Non-Existent rectangle deletion\n\n" ++ (show r) ++ "\n\n"
  show (UnknownError msg) = msg

newRTree :: Int -> Int -> RTree
newRTree cl cn = RTree cl cn newHRTree

newHRTree :: HRTree
newHRTree = Leaf empty emptyRectangle 0

getL :: ViewL a -> a
getL (x :< xs) = x

head' :: Seq a -> a
head' sec = getL $ viewl sec

tail' :: Seq a -> Seq a
tail' sec = case (viewl sec) of
  (x :< xs) -> xs

{- Some other useful functions on sequences -}
-- init' :: Seq a -> Seq a
-- init' sec = case (viewr sec) of
--   (xs :> x) -> xs

-- last' :: Seq a -> a
-- last' sec = getR $ viewr sec

-- getR :: ViewR a -> a
-- getR (xs :> x) = x

deleteFromSeq :: (Eq a) => Seq a -> a -> Seq a
deleteFromSeq sec elem = foldl' decide empty sec
  where
    decide acc e
      | elem /= e = acc |> e
      | otherwise = acc

reconstruct :: Zipper -> HRTree
reconstruct z@( focus , crumbs )
  | S.null crumbs = focus
  | otherwise   = case (head' crumbs) of
    Crumb mbr lhv before after ->
      reconstruct (Node (before >< (focus <| after)) mbr lhv , tail' crumbs)

remove:: Zipper -> Rectangle -> Zipper
remove z@(Leaf recs mbr lhv, crumbs) r = (newHRTree, empty)
--remove z@(Leaf recs mbr lhv, crumbs) r =

delete :: RTree -> Rectangle -> Either e RTree
delete (RTree cl cn t) r = case (delete' (t,empty) r) of
  Left e -> Left $ error (show e)
  Right t -> return $ RTree cl cn t
  where
    delete' :: Zipper -> Rectangle -> Either RTreeError HRTree
    delete' (Node trees mbr lhv, crumbs) r = delete' ( focus , trail ) r
      where
        focus = head' afterChosen
        trail = (Crumb mbr lhv beforeChosen $ tail' afterChosen) <| crumbs
        beforeChosen = fst $ chooseChild trees
        afterChosen = snd $ chooseChild trees
        chooseChild = spanl (\ (Node _ _ lhv) -> lhv < hilbert r)
    delete' z@(Leaf recs mbr lhv, crumbs) r = case (elemIndexL r recs) of
      Nothing -> throwError $ NonExistentRectangleDeletion r
      Just i  -> return $ reconstruct $ remove z r