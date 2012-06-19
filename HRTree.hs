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

import Control.Monad.Error
import Data.Sequence as S
import Data.Foldable as F
import Data.Bits
import Graphics.HGL as G

data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) } deriving (Show)

type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

data RTree = RTree Int Int HRTree deriving (Show)

data HRTree = Node {tree::(Seq HRTree), mbr :: MBR, lhv :: LHV} 
            | Leaf {recs::(Seq Rectangle), lmbr :: MBR, llhv :: LHV} 
            deriving (Show)

--overflow information
type OvInfo = Either Rectangle HRTree
              
newtype CoopS = CoopS (Int,HRTree)

winSize :: Int
winSize = 700

showTree :: RTree -> IO ()
showTree (RTree cl cn hrtree) = do
  putStr("Max recs in leafs: ")
  putStrLn(show(cl))
  putStr("Max sons in nodes: ")
  putStrLn(show(cn))
  G.runGraphics (
    G.withWindow_ "Ugly tree" (winSize, winSize) $
    (\w -> do
        showTree' w hrtree
        G.getKey w)
    )

drawRectangle :: G.Window -> Rectangle -> IO ()
drawRectangle w R{ul=(ulxB,ulyB) , lr=(lrxB,lryB)} = F.mapM_ (G.drawInWindow w) r
  where  ulx = (ulxB * winSize) `div` 65335
         uly = (ulyB * winSize) `div` 65335
         lrx = (lrxB * winSize) `div` 65335
         lry = (lryB * winSize) `div` 65335
         r = (G.line (ulx,uly) (lrx,uly)): --top
             (G.line (lrx,uly) (lrx,lry)): --right
             (G.line (lrx,lry) (ulx,lry)): --bottom
             [(G.line (ulx,lry) (ulx,uly))]  --left
  
showTree' :: G.Window -> HRTree -> IO ()
showTree' w (Node sons mbr _) = do
  drawRectangle w mbr
  F.mapM_ (showTree' w) sons
showTree' w (Leaf rs mbr _) = do
  drawRectangle w mbr
  F.mapM_ (drawRectangle w) rs

getLHV :: HRTree -> LHV
getLHV (Node _ _ l) = l
getLHV (Leaf _ _ l) = l

getMBR :: HRTree -> MBR
getMBR (Node _ m _) = m
getMBR (Leaf _ m _) = m

getOvLHV :: OvInfo -> LHV
getOvLHV (Right t) = getLHV t
getOvLHV (Left  r) = hilbert r

createSon :: HRTree -> HRTree
createSon (Node _ _ _) = emptyNode
createSon (Leaf _ _ _) = emptyLeaf

updateSon :: Int -> OvInfo -> HRTree -> HRTree

updateSon i (Right n) parent@(Node t m l) =
  parent{tree = S.update i newSon (tree parent) }
  where son@(Node _ _ _) = index t i
        newGrandSons = S.unstableSort (n <| (tree son))
        newSon = son{tree=newGrandSons}

updateSon i (Left r) parent@(Node t m l) =
  parent{tree = S.update i newSon (tree parent) }
  where son@(Leaf _ _ _) = index t i
        newGrandSons = S.unstableSort (r <| (recs son))
        newSon = son{recs=newGrandSons}

updateMBRLHV :: HRTree -> HRTree

updateMBRLHV (Node t m l) = (Node t newm newl)
  where newm = F.foldl' sumRects emptyMBR (fmap getMBR t)
        newl = F.foldl' max 0 (fmap getLHV t)
                            
updateMBRLHV (Leaf t m l) = (Leaf t newm newl)
  where newm = F.foldl' sumRects emptyMBR t
        newl = F.foldl' max 0 (fmap hilbert t)

sumRects :: Rectangle -> Rectangle -> Rectangle
sumRects
  R{ ul=(ulx1,uly1) , lr=(lrx1,lry1)}
  R{ ul=(ulx2,uly2) , lr=(lrx2,lry2)} = 
    R{ul=(ulx,uly) , ur=(lrx,uly),
      ll=(ulx,lry) , lr=(lrx,lry)}
      where ulx = min ulx1 ulx2
            uly = min uly1 uly2
            lrx = max lrx1 lrx2
            lry = max lry1 lry2

getGrandSons :: HRTree -> Seq (Seq (Either Rectangle HRTree))
getGrandSons (Node t _ _ ) = fmap getSons t

getSons :: HRTree -> Seq (Either Rectangle HRTree)
getSons (Node s _ _) = fmap Right s
getSons (Leaf s _ _) = fmap Left s
  
putIntoSeq :: OvInfo -> Seq (Either Rectangle HRTree)
putIntoSeq (Left  r) = S.empty |> (Left  r)
putIntoSeq (Right t) = S.empty |> (Right t)

--This is a "negative rectangle"
emptyMBR :: MBR
emptyMBR = R{ul=(65535,65535) , ur=(0,65535),
             ll=(65535,0)     , lr=(0,0)}

just :: Maybe a -> a
just (Just bla) = bla

right :: Either a b -> b
right (Right bla) = bla

left :: Either a b -> a
left (Left bla) = bla

emptyNode :: HRTree
emptyNode = Node S.empty emptyMBR 65336

emptyLeaf :: HRTree
emptyLeaf = Leaf S.empty emptyMBR 65336

isRectangle :: Rectangle -> Bool
isRectangle r =    snd (ul r) >  snd (ll r)
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

centroid :: Rectangle -> Main.Point
centroid r = (((fst (ul r)) + (fst (lr r))) `div` 2,
             ((snd (ul r)) + (snd (lr r))) `div` 2)

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
hilbert r = hilbertDistance 16 $ centroid r

instance Eq Rectangle where
  r1 == r2 = (hilbert r1) == (hilbert r2)

instance Ord Rectangle where
  r1 < r2 = (hilbert r1) < (hilbert r2)
  
instance Eq HRTree where
  (Node _ _ l1) == (Node _ _ l2) = l1 == l2
  (Node _ _ l1) == (Leaf _ _ l2) = l1 == l2
  (Leaf _ _ l1) == (Node _ _ l2) = l1 == l2
  (Leaf _ _ l1) == (Leaf _ _ l2) = l1 == l2  

instance Ord HRTree where
  (Node _ _ l1) < (Node _ _ l2) = l1 < l2
  (Node _ _ l1) < (Leaf _ _ l2) = l1 < l2
  (Leaf _ _ l1) < (Node _ _ l2) = l1 < l2
  (Leaf _ _ l1) < (Leaf _ _ l2) = l1 < l2

{-Api del RTree-}

newRTree :: Int -> Int -> RTree
newRTree cl cn = RTree cl cn $ Node (S.empty |> (Leaf S.empty emptyMBR 65336)) emptyMBR 65336

search :: RTree -> Rectangle -> Maybe [ Rectangle ]
search (RTree cl cn t) r = case (toList $ auxSearch r t) of
  ls@(x:_) -> Just ls
  []       -> Nothing
  where
    auxSearch :: Rectangle -> HRTree -> Seq Rectangle
    auxSearch r n@(Node _ _ _) =
      fromList $ F.concatMap
      (\ni -> case ni of
          (Leaf sons m h) -> toList $ S.filter (overlapped r) sons
          (Node sons m h) -> Prelude.concat $
                             map toList $ toList $ fmap (auxSearch r) sons
      )
      (S.filter verifyOverlap (tree n))
    auxSearch r l@(Leaf _ _ _) = S.filter (overlapped r) (recs l)
    verifyOverlap ni = overlapped r (mbr ni)


insert :: RTree -> Rectangle -> Either String RTree
insert (RTree cl cn t) r = do
   (maybeov,newt) <- insert' t r
   newSons <- Right $
              maybe newt (\ov->(Node (S.empty|>newt|>(right ov)) emptyMBR 0))
              maybeov
   Right $ RTree cl cn newSons
   
   
insert' :: HRTree -> Rectangle ->
           Either String (Maybe OvInfo,HRTree)
insert' (Leaf rs m h) r =
  let (xs :> bla) = viewr rs
      newRecs1 = updateMBRLHV (Leaf (S.unstableSort (r <| xs)) m h)
      newRecs2 = updateMBRLHV (Leaf (S.unstableSort (r <| rs)) m h) in
  if ((S.length rs) >= 3) then
    Right $ ((Just (Left bla)) , newRecs1)
  else
    Right $ (Nothing , newRecs2)
           
insert' (Node sons m h) rect = do
  (i,node) <- Right $ pickNode sons rect
  --this can return a NEW son, we have to consider that case in the update that follows
  (ovinfo,newnode) <- insert' node rect
  newsons <- Right $ S.update i newnode sons
  Right $ handleOverFlow (Node newsons m h) ovinfo rect 
  
  
handleOverFlow :: HRTree -> Maybe OvInfo -> Rectangle ->
                  (Maybe OvInfo, HRTree)
handleOverFlow parent Nothing rect = (Nothing,updateMBRLHV parent)
handleOverFlow parent@(Node sons m h) (Just ovinfo) rect =
  either
  (split parent rect)
  ((,) Nothing)
  (insertNodeIfNotFull parent cs ovinfo rect)
    where cs = getCooperatingSibling (updateMBRLHV parent) ovinfo
          
          
insertNodeIfNotFull :: HRTree -> Maybe CoopS ->
                       OvInfo -> Rectangle ->
                       --this two recs are for mbr and lhv updating
                       Either OvInfo HRTree
insertNodeIfNotFull _ Nothing ovinfo _ = Left ovinfo
insertNodeIfNotFull parent (Just (CoopS (i,cs))) ovinfo rect =
  if (S.length (getSons cs)) < 2 then
    Right $ updateSon i ovinfo (updateMBRLHV parent)
  else
    Left ovinfo
 
    
insertRectIfNotFull :: HRTree -> Rectangle -> Either HRTree HRTree
insertRectIfNotFull l@(Leaf sons m h) r =
  if (S.length sons) < 2 then --WIRED
    Right $ 
    updateMBRLHV l{recs = S.unstableSort (r <| sons)}
  else
    Left l
    
    
getCooperatingSibling :: HRTree -> OvInfo -> Maybe CoopS
getCooperatingSibling parent@(Node t _ _) ovinfo = 
  let  biggerThanOvLHV = ((<) (getOvLHV ovinfo)).getLHV in
  do
    ind <- S.findIndexL biggerThanOvLHV (tree parent)
    return $ CoopS (ind,S.index t ind)


pickNode:: Seq HRTree -> Rectangle -> (Int,HRTree)
pickNode sons r = maybe
                  (S.length sons , createSon (index sons 0))
                  (\i -> (i,index sons i))
                  maybeind
  where biggerThanRectLHV = ((<) (hilbert r)).getLHV
        maybeind = S.findIndexL biggerThanRectLHV sons


split:: HRTree -> Rectangle -> OvInfo -> (Maybe OvInfo, HRTree)
split parent rect ovInfo = if (S.length newSons <= 2) then --WIRED
                             (Nothing,updateMBRLHV parent{tree=newSons})
                           else
                             let xs :> a = viewr newSons in
                             (Just (Right a),updateMBRLHV parent{tree=xs})
  where allGrandSons = (putIntoSeq ovInfo) <| (getGrandSons parent)
        newGrandsons = redistribute (getGrandSons parent) 3 --WIRED
        newSons = S.zipWith replaceSons (getSons parent) newGrandsons


replaceSons :: Either Rectangle HRTree -> Seq (Either Rectangle HRTree) -> HRTree
replaceSons (Right son@(Node _ _ _)) t  = son{tree = fmap right t}
replaceSons (Right son@(Leaf _ _ _)) rs = son{recs = fmap left rs}
    
                                          
redistribute :: Seq (Seq a) -> Int -> Seq (Seq a)
redistribute stuffList parts = evenly stuff S.empty
  where stuff = F.foldl1 (><) stuffList
        sz = (S.length stuff) `div` parts
        evenly sequ acc =
          if (S.null sequ) then
            acc
          else
            evenly (S.drop sz sequ) (acc >< (S.empty |> (S.take sz sequ)))
  
        
runTest :: IO ()
runTest = do
  ini <- return (newRTree 2 3)
  test <- return (do
                     a1 <- insert ini R{ul=(40000,45000), ur=(50000,45000),
                                        ll=(40000,60000), lr=(50000,60000)}
                     a2 <- insert a1  R{ul=(20000,20000), ur=(25000,20000),
                                        ll=(20000,25000), lr=(25000,25000)}
                     a3 <- insert a2  R{ul=(50000,50000), ur=(65000,50000),
                                        ll=(50000,60000), lr=(65000,60000)}
                     a4 <- insert a3  R{ul=(1000 ,1000) , ur=(10000,1000),
                                        ll=(1000 ,10000), lr=(10000,10000)}
                     return a2
                 )
  putStrLn(show test)
  either putStrLn showTree test
  
--delete :: tree -> Rectangle -> Either e tree
