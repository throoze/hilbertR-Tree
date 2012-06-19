{-|
Module      :  RTree
Copyright   :  (c) Victor De Ponte - 05-38087 - <rdbvictor19@gmail.com> 2012,
               (c) Germán Jaber    - 06-39749 - <plaga701@gmail.com> 2012,
               (c) Universidad Sim&#243;n Bol&#237;var 2012
License     :  GNU GPL v3

Maintainer  :  rdbvictor19@gmail.com,
               plaga701@gmail.com
Stability   :  experimental
Portability :  portable

[Computation type:] Storage and querying of geospatial data, specified as
'Rectangle's.

[Useful for:] Storage and querying of geospatial data, specified as
'Rectangle's.

Defines the /Hilbert's RTree/ data structure. Used for storing geospatial data,
especially in data bases. It's a hybrid structure between the R-Tree and the
B+-tree. Uses the Hilbert's distance of the middle point of the stored
rectangles to establish an order relation over them. The implementation of this
module is based on the paper \'Hilbert R-tree: An Improved R-tree Using
Fractals\' (1994), by Ibrahim Kamel and Christos Faloutsos. It was developed as
an assignment for the course of \'Programaci&#243;n Funcional Avanzada -
CI-4251\' (Advanced Functional Programming - CI-4251) in the Universidad
Sim&#243;n Bol&#237;var, on the period April-July 2012, due to 2012-06-17 23:59
VET.

Reference to the paper:

<http://www.cis.temple.edu/~vasilis/Courses/CIS750/Papers/HilbertRtree-Kamel.pdf>
-}
module RTree
       (
         Rectangle(..), RTree(..),
         -- * Rectangle Manipulation
         emptyRectangle,  -- :: Rectangle
         isRectangle,     -- :: Rectangle -> Bool
         overlapped,      -- :: Rectangle -> Rectangle -> Bool
         centroid,        -- :: Rectangle -> Point
         hilbert,         -- :: Rectangle -> Int
         -- * Construction
         newRTree,        -- :: Int -> Int -> RTree
         -- * Insertion
         --insert,          -- :: RTree -> Rectangle -> Either e RTree
         -- * Deletion
         delete,          -- :: RTree -> Rectangle -> Either e RTree
         -- * Queries
         --search,          -- :: RTree -> Rectangle -> Maybe [ Rectangle ]
       ) where

import Data.Sequence as S
import Data.Foldable as F
import Data.Bits
import Control.Monad.Error
import Graphics.HGL as G
import System.IO

-- | General representation of a rectangle.
data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) } --deriving (Show)

-- Establish the Rectanles order relation by comparing them on their centroid
-- hilbert's value.
instance Eq Rectangle where
  r1 == r2 = (hilbert r1) == (hilbert r2)

instance Ord Rectangle where
  r1 <= r2 = (hilbert r1) <= (hilbert r2)

instance Eq HRTree where
  (Node _ _ l1) == (Node _ _ l2) = l1 == l2
  (Node _ _ l1) == (Leaf _ _ l2) = l1 == l2
  (Leaf _ _ l1) == (Node _ _ l2) = l1 == l2
  (Leaf _ _ l1) == (Leaf _ _ l2) = l1 == l2

instance Ord HRTree where
  (Node _ _ l1) <= (Node _ _ l2) = l1 <= l2
  (Node _ _ l1) <= (Leaf _ _ l2) = l1 <= l2
  (Leaf _ _ l1) <= (Node _ _ l2) = l1 <= l2
  (Leaf _ _ l1) <= (Leaf _ _ l2) = l1 <= l2

-- Some convenience aliases and constructors
type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

--overflow information
type OvInfo = Either Rectangle HRTree
newtype CoopS = CoopS (Int,HRTree)

-- | Representation of the data type 'RTree'
data RTree = RTree Int Int HRTree deriving (Show)

{- Internal representation of the tree. The wrapper 'RTree' is used and exported
instead, so the user can define a maximum leaf capacity and a maximum node
capacity, on creating a new 'RTree'.
-}
data HRTree = Node {tree::(Seq HRTree), mbr :: MBR, lhv :: LHV}
            | Leaf {recs::(Seq Rectangle), lmbr :: MBR, llhv :: LHV}
            --deriving(Show)

instance Show HRTree where
  show (Node t _ _) = "N ["++show (F.concat (fmap show t))++"]"
  show (Leaf t _ _) = "L ["++show (F.concat (fmap show t))++"]"

instance Show Rectangle where
  show (R _ _ _ _) = "R "

-- Internal model for applying the Zipper technique.
data Crumb = Crumb MBR LHV (Seq HRTree) (Seq HRTree)
type Zipper = ( HRTree , (Seq Crumb) )


-- | Creates a new empty meaningless Rectangle.
emptyRectangle :: Rectangle -- ^ An empty degenerate rectangle.
emptyRectangle = R (0,0) (0,0) (0,0) (0,0)


-- | Checks if a 'Rectangle' structure represents indeed a rectangle.
isRectangle :: Rectangle -- ^ The 'Rectangle' to check
               -> Bool   -- ^ /True/ if the structure represents a real
                         --   rectangle. /False/ otherwise.
isRectangle r = snd (ul r) > snd (ll r)
                && snd (ur r) >  snd (lr r)
                && fst (ll r) <  fst (lr r)
                && fst (ul r) <  fst (ur r)
                && snd (ll r) == snd (lr r)
                && snd (ul r) == snd (ur r)
                && fst (ll r) == fst (ul r)
                && fst (lr r) == fst (ur r)

-- | Checks if two rectangles are overlapped.
overlapped :: Rectangle    -- ^ First Rectagle to check
              -> Rectangle -- ^ Second Rectangle to check
              -> Bool      -- ^ /True/ if the two rectangles overlap, /False/
                           --   otherwise.
overlapped r1 r2 = not $
                   fst (ul r1) > fst (lr r2)
                   || fst (lr r1) < fst (ul r2)
                   || snd (ul r1) < snd (lr r2)
                   || snd (lr r1) > snd (ul r2)

-- | Calculates the centroid of a 'Rectangle'.
centroid :: Rectangle -- ^ A Rectangle
            -> RTree.Point  -- ^ Centroid of the 'Rectangle' passed.
centroid r = (((fst (ul r)) + (fst (lr r))) `div` 2,
              ((snd (ul r)) + (snd (lr r))) `div` 2
             )

{-
Calculates the Hilbert's distance of a point on a @dxd@ grid.
-}
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

-- | Calculates the hilbert's value of the centroid of a 'Rectangle'
hilbert :: Rectangle -- ^ A rectangle
           -> Int    -- ^ The hilbert's value of the centroid of the Rectangle
                     --   passed.
hilbert r = hilbertDistance 16 $ centroid r

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

{-
Data representation of possible errors when manipulating 'RTree's.
-}
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

{-
Convinience function that creates a new empty 'RTree'
-}
newHRTree :: HRTree
newHRTree = Leaf empty emptyMBR 0

{- Convinience functions on 'Seq's -}

{-
Gets the element contained in a 'ViewL' structure.
-}
getL :: ViewL a -> a
getL (x :< xs) = x

{-
Gets the leftmost element of a 'Seq'
-}
head' :: Seq a -> a
head' sec = getL $ viewl sec

{-
Gets the result of removing the leftmost element of a 'Seq'
-}
tail' :: Seq a -> Seq a
tail' sec = case (viewl sec) of
  (x :< xs) -> xs

{-
Gets the element contained in a 'ViewR' structure.
-}
getR :: ViewR a -> a
getR (xs :> x) = x

{-
Gets the rightmost element of a 'Seq'
-}
last' :: Seq a -> a
last' sec = getR $ viewr sec

{-
Gets the result of removing the rightmost element of a 'Seq'
-}
init' :: Seq a -> Seq a
init' sec = case (viewr sec) of
  (xs :> x) -> xs


{-
Deletes a given element of a Sequence in case of the element belongs to it. If
not, returns the same Sequence.
-}
deleteFromSeq :: (Eq a) => Seq a -> a -> Seq a
deleteFromSeq sec elem = foldl' decide empty sec
  where
    decide acc e
      | elem /= e = acc |> e
      | otherwise = acc

{- Convinience functions on Zippers -}

{-
Reconstruct an HRTree from a Zipper, updating mbr's and lhv's.
-}
reconstruct :: Zipper -> HRTree
reconstruct ( focus , crumbs )
  | S.null crumbs = focus
  | otherwise   = case (focus) of
    Leaf rects _ _ -> case (head' crumbs) of
      Crumb cmbr clhv before after ->
        reconstruct (Node
                     (before >< (updatedFocus <| after))
                     (maximumBR (lmbr updatedFocus) cmbr)
                     (max (llhv updatedFocus) clhv) ,
                      tail' crumbs)
      where
        updatedFocus = Leaf rects (maxBoundingRectangle focus) (largestHV rects)
    Node tree mbr lhv -> case (head' crumbs) of
      Crumb cmbr clhv before after ->
        reconstruct (Node
                     (before >< (focus <| after))
                     (maximumBR mbr cmbr)
                     (max lhv clhv) ,
                     tail' crumbs)

{-
Calculates the maximum bounding rectangle between two rectangles.
-}
maximumBR :: MBR -> MBR -> MBR
maximumBR (R (ul1x,ul1y) (ll1x,ll1y) (lr1x,lr1y) (ur1x,ur1y))
  (R (ul2x,ul2y) (ll2x,ll2y) (lr2x,lr2y) (ur2x,ur2y)) =
  R ((min ul1x ul2x),(max ul1y ul2y)) ((min ll1x ll2x),(min ll1y ll2y))
  ((max lr1x lr2x),(min lr1y lr2y)) ((max ur1x ur2x),(max ur1y ur2y))

{-
Returns the maximum bounding rectangles for a given 'HRTree'
-}
maxBoundingRectangle :: HRTree -> Rectangle
maxBoundingRectangle (Leaf rects _ _) =
  buildRectangle $ foldl' buildBiggest ((0,0),(0,0)) rects
maxBoundingRectangle (Node trees _ _) =
  buildRectangle $ foldl' buildBiggest ((0,0),(0,0)) rects
  where
    rects = fmap maxBoundingRectangle trees

{-
Returns a pair of 'Point's  representing the highest leftmost corner and the
lowest rightmost corner of the minimum rectangle which contains both rectangles:
the one represented by the pair of points received, and the one received as a
rectangle. Useful within a foldl.
-}
buildBiggest :: (RTree.Point,RTree.Point) -> Rectangle -> (RTree.Point,RTree.Point)
buildBiggest ((lx,hy),(hx,ly)) r = ((minx,maxy),(maxx,miny))
  where
    minx = Prelude.minimum [(fst $ ul r), (fst $ ll r), lx]
    miny = Prelude.minimum [(snd $ ll r), (snd $ lr r), ly]
    maxx = Prelude.maximum [(fst $ ur r), (fst $ lr r), hx]
    maxy = Prelude.maximum [(snd $ ur r), (snd $ ul r), hy]

{-
Builds a new 'Rectangle' from its highest leftmost corner and its lowest
rightmost corner.
-}
buildRectangle :: (RTree.Point,RTree.Point) -> Rectangle
buildRectangle (uple@(lx,hy),lori@(hx,ly)) = R uple (lx,ly) lori (hx,hy)

{-
Calculates the largest hilbert's value for a given 'Rectangle' Sequence. Assumes
that the given Sequence is ordered by the hilbert's value.
-}
largestHV :: (Seq Rectangle) -> Int
largestHV recs = hilbert $ last' recs

{-
Abstraction of the extraction of the lhv from any HRTree constructor
-}
getLHV :: HRTree -> LHV
getLHV (Leaf _ _ l) = l
getLHV (Node _ _ l) = l

{-
Abstraction of the extraction of the mbr from any HRTree constructor
-}
getMBR :: HRTree -> MBR
getMBR (Leaf _ m _) = m
getMBR (Node _ m _) = m

{-
Rebalances the tree contained on the zipper, by asking HRTrees from the brothers
of the focused 'HRTRee'. Invokes merging if necessary.
-}
askFromBrothers :: Zipper -> Zipper
askFromBrothers z@( Leaf recs lmbr llhv , crumbs ) =
  case (head' crumbs) of
    (Crumb cmbr clhv before after) ->
      (newHRTree,empty)
askFromBrothers z@( Node trees mbr lhv , crumbs ) =
  case (head' crumbs) of
    (Crumb cmbr clhv before after) ->
      (newHRTree,empty)
      --merge z i

{-
Auxiliar function which removes a rectangle from a Zipper on a Leaf.
It calls for propagating underflow or merging if necessary.
-}
remove:: Zipper -> Rectangle -> HRTree
remove (leaf@(Leaf recs mbr lhv), crumbs) r
  | S.length newRecs > 0 = reconstruct (newLeaf,crumbs)
  | otherwise            = reconstruct $ askFromBrothers (leaf,crumbs)
    where
      newRecs   = deleteFromSeq recs r
      newMBR    = maxBoundingRectangle leaf
      newLHV    = largestHV newRecs
      newLeaf   = Leaf newRecs mbr lhv

{-Exported API for RTree-}

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
insert (RTree cl cn t) r = 
  let newson ov =
        (either
         (\r -> updateMBRLHV (Leaf (S.empty |> r) emptyMBR 0))
         (\n -> updateMBRLHV (Node (S.empty |> n) emptyMBR 0)) ov) in
  do
   (maybeov,newt) <- insert' t r cl cn
   newSons <- Right $
              maybe newt (\ov->updateMBRLHV (Node (S.empty|>newt|>(newson ov)) 
                                             emptyMBR 0))
              maybeov
   Right $ RTree cl cn newSons


insert' :: HRTree -> Rectangle -> Int -> Int ->
           Either String (Maybe OvInfo,HRTree)
insert' (Leaf rs m h) r cl cn = do
  maybe (Right 0) (\_ -> Left "Ahhhh") (S.elemIndexL r rs)
  if ((S.length rs) >= cl) then
    Right $ ((Just (Left bla)) , newRecs1)
    else
    Right $ (Nothing , newRecs2)
  where (xs :> bla) = viewr (S.unstableSort (r<|rs))
        newRecs1 = updateMBRLHV (Leaf xs m h)
        newRecs2 = updateMBRLHV (Leaf (S.unstableSort (r <| rs)) m h)

insert' (Node sons m h) rect cl cn = do
  (i,node) <- Right $ pickNode sons rect
  (ovinfo,newnode) <- insert' node rect cl cn
  newsons <- if((S.length sons) > i) then
               Right $ S.update i newnode sons
             else
               Right $ sons |> newnode
  Right $ handleOverFlow (Node newsons m h) ovinfo rect cn


handleOverFlow :: HRTree -> Maybe OvInfo -> Rectangle -> Int ->
                  (Maybe OvInfo, HRTree)
handleOverFlow parent Nothing rect cn = (Nothing,updateMBRLHV parent)
handleOverFlow parent@(Node sons m h) (Just ovinfo) rect cn =
  either
  (split cn parent rect)
  ((,) Nothing)
  (insertNodeIfNotFull parent cs ovinfo cn)
    where cs = getCooperatingSibling (updateMBRLHV parent) ovinfo


insertNodeIfNotFull :: HRTree -> Maybe CoopS ->
                       OvInfo -> Int ->
                       Either OvInfo HRTree
insertNodeIfNotFull _ Nothing ovinfo _ = Left ovinfo
insertNodeIfNotFull parent (Just (CoopS (i,cs))) ovinfo cn =
  if (S.length (getSons cs)) < cn then --Wired
    Right $ updateSon i ovinfo (updateMBRLHV parent)
  else
    Left ovinfo

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


split:: Int -> HRTree -> Rectangle -> OvInfo -> (Maybe OvInfo, HRTree)
split cn parent rect ovInfo =
  if (S.length newSons <= cn) then --WIRED
    (Nothing,updateMBRLHV parent{tree=newSons})
  else
    let xs :> a = viewr newSons in
    (Just (Right a),updateMBRLHV parent{tree=xs})
  where allGrandSons = (putIntoSeq ovInfo) <| (getGrandSons parent)
        newGrandsons = redistribute (getGrandSons parent) 3
        newSons = S.zipWith replaceSons (getSons parent) newGrandsons


replaceSons :: Either Rectangle HRTree -> Seq (Either Rectangle HRTree) -> HRTree
replaceSons (Right son@(Node _ _ _)) t  = son{tree = fmap right t}
replaceSons (Right son@(Leaf _ _ _)) rs = son{recs = fmap left rs}


redistribute :: Seq (Seq a) -> Int -> Seq (Seq a)
redistribute stuffList parts = evenly stuff S.empty
  where stuff = F.foldl1 (><) stuffList
        m = max 1 ((S.length stuff) `mod` parts)
        sz = m + ((S.length stuff) `div` parts)
        evenly sequ acc =
          if (S.null sequ) then
            acc
          else
            evenly (S.drop sz sequ) (acc >< (S.empty |> (S.take sz sequ)))


-- | Construct a new empty RTree which Leaf capacity is @cl@, and which Node
--   capacity is @cn@.
newRTree :: Int      -- ^ Leaf capacity on the new 'RTree'
            -> Int   -- ^ Node capacity on the new 'RTree'
            -> RTree -- ^ New empty 'RTree' with Leaf capacity @lc@ and Node
                     --   capacity @nc@.
newRTree lc nc = RTree lc nc newHRTree

-- | Deletes a 'Rectangle' from an 'RTree'. Deleting a non-existent 'Rectangle'
--   will result in an error report.
delete :: RTree             -- ^ @RTree@ where thedeletion will be performed.
          -> Rectangle      -- ^ @Rectangle@ to delete.
          -> Either e RTree -- ^ Either an @RTree@ which is the original @RTree@
                            --   without the deleted @Rectangle@ and rebalanced
                            --   (if necessary), or an error report
                            --   (if necessary).
delete (RTree cl cn t) r = case (delete' (t,empty) r) of
  Left e -> Left $ error (show e)
  Right t -> Right $ RTree cl cn t
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
      Just i  -> return $ remove z r

{- Graphic tools to show trees -}


winSize :: Int
winSize = 900

recsSize :: Int
recsSize = 12000 --65536

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
  where  ulx = (ulxB * winSize) `div` recsSize 
         uly = (ulyB * winSize) `div` recsSize 
         lrx = (lrxB * winSize) `div` recsSize 
         lry = (lryB * winSize) `div` recsSize 
         r = (G.line (ulx,uly) (lrx,uly)): --top
             (G.line (lrx,uly) (lrx,lry)): --right
             (G.line (lrx,lry) (ulx,lry)): --bottom
             [(G.line (ulx,lry) (ulx,uly))]  --left

drawMBR :: G.Window -> Rectangle -> IO ()
drawMBR w R{ul=(ulxB,ulyB) , lr=(lrxB,lryB)} = F.mapM_ (G.drawInWindow w) r
  where  ulx = ((ulxB-50) * winSize) `div` recsSize 
         uly = ((ulyB-50) * winSize) `div` recsSize 
         lrx = ((lrxB+50) * winSize) `div` recsSize 
         lry = ((lryB+50) * winSize) `div` recsSize 
         r = withColor (Red) (G.line (ulx,uly) (lrx,uly)): --top
             withColor (Red) (G.line (lrx,uly) (lrx,lry)): --right
             withColor (Red)(G.line (lrx,lry) (ulx,lry)): --bottom
             [withColor (Red) (G.line (ulx,lry) (ulx,uly))]  --left

showTree' :: G.Window -> HRTree -> IO ()
showTree' w (Node sons mbr _) = do
  drawMBR w mbr
  F.mapM_ (showTree' w) sons
showTree' w (Leaf rs mbr _) = do
  drawMBR w mbr
  F.mapM_ (drawRectangle w) rs

r1 = R{ul=(1000 ,1000) , ur=(10000,1000),
       ll=(1000 ,10000), lr=(10000,10000)}
     
r2 = R{ul=(4000,4500), ur=(5000,4000),
       ll=(4000,6000), lr=(5000,6000)}

r4 = R{ul=(2000,2000), ur=(2500,2000),
       ll=(2000,2500), lr=(2500,2500)}

r5 = R{ul=(12000,12000), ur=(12500,12000),
       ll=(12000,12500), lr=(12500,12500)}
     
r3 = R{ul=(50000,50000), ur=(65536,50000),
       ll=(50000,65536), lr=(65536,65536)}

r6 = R{ul=(40000,40000), ur=(65536,40000),
       ll=(40000,65536), lr=(65536,65536)}
      
t1 = R{ur=(2339,5422),lr=(2339,5447),ll=(1876,5447),ul=(1876,5422)}
t2 = R{ur=(6654,6324),lr=(6654,6299),ll=(7084,6299),ul=(7084,6324)}
t3 = R{ur=(8058,6474),lr=(8058,6354),ll=(8083,6354),ul=(8083,6474)}
t4 = R{ur=(5921,7096),lr=(5921,7071),ll=(6125,7071),ul=(6125,7096)}

runTest2 :: String -> IO ()
runTest2 filename = do
  fileHandle <- openFile filename ReadMode
  bigString <- hGetContents fileHandle
  recsLines <- return $ lines bigString
  recs <- return $ map decode $! recsLines
  ini <- return (newRTree 10 10)
  test <- return $ processRecs (Right ini) $! recs
  putStrLn(bigString)
  putStrLn(show test)
  either putStrLn showTree test
  
decode :: String -> Rectangle
decode recStr = R{ur=(urx,ury),lr=(lrx,lry),ll=(llx,lly),ul=(ulx,uly)}
  where (urx:ury:lrx:lry:llx:lly:ulx:uly:xs) = map read $ words $ map commasToWthLn recStr

commasToWthLn :: Char -> Char
commasToWthLn ',' = ' '
commasToWthLn c = c

processRecs :: Either String RTree -> [Rectangle] -> Either String RTree
processRecs tree []     = tree
processRecs tree (r:rs) = do
  t <- tree
  processRecs (insert t r) rs

runTest :: IO ()
runTest = do
  ini <- return (newRTree 3 2)
  test <- return (do
                     a1 <- insert ini t1
                     a2 <- insert a1 t1
                     a3 <- insert a2 t3
                     a4 <- insert a3 t4
                     a5 <- insert a4 r5
                     a6 <- insert a5 r6
                     return a4
                 )
  putStrLn(show test)
  either putStrLn showTree test