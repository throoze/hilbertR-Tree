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
         newRTree,       -- :: Int -> Int -> RTree
         -- * Insertion
         --insert,         -- :: RTree -> Rectangle -> Either e RTree
         -- * Deletion
         delete,         -- :: RTree -> Rectangle -> Either e RTree
         -- * Queries
         --search,         -- :: RTree -> Rectangle -> Maybe [ Rectangle ]
       ) where

import Data.Sequence as S
import Data.Foldable as F
import Data.Bits
import Control.Monad.Error

-- | General representation of a rectangle.
data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) } deriving (Eq, Show)

-- Establish the Rectanles order relation by comparing them on their centroid
-- hilbert's value.
instance Ord Rectangle where
  r1 < r2 = hilbert r1 < hilbert r2

-- Some convenience aliases and constructors
type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

-- | Representation of the data type 'RTree'
data RTree = RTree Int Int HRTree deriving (Show)

{- Internal representation of the tree. The wrapper 'RTree' is used and exported
instead, so the user can define a maximum leaf capacity and a maximum node
capacity, on creating a new 'RTree'.
-}
data HRTree = Node {tree::(Seq HRTree), mbr :: MBR, lhv :: LHV}
            | Leaf {recs::(Seq Rectangle), lmbr :: MBR, llhv :: LHV}
            deriving(Show)

-- Internal model for applying the Zipper technique.
data Crumb = Crumb MBR LHV (Seq HRTree) (Seq HRTree)
type Zipper = ( HRTree , (Seq Crumb) )


-- | Creates a new empty meaningless Rectangle.
emptyRectangle :: Rectangle -- ^ An empty degenerated rectangle.
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
            -> Point  -- ^ Centroid of the 'Rectangle' passed.
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
hilbert r = hilbertDistance 65536 $ centroid r

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
newHRTree = Leaf empty emptyRectangle 0

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
Returns the maximum bounding rectangles for a given 'HRTree'.
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
buildBiggest :: (Point,Point) -> Rectangle -> (Point,Point)
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
buildRectangle :: (Point,Point) -> Rectangle
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
getlhv :: HRTree -> LHV
getlhv (Leaf _ _ l) = l
getlhv (Node _ _ l) = l

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
      (newHRTree,tail)
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
      Just i  -> return $ remove z r