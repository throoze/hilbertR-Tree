import Data.Sequence
import Data.Foldable

data Rectangle = R { ul , ll , lr , ur :: ( Int , Int ) }

type LHV = Int
type MBR = Rectangle
type Point = ( Int, Int )

data RTree = RTree Int HRTree

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

-- class HRTree tree where
--   capacity :: tree -> Int
--   policy :: tree -> Int
--   isNode :: tree -> Bool
--   isLeaf :: tree -> Bool
--   isLeaf t = not $ isNode t
--   getChildren :: tree -> Seq (MBR,Rtree)
--   insert :: tree -> Rectangle -> Either e tree
--   delete :: tree -> Rectangle -> Either e tree
--   search :: tree -> Rectangle -> Maybe [ Rectangle ]
--   search t w = case (isNode t) of
--     True  ->
--     False -> a


--insert :: HRTree -> Rectangle -> Either e HRTree
--delete :: tree -> Rectangle -> Either e tree
--search :: HRTree -> Rectangle -> Maybe [ Rectangle ]
--search (Node s) r = concatmap (\ (_,_,tree) -> search tree r) $ filter (\ (mbr,_,_) -> overlapped r mbr) s