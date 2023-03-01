module SetAVL where

type Depth = Int

data Set = Nil | Node Set Int Set Depth
  deriving (Show)

invariant :: Set -> Bool
invariant Nil = True
invariant (Node l n r d) =
  and [m < n | m <- list l]
    && and [m > n | m <- list r]
    && abs (depth l - depth r) <= 1
    && d == 1 + (depth l `max` depth r)
    && invariant l
    && invariant r

list :: Set -> [Int]
list Nil = []
list (Node l n r _) = list l ++ [n] ++ list r

empty :: Set
empty = Nil

depth :: Set -> Int
depth Nil = 0
depth (Node _ _ _ d) = d

node :: Set -> Int -> Set -> Set
node l n r = Node l n r (1 + depth l `max` depth r)

insert :: Int -> Set -> Set
insert m Nil = node empty m empty
insert m (Node l n r _)
  | m == n = node l n r
  | m < n = rebalance (node (insert m l) n r)
  | m > n = rebalance (node l n (insert m r))

rebalance :: Set -> Set
rebalance (Node (Node a m b _) n c _)
  | depth a >= depth b && depth a > depth c =
      node a m (node b n c)
rebalance (Node a m (Node b n c _) _)
  | depth c >= depth b && depth c > depth a =
      node (node a m b) n c
rebalance (Node (Node a m (Node b n c _) _) p d _)
  | depth (node b n c) > depth d =
      node (node a m b) n (node c p d)
rebalance (Node a m (Node (Node b n c _) p d _) _)
  | depth (node b n c) > depth a =
      node (node a m b) n (node c p d)
rebalance a = a
