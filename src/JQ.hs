{-# LANGUAGE OverloadedStrings #-}
module JQ where

import Control.Applicative
import Control.Lens.At
import Control.Lens.Fold (filtered)
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Monad
import Data.Aeson (Value)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.Aeson.Types qualified as J
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Scientific
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Exts (IsString(..), fromList)

-- * Syntax

type Var = String

data Filter
  = Id -- .
  | Filter :| Filter -- |
  | Empty -- empty
  | Filter `Comma` Filter -- ,
  | Null
  | Bool Bool
  | Number Scientific
  | String String
  | Array Filter -- […]
  | Object [(Filter, Filter)] -- {…}
  | Index Filter Optional -- .[…]
  | Values Optional -- .[]
  | Path Filter -- path
  | Del Filter -- del
  | Filter :+ Filter -- +
  | Filter :- Filter -- -
  | Filter :* Filter -- *
  | Filter :/ Filter -- /
  | Filter :% Filter -- %
  | Filter :// Filter -- //
  | If Filter Filter Filter -- if … then … else … end
  | Length -- length
  | Type -- type
  | Filter :|= Filter -- |=
  | Filter := Filter -- =
  | Bind Var Filter Filter -- … as $v | …
  | Var Var -- $v
  deriving (Show)

data Optional = Do | Try -- ?
  deriving (Show)

map' f = Array (Values Do :| f)
select f = If f Id Empty

-- * Semantics

type Journey s t a b = forall m. Monad m => (a -> m b) -> s -> m t
type Journey' s a = Journey s s a a
type IndexedJourney i s t a b = forall p m. (Indexable i p, Monad m) => p a (m b) -> s -> m t
type IndexedJourney' i s a = IndexedJourney i s s a a

-- toListOf :: Journey s t a a -> s -> [a]
toListOf l s = k [] where (Endo k, _) = l (\ a -> (Endo (a:), a)) s

-- itoListOf :: IndexedJourney i s t a a -> s -> [(i, a)]
itoListOf l s = k [] where (Endo k, _) = l (Indexed \ i a -> (Endo ((i, a):), a)) s

infixl 8 ^..
(^..) :: s -> Journey s t a a -> [a]
s ^.. l = toListOf l s

folding :: Foldable f => (s -> f a) -> IndexedJourney Path s s a b
folding f k s = s <$ traverse_ (indexed k Nowhere) (f s) -- TODO error on assignment?

to :: (s -> a) -> IndexedJourney Path s s a b
to f = folding ((:[]) . f)

like :: a -> IndexedJourney Path s s a b
like a = to (const a)

adjoin :: Monad m => (r -> a -> m b) -> (r -> b -> m c) -> r -> a -> m c
adjoin p q k = p k >=> q k

visiting :: (Foldable f, Monad m) => (s -> f (r -> s -> m s)) -> r -> s -> m s
visiting f k s = foldlM (\ a l -> l k a) s (f s)

data Path = Nowhere | At [Value]

instance Semigroup Path where
  At a <> At b = At (a <> b)
  _ <> _ = Nowhere
instance Monoid Path where
  mempty = At mempty

unPath (At i, _) = i
unPath (Nowhere, v) = error ("invalid path expression with result " <> show v)

type Env = KM.KeyMap Value

jq :: Env -> Filter -> IndexedJourney' Path Value Value
jq env = \case
  Id -> \ k -> indexed k (mempty :: Path)
  a :| b -> icompose (<>) (jq env a) (jq env b)
  Empty -> ignored
  a `Comma` b -> jq env a `adjoin` jq env b
  Null -> like J.Null
  Bool b -> like (J.Bool b)
  Number n -> like (J.Number n)
  String s -> like (J.String (fromString s))
  Array a -> to (Value_ . toListOf (jq env a))
  Object o -> folding \ s -> J.object <$> traverse (\ (k, v) -> liftA2 mkPair (s ^.. jq env k) (s ^.. jq env v)) o
  Index i l -> visiting (toListOf (jq env i . to (\ i' k -> index' l i' (indexed k (At [i'])))))
  Values l -> reindexed (At . (:[])) (values' l)
  Path p -> folding (map (Value_ . unPath) . itoListOf (jq env p))
  Del p -> to (\ s -> delpaths (unPath <$> itoListOf (jq env p) s) s)
  a :+ b -> folding \ s -> liftA2 add (s ^.. jq env a) (s ^.. jq env b)
  a :- b -> folding \ s -> liftA2 sub (s ^.. jq env a) (s ^.. jq env b)
  a :* b -> folding \ s -> liftA2 mul (s ^.. jq env a) (s ^.. jq env b)
  a :/ b -> folding \ s -> liftA2 divide (s ^.. jq env a) (s ^.. jq env b)
  a :% b -> folding \ s -> liftA2 modulo (s ^.. jq env a) (s ^.. jq env b)
  a :// b -> visiting \ s -> [if any defined (s ^.. jq env a) then jq env a . filtered defined else jq env b]
  If cond yes no -> visiting (toListOf (jq env cond . to (\ b -> jq env (if truthy b then yes else no))))
  Length -> to (J.Number . fromIntegral . length')
  Type -> to (J.String . type')
  p :|= v -> folding (jq env p %%~ toListOf (jq env v))
  p := v -> folding \ s -> s ^.. jq env v . to (set (jq env p) `flip` s)
  Bind v e f -> visiting \ s -> s ^.. jq env e . to (\i -> jq (KM.insert (J.fromString v) i env) f)
  Var v | Just i <- env KM.!? J.fromString v -> like i
        | otherwise -> error ("unbound variable " <> v)
  where
  mkPair (J.String k) v = (J.fromText k, v)
  mkPair i _ = error ("cannot use " <> type' i <> " as object key")
  nth' i k v | 0 <= i, i < n = ix i k v
             | -n <= i, i < 0 = ix (n + i) k v
             | i >= n = k J.Null <&> \ a' -> v <> V.replicate (i - n) J.Null <> V.singleton a'
             | otherwise = k J.Null <&> error "negative index out of bounds"
             where n = V.length v
  index' _ (J.Number n) k (J.Array a) = case floatingOrInteger n of
    Left (_ :: Double) -> error "cannot index array with fractional index"
    Right i -> J.Array <$> nth' i k a
  index' l (J.Number n) k J.Null = index' l (J.Number n) k J.emptyArray
  index' _ (J.String s) k (J.Object o) = J.Object <$> (at (J.fromText s) . orNull) k o
  index' l (J.String s) k J.Null = index' l (J.String s) k J.emptyObject
  index' _ (J.Object _) _ (J.Array _) = error "TODO: slices"
  index' _ (J.Object _) _ (J.String _) = error "TODO: slices"
  index' Do i _ v = error ("cannot index " <> type' v <> " with " <> type' i)
  index' Try _ _ v = pure v
  orNull :: Iso' (Maybe Value) Value
  orNull = anon J.Null (const False)
  values' _ k (J.Array a) = J.Array <$> reindexed Value_ traversed k a
  values' _ k (J.Object o) = J.Object <$> reindexed Value_ itraversed k o
  values' Do _ s = error ("cannot iterate over " <> type' s)
  values' Try _ s = pure s
  delpaths _ _ = error "TODO: delpaths"
  add J.Null b = b
  add a J.Null = a
  add (J.Number n) (J.Number m) = J.Number (n + m)
  add (J.String n) (J.String m) = J.String (n <> m)
  add (J.Array n) (J.Array m) = J.Array (n <> m)
  add (J.Object n) (J.Object m) = J.Object (KM.unionWith (const id) n m)
  add a b = error ("cannot add " <> type' a <> " and " <> type' b)
  sub (J.Number n) (J.Number m) = J.Number (n - m)
  sub (J.Array n) (J.Array m) = J.Array (fromList (toList n \\ toList m))
  sub a b = error ("cannot subtract " <> type' a <> " and " <> type' b)
  mul (J.Number n) (J.Number m) = J.Number (n * m)
  mul (J.Object n) (J.Object m) = J.Object (n `merge` m)
  mul (J.String s) (J.Number n) = mul (J.Number n) (J.String s)
  mul (J.Number n) (J.String s) = J.String (T.replicate (truncate n) s)
  mul a b = error ("cannot multiply " <> type' a <> " and " <> type' b)
  merge = KM.unionWith mergeValues
  mergeValues (J.Object n) (J.Object m) = J.Object (n `merge` m)
  mergeValues _ b = b
  divide (J.Number n) (J.Number m) = J.Number (n / m)
  divide (J.String n) (J.String m) = J.Array (fromList (map J.String (T.splitOn m n)))
  divide a b = error ("cannot divide " <> type' a <> " and " <> type' b)
  modulo (J.Number n) (J.Number m) = J.Number (fromInteger (truncate n `rem` truncate m))
  modulo a b = error ("cannot take remainder of " <> type' a <> " and " <> type' b)
  truthy J.Null = False
  truthy (J.Bool b) = b
  truthy v = error (type' v <> " has no truth value")
  defined J.Null = False
  defined (J.Bool False) = False
  defined _ = True
  length' J.Null = 0
  length' (J.String s) = T.length s
  length' (J.Array a) = length a
  length' (J.Object o) = length o
  length' v = error (type' v <> " has no length")
  type' J.Null = "null"
  type' (J.Bool _) = "boolean"
  type' (J.Number _) = "number"
  type' (J.String _) = "string"
  type' (J.Array _) = "array"
  type' (J.Object _) = "object"
