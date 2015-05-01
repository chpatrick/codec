module Data.Codec.TH (genFields) where

import Data.Foldable (foldl')
import Data.Traversable (for)
import Language.Haskell.TH

import Data.Codec.Common

replaceAt :: a -> Int -> [ a ] -> [ a ]
replaceAt x i xs = pr ++ x : suf
  where ( pr, _ : suf ) = splitAt i xs

deleteAt :: Int -> [ a ] -> [ a ]
deleteAt i xs = pr ++ suf
  where ( pr, _ : suf ) = splitAt i xs

-- | Generate `Field`s for a given data type. Currently only single-constructor records are supported.
-- Each record field @a@ will be turned into a `Field` @f_a@.
genFields :: Name -> Q [ Dec ]
genFields n = reify n >>= \case
  TyConI (DataD [] _ vs [ RecC _ fs ] _) -> do
      let fc = length fs
          pfns = map (\j -> mkName ("arg" ++ show j)) [1..fc] -- polymorphic field names (a1, a2..)
          pfts = map VarT pfns -- polymorphic field types
      vns <- for vs $ \case
        PlainTV vn -> return vn
        KindedTV vn k | k == starK -> return vn
        _ -> fail "Only simple type variables supported."
      let rt = foldl' (\t v -> AppT t (VarT v)) (ConT n) vns
      kot <- [t|X|]
      fds <- for (zip [0..] fs) $ \( i, ( fn, _, ft ) ) -> do
        pfas <- for [1..fc] $ \j -> newName ("a" ++ show j) -- polymorphic field arguments
        let fan = mkName ("f_" ++ nameBase fn)
            mkCtr = foldr (\t t' -> AppT (AppT ArrowT t) t') rt
            xt = mkCtr $ replaceAt ft i pfts
            yt = mkCtr $ replaceAt kot i pfts
            apply c x = foldl' AppE (VarE c) $ replaceAt (VarE x) i $ map VarE pfas
            applicator' c x = foldr (\pfap -> LamE [ pfap ]) (apply c x) $ replaceAt WildP i $ map VarP pfas
            applicator = [|\x c -> $(pure $ applicator' 'c 'x)|]
            extractor = VarE fn
        fat <- ForallT (map PlainTV (vns ++ deleteAt i pfns)) [] <$> [t|Field $(pure rt) $(pure ft) $(pure xt) $(pure yt) |]
        body <- [|Field $(applicator) $(pure extractor)|]
        return [ SigD fan fat, ValD (VarP fan) (NormalB body) [] ]
      return $ concat fds
  _ -> fail "Unsupported record type."