module Data.Codec.TH (genFields) where

import Data.Foldable (foldl')
import Data.Traversable (for)
import Language.Haskell.TH

import Data.Codec

replaceAt :: a -> Int -> [ a ] -> [ a ]
replaceAt x i xs = pr ++ x : suf
  where ( pr, _ : suf ) = splitAt i xs

deleteAt :: Int -> [ a ] -> [ a ]
deleteAt i xs = pr ++ suf
  where ( pr, _ : suf ) = splitAt i xs

genFields :: Name -> Q [ Dec ]
genFields n = reify n >>= \case
  TyConI (DataD [] _ [] [ RecC _ fs ] _) -> do
      let rt = ConT n
          fc = length fs
          pfns = map (\j -> mkName ("a" ++ show j)) [1..fc] -- polymorphic field names (a1, a2..)
          pfts = map VarT pfns -- polymorphic field types
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
        fat <- ForallT (deleteAt i $ map PlainTV pfns) [] <$> [t|Field $(pure rt) $(pure ft) $(pure xt) $(pure yt) |]
        body <- [|Field $(applicator) $(pure extractor)|]
        return [ SigD fan fat, ValD (VarP fan) (NormalB body) [] ]
      return $ concat fds
  _ -> fail "Unsupported record type."