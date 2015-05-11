module Data.Codec.TH (genFields) where

import Control.Applicative
import Data.Foldable (foldl')
import Data.Traversable (for, traverse)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

import Data.Codec.Field as F

replaceAt :: a -> Int -> [ a ] -> [ a ]
replaceAt x i xs = pr ++ x : suf
  where ( pr, _ : suf ) = splitAt i xs

deleteAt :: Int -> [ a ] -> [ a ]
deleteAt i xs = pr ++ suf
  where ( pr, _ : suf ) = splitAt i xs

fun :: Type -> Type -> Type
fun x = AppT (AppT ArrowT x)

genField :: [ Name ] -> Type -> Int -> ( Int, VarStrictType ) -> Q [ Dec ]
genField recVars recType fc ( i, ( fn, _, ft ) ) = do
  polyNames <- for [1..fc] $ \j -> do
    let pn = "arg" ++ show j
    if any (\rv -> nameBase rv == pn) recVars
      then newName pn
      else return $ mkName pn
  let polyTypes = map VarT polyNames
      polyArgs = map (\j -> mkName $ "arg" ++ show j) [1..fc]
      fieldVars = map PlainTV $ recVars ++ deleteAt i polyNames
      fieldName = mkName ("f_" ++ nameBase fn)
      r = pure recType
      a = pure ft
      x = pure $ foldr fun recType $ replaceAt ft i polyTypes
      y = pure $ foldr fun recType $ replaceAt (ConT ''X) i polyTypes
      mkApplicator c v = pure $ LamE argPats app
        where
          app = foldl' AppE (VarE c) $ map VarE $ replaceAt v i polyArgs
          argPats = replaceAt WildP i $ map VarP polyArgs
      -- \c x -> \a1 -> .. \_ -> .. \an -> c a1 .. x .. an
      applicator = [|\v c -> $(mkApplicator 'c 'v)|]
      extractor = pure $ VarE fn
  fieldType <- ForallT fieldVars [] <$>
    [t|Field $r $a $x $y|]
  fieldBody <-
    [|Field $applicator $extractor|]
  return [ SigD fieldName fieldType, ValD (VarP fieldName) (NormalB fieldBody) [] ]

genCon :: [ Name ] -> Type -> Int -> TH.Con -> Q [ Dec ]
genCon recVars recType cc
  = \case
    RecC cName fields -> genCon' cName fields
    NormalC cName [] -> genCon' cName []
    _ -> fail "Unsupported constructor."
  where
    genCon' cName fields = do
      let fieldTypes = [ ft | ( _, _, ft ) <- fields ]
          conName = mkName ("c_" ++ nameBase cName)
          cType = foldr fun recType fieldTypes
          conMatch
            | cc == 1 = [|const True|]
            | otherwise = [|\r -> $(mkConMatch 'r)|]
          mkConMatch r = pure $ CaseE (VarE r)
                         [ Match (RecP cName []) (NormalB (ConE 'True)) []
                         , Match WildP (NormalB (ConE 'False)) []
                         ]
          fc = length fields
      conType <- ForallT (map PlainTV recVars) [] <$> [t|F.Con $(pure recType) $(pure cType)|]
      conBody <- [|F.Con $(pure $ ConE cName) $conMatch|]
      fDecs <- traverse (genField recVars recType fc) $ zip [0..] fields
      return $
        [ SigD conName conType
        , ValD (VarP conName) (NormalB conBody) []
        ] ++ concat fDecs

-- | Generate `Field`s for a given data type. Currently only single-constructor records are supported.
-- Each record field @a@ will be turned into a `Field` @f_a@, and all constructors will be turned into `Con`s.
genFields :: Name -> Q [ Dec ]
genFields n = reify n >>= \case
  TyConI (DataD [] _ vs cs _) -> do
      recVars <- for vs $ \case
        PlainTV vn -> return vn
        KindedTV vn k | k == starK -> return vn
        _ -> fail "Only simple type variables supported."
      let recType = foldl' (\t v -> AppT t (VarT v)) (ConT n) recVars
          cc = length cs
      concat <$> traverse (genCon recVars recType cc) cs
  _ -> fail "Unsupported record type."
