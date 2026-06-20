module Nix.Lang.Edit.Internal.TH
  ( RequiredSelectorSpec (..),
    OptionalSelectorSpec (..),
    makeRequiredSelectors,
    makeOptionalSelectors,
  )
where

import Control.Applicative ((<|>))
import Language.Haskell.TH

data RequiredSelectorSpec = RequiredSelectorSpec
  { requiredSelectorName :: String,
    requiredSelectorLabel :: String,
    requiredParentType :: Q Type,
    requiredChildType :: Q Type,
    requiredConstructor :: Name,
    requiredFieldIndex :: Int
  }

data OptionalSelectorSpec = OptionalSelectorSpec
  { optionalSelectorName :: String,
    optionalSelectorLabel :: String,
    optionalParentType :: Q Type,
    optionalChildType :: Q Type,
    optionalConstructor :: Name,
    optionalFieldIndex :: Int
  }

makeRequiredSelectors :: [RequiredSelectorSpec] -> Q [Dec]
makeRequiredSelectors = fmap concat . traverse makeRequiredSelector

makeOptionalSelectors :: [OptionalSelectorSpec] -> Q [Dec]
makeOptionalSelectors = fmap concat . traverse makeOptionalSelector

makeRequiredSelector :: RequiredSelectorSpec -> Q [Dec]
makeRequiredSelector RequiredSelectorSpec {..} = do
  fieldCount <- constructorArity requiredConstructor
  parentTy <- requiredParentType
  childTy <- requiredChildType
  spanName <- newName "span'"
  nodeName <- newName "node"
  replacementName <- newName "replacement"
  fieldNames <- traverse (newName . ("field" <>) . show) [0 .. fieldCount - 1]
  let targetName = fieldNames !! requiredFieldIndex
      replacedFields = replaceAt requiredFieldIndex (AppE (VarE (mkName "editedValue")) (VarE replacementName)) (map VarE fieldNames)
      selectorBody =
        AppE
          (AppE (VarE (mkName "required")) (LitE (StringL requiredSelectorLabel)))
          ( LamE
              [ConP (mkName "L") [] [VarP spanName, VarP nodeName]]
              ( CaseE
                  (VarE nodeName)
                  [ Match
                      (ConP requiredConstructor [] (map VarP fieldNames))
                      ( NormalB
                          (AppE (ConE (mkName "Just")) (requiredSelected spanName targetName replacementName requiredConstructor replacedFields))
                      )
                      [],
                    Match WildP (NormalB (ConE (mkName "Nothing"))) []
                  ]
              )
          )
  sig <- sigD (mkName requiredSelectorName) (pure (AppT (AppT (ConT (mkName "Selector")) parentTy) childTy))
  val <- valD (varP (mkName requiredSelectorName)) (normalB (pure selectorBody)) []
  pure [sig, val]

makeOptionalSelector :: OptionalSelectorSpec -> Q [Dec]
makeOptionalSelector OptionalSelectorSpec {..} = do
  fieldCount <- constructorArity optionalConstructor
  parentTy <- optionalParentType
  childTy <- optionalChildType
  spanName <- newName "span'"
  nodeName <- newName "node"
  replacementName <- newName "replacement"
  innerName <- newName "selectedValue"
  fieldNames <- traverse (newName . ("field" <>) . show) [0 .. fieldCount - 1]
  let targetName = fieldNames !! optionalFieldIndex
      replacedFields = replaceAt optionalFieldIndex (AppE (ConE (mkName "Just")) (AppE (VarE (mkName "editedValue")) (VarE replacementName))) (map VarE fieldNames)
      mappedSelected =
        AppE
          (AppE (VarE (mkName "flip")) (VarE (mkName "fmap")))
          (VarE targetName)
          `AppE` LamE [VarP innerName] (optionalSelected spanName innerName replacementName optionalConstructor replacedFields)
      selectorBody =
        AppE
          (AppE (VarE (mkName "optional")) (LitE (StringL optionalSelectorLabel)))
          ( LamE
              [ConP (mkName "L") [] [VarP spanName, VarP nodeName]]
              ( CaseE
                  (VarE nodeName)
                  [ Match
                      (ConP optionalConstructor [] (map VarP fieldNames))
                      ( NormalB
                          (AppE (ConE (mkName "Just")) mappedSelected)
                      )
                      [],
                    Match WildP (NormalB (ConE (mkName "Nothing"))) []
                  ]
              )
          )
  sig <- sigD (mkName optionalSelectorName) (pure (AppT (AppT (ConT (mkName "Selector")) parentTy) childTy))
  val <- valD (varP (mkName optionalSelectorName)) (normalB (pure selectorBody)) []
  pure [sig, val]

requiredSelected :: Name -> Name -> Name -> Name -> [Exp] -> Exp
requiredSelected spanName targetName replacementName conName replacedFields =
  RecConE
    (mkName "Selected")
      [ (mkName "selected", VarE targetName),
        ( mkName "replaceSelected",
          LamE
            [VarP replacementName]
            ( AppE
                (ConE (mkName "Right"))
                ( AppE
                    (ConE (mkName "Ready"))
                    ( AppE
                        (AppE (ConE (mkName "L")) (VarE spanName))
                        (foldl AppE (ConE conName) replacedFields)
                    )
                )
            )
        )
      ]

optionalSelected :: Name -> Name -> Name -> Name -> [Exp] -> Exp
optionalSelected spanName innerName replacementName conName replacedFields =
  RecConE
    (mkName "Selected")
      [ (mkName "selected", VarE innerName),
        ( mkName "replaceSelected",
          LamE
            [VarP replacementName]
            ( AppE
                (ConE (mkName "Right"))
                ( AppE
                    (ConE (mkName "Ready"))
                    ( AppE
                        (AppE (ConE (mkName "L")) (VarE spanName))
                        (foldl AppE (ConE conName) replacedFields)
                    )
                )
            )
        )
      ]

constructorArity :: Name -> Q Int
constructorArity name = do
  info <- reify name
  case info of
    DataConI _ _ parentName -> do
      parentInfo <- reify parentName
      case parentInfo of
        TyConI dec ->
          case findConstructor name dec of
            Just arity -> pure arity
            Nothing -> fail $ "Constructor not found in parent type: " <> show name
        _ -> fail $ "Unsupported parent type for constructor: " <> show name
    _ -> fail $ "Unsupported constructor: " <> show name
  where
    findConstructor conName = \case
      DataD _ _ _ _ cons _ -> foldr ((<|>) . matchCon conName) Nothing cons
      NewtypeD _ _ _ _ con _ -> matchCon conName con
      _ -> Nothing

    matchCon conName = \case
      NormalC n bangs
        | n == conName -> Just (length bangs)
      RecC n vars
        | n == conName -> Just (length vars)
      InfixC _ n _
        | n == conName -> Just 2
      ForallC _ _ con -> matchCon conName con
      GadtC names bangs _
        | conName `elem` names -> Just (length bangs)
      RecGadtC names vars _
        | conName `elem` names -> Just (length vars)
      _ -> Nothing

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx replacement xs =
  [if i == idx then replacement else x | (i, x) <- zip [0 ..] xs]
