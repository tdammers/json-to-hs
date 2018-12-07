{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Main
where

import Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import Data.List (intercalate)

import Control.Monad.State
import System.IO (hPutStrLn, stderr)
import Data.Foldable

data Constraint
  = Absent
  | ExactlyNull
  | AnyString
  | AnyNumber
  | AnyBool
  | EmptyList
  | ListOf Constraint
  | EmptyObject
  | ObjectOf Constraint
  | ObjectWhere (Map Text Constraint)
  | AnyOf (Set Constraint)
  deriving (Show, Read, Eq, Ord)

mappendConstraint :: Constraint -> Constraint -> Constraint
mappendConstraint (AnyOf xs) (AnyOf ys) = AnyOf (xs <> ys)
mappendConstraint (AnyOf xs) y = AnyOf (xs <> [y])
mappendConstraint x (AnyOf ys) = AnyOf ([x] <> ys)
mappendConstraint (ListOf x) (ListOf y) = ListOf (x <> y)
mappendConstraint EmptyList (ListOf x) = ListOf x
mappendConstraint (ListOf x) EmptyList = ListOf x
mappendConstraint EmptyObject (ObjectOf x) = ObjectOf x
mappendConstraint (ObjectOf x) EmptyObject = ObjectOf x
mappendConstraint (ObjectOf x) (ObjectOf y) = ObjectOf (x <> y)
mappendConstraint (ObjectWhere x) (ObjectWhere y) = ObjectWhere $ mappendObjects x y
mappendConstraint a b = AnyOf [a, b]

mappendObjects :: Map Text Constraint -> Map Text Constraint -> Map Text Constraint
mappendObjects =
  Map.merge
    (Map.mapMissing (\_ -> (Absent <>)))
    (Map.mapMissing (\_ -> (Absent <>)))
    (Map.zipWithMatched (const (<>)))

instance Semigroup Constraint where
  (<>) = mappendConstraint

analyze :: Value -> Constraint
analyze Null = ExactlyNull
analyze (String t) = AnyString
analyze (Number n) = AnyNumber
analyze (Bool b) = AnyBool
analyze (Array []) = EmptyList
analyze (Array xs) = ListOf $ foldl1 (<>) . fmap analyze $ xs
-- analyze (Object o) = ObjectWhere . Map.fromList . map (\(k,v) -> (k, analyze v)) . HashMap.toList $ o
analyze (Object []) = EmptyObject
analyze (Object o) = ObjectOf $ foldl1 (<>) . fmap analyze $ HashMap.elems o

type Render = State (Map String String) -- type name, definition

render :: Constraint -> Render String
render (ListOf c) = do
  inner <- render c
  return $ "[" ++ inner ++ "]"
render (ObjectOf c) = do
  inner <- render c
  return $ "(Map Text " ++ inner ++ ")"
render (AnyOf items) = renderAnyOf items
render (ObjectWhere o) = error "Structured objects not implemented yet"
render ExactlyNull = return "()"
render EmptyList = return "()"
render EmptyObject = return "()"
render AnyNumber = return "Scientific"
render x = return $ getTypeName x

renderAnyOf :: Set Constraint -> Render String
renderAnyOf constraints = do
  if (ExactlyNull `Set.member` constraints || Absent `Set.member` constraints)
      then do
        inner <- renderAnyOf (constraints `Set.difference` [ExactlyNull, Absent])
        return $ "(Maybe " ++ inner ++ ")"
  else case Set.toAscList constraints of
      [x] ->
          render x
      [] ->
          error "Empty constraint set"
      _ ->
          concoctSumType (getTypeName (AnyOf constraints)) (Set.toAscList constraints)

getTypeName :: Constraint -> String
getTypeName Absent = "Void"
getTypeName ExactlyNull = "Unit"
getTypeName EmptyList = "Unit"
getTypeName EmptyObject = "Unit"
getTypeName AnyString = "Text"
getTypeName AnyNumber = "Number"
getTypeName AnyBool = "Bool"
getTypeName (ListOf c) = "ListOf" ++ getTypeName c ++ "_"
getTypeName (ObjectOf c) = "ObjectOf" ++ getTypeName c ++ "_"
getTypeName (AnyOf items) = "Either" ++ concat (map getTypeName (Set.toAscList items)) ++ "_"
getTypeName (ObjectWhere o) = error "Objects not implemented yet"

concoctSumType :: String -> [Constraint] -> Render String
concoctSumType name constraints = do
  (Map.lookup name <$> get) >>= \case
    Just def ->
      -- already exists, don't generate again
      return name
    Nothing -> do
      ctors <- mapM (\constraint -> do
                  ty <- render constraint
                  let ctorName = name ++ "_" ++ getTypeName constraint
                  return $ ctorName ++ " " ++ ty)
                constraints
      let def = "data " ++ name ++
                "\n    = " ++
                intercalate "\n    | " ctors ++
                "\n    deriving (Show, Read, Eq)\n"
      modify $ Map.insert name def
      return name

main :: IO ()
main = do
  inStr <- LBS.getContents
  (inVal :: Value) <- either error return $ eitherDecode inStr

  let constraints = analyze inVal

  let (ty, tyMap) = runState (render constraints) Map.empty
  putStrLn "import Data.Map (Map)"
  putStrLn "import Data.Scientific (Scientific)"
  putStrLn ""
  mapM putStrLn $ Map.elems tyMap
  putStrLn $ "type T = " ++ ty
