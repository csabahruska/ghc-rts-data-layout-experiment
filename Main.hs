{-# LANGUAGE LambdaCase, RecordWildCards #-}
import Control.Monad.State
import Control.Monad
import Text.Pretty.Simple
import Data.List
import System.Environment
import System.FilePath

import Language.C
import Language.C.Analysis
import Language.C.System.GCC
import Language.C.Data.Ident

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

-- parseCFilePre :: FilePath -> IO (Either ParseError CTranslUnit) 

{-
  - parse files
  - report errors
  - analyse translation units
  - merge global decls
  - return single global decl
-}

parse fname = do
  result <- parseCFile (newGCC "cpp") Nothing [] fname
  case result of
    Left err -> fail $ show err
    Right tu -> pure (fname, tu)

fixTU tu@(CTranslUnit ast ni) = CTranslUnit (map cutFunBody ast) ni
 where
  cutFunBody = \case
    CFDefExt (CFunDef s d l (CCompound a _b c) n) -> CFDefExt $ CFunDef s d l ((CCompound a [] c)) n
    x -> x

processM tuList = do
  gdList <- forM tuList $ \(fname, tu) -> do
    globalDecls <- analyseAST $ fixTU tu
    let globVarDefs =
                [ varDecl -- (identToString ident, show $ pretty ty)
                | ObjectDef (ObjDef varDecl@(VarDecl (VarName ident _) _attrs ty) _ _) <- Map.elems $ gObjs globalDecls
                ]
    unless (null globVarDefs) $ do
      pPrint (takeFileName fname, map (show . pretty) globVarDefs)
    pure (globalDecls)
  pure $ foldr mergeGlobalDecls emptyGlobalDecls gdList

process action = runTravT () action >>= \case
  Left err -> fail $ show err
  Right (result, _) -> pure result

getFallbackGlobalDecls = do
  parsedFallbackDecls <- liftIO $ parse "fallback_decls.h"
  analyseAST $ snd parsedFallbackDecls

main :: IO ()
main = do
  files <- getArgs
  parsedFiles <- mapM parse files
  gd@GlobalDecls{..} <- process $ processM parsedFiles
  fallbackGD <- process getFallbackGlobalDecls

  let globVarMap :: Map String VarDecl
      globVarMap = Map.fromList
        [ (nameStr, v)
        | ObjectDef (ObjDef v@(VarDecl (VarName (Ident nameStr _ _) _) _attrs _ty) _ _) <- Map.elems gObjs
        ]
      v = globVarMap Map.! "capabilities"

      emptyEnv = Env gd fallbackGD Set.empty Nothing Nothing
  runStateT (visitGlobVar v) emptyEnv
  pure ()

data Env
  = Env
  { envGlobalDecls          :: GlobalDecls
  , envFallbackGlobalDecls  :: GlobalDecls
  , envVisitedSUERefs       :: Set SUERef
  , envCurrentMember        :: Maybe VarDecl
  , envCurrentSUERef        :: Maybe SUERef
  }

type M = StateT Env IO

visitGlobVar :: VarDecl -> M ()
visitGlobVar (VarDecl varName _declAttrs ty) = do
  pPrint ("global variable", pretty varName)
  visitType ty

visitType :: Type -> M ()
visitType t = do
 --liftIO $ pPrint ("visitType", pretty t)
 case t of
  PtrType ty _TypeQuals _Attributes                       -> visitType ty
  ArrayType ty _ArraySize _TypeQuals _Attributes          -> visitType ty
  TypeDefType (TypeDefRef _ ty _) _TypeQuals _Attributes  -> visitType ty
  FunctionType{}                                          -> pure ()
  DirectType typeName _TypeQuals _Attributes -> case typeName of
    TyComp (CompTypeRef sueRef _CompTyKind _NodeInfo) -> do
      wasMarked <- markSUERef sueRef
      unless wasMarked $ do
        visitSUERef sueRef
    _ -> pure ()

markSUERef :: SUERef -> M Bool
markSUERef ref = do
  marked <- Set.member ref <$> gets envVisitedSUERefs
  unless marked $ modify' $ \env@Env{..} -> env {envVisitedSUERefs = Set.insert ref envVisitedSUERefs}
  --pPrint ("markSUERef", pretty ref, marked)
  pure marked

lookupTagDef :: SUERef -> M TagDef
lookupTagDef ref = do
  (Map.lookup ref) <$> gets (gTags . envGlobalDecls) >>= \case
    Just tag -> pure tag
    Nothing -> do
      pPrint ("unknown tag: ", pretty ref, " fallback")
      (Map.lookup ref) <$> gets (gTags . envFallbackGlobalDecls) >>= \case
        Just tag -> pure tag
        Nothing -> fail $ "unknown tag: " ++ show ref

visitSUERef :: SUERef -> M ()
visitSUERef sueRef = do
  saved <- gets envCurrentSUERef
  modify' $ \env -> env {envCurrentSUERef = Just sueRef}
  lookupTagDef sueRef >>= \case
    EnumDef{} -> pure ()
    CompDef (CompType _SUERef _CompTyKind memberDecls _Attributes _NodeInfo) -> do
      member <- gets envCurrentMember
      pPrint ("visit type:", fmap pretty saved, fmap pretty member, pretty sueRef, map pretty memberDecls)
      forM_ memberDecls $ \case
        AnonBitField{} -> pure ()
        MemberDecl varDecl _ _ -> visitMemberVar varDecl
  modify' $ \env -> env {envCurrentSUERef = saved}

visitMemberVar :: VarDecl -> M ()
visitMemberVar vd@(VarDecl varName _declAttrs ty) = do
  saved <- gets envCurrentMember
  modify' $ \env -> env {envCurrentMember = Just vd}
  --pPrint ("field", pretty varName)
  visitType ty
  modify' $ \env -> env {envCurrentMember = saved}
