{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python (
  Config(..),
  generate,
  ) where

import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import System.Directory

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile "__init__.py" $ templ configFilePath [lt|
|]
  LT.writeFile "types.py" $ templ configFilePath [lt|
import sys
import msgpack
import mpidl

#{LT.concat $ map (genTypeDecl "") spec }
|]

  LT.writeFile "server.tmpl.py" $ templ configFilePath [lt|
import msgpackrpc
from types import *
# write your server here and change file name to server.py

|]

  LT.writeFile "client.py" $ templ configFilePath [lt|
import msgpackrpc
import mpidl
from types import *

#{LT.concat $ map (genClient) spec}
|]

genTypeDecl :: String -> Decl -> LT.Text

genTypeDecl _ MPType {..} = [lt|
class #{tyName}:
  @staticmethod
  def from_msgpack(arg):
    return #{fromMsgpack tyType "arg"}
|]

genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True

genTypeDecl _ _ = ""

genUnused :: Int -> T.Text
genUnused i = "_UNUSED" `mappend` T.pack (show i)

genUserDefArgs :: (Int -> T.Text) -> [Field] -> [T.Text]
genUserDefArgs unused flds = zipWith (\ix -> maybe (unused ix) fldName) [0 .. ] (sortField flds)

genArgs :: [T.Text] -> T.Text
genArgs = T.intercalate ", "

genMsg :: ToText a => a -> [Field] -> Bool -> LT.Text
genMsg name flds isExc =
  [lt|
class #{name}#{e}:
  def __init__(#{genArgs initArgs}):
#{LT.concat $ map f flds}
  def to_msgpack(self):
    return (#{LT.concat $ map typ flds}
      )

  @staticmethod
  def from_msgpack(arg):
    [#{genArgs $ map fldName flds}] = #{fromMsgpack (TTuple $ map fldType flds) "arg"}
    return #{name}(#{genArgs vs})
|]

  where
    e = if isExc then [lt|(Exception)|] else ""
    fs = genUserDefArgs genUnused flds
    vs = genUserDefArgs (\_ -> T.pack "None") flds
    initArgs = T.pack "self" : fs
    f Field {..} = [lt|    self.#{fldName} = #{fldName}
|]
    typ Field {..} = 
      let var = "self." `mappend` fldName in
      [lt|
      #{toMsgpack fldType var},|]

sortField :: [Field] -> [Maybe Field]
sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
class #{serviceName}:
  def __init__ (self, host, port):
    address = msgpackrpc.Address(host, port)
    self.client = msgpackrpc.Client(address)
#{LT.concat $ map genMethodCall serviceMethods}
|]
  where
  genMethodCall Function {..} =
    case methodRetType of
      Nothing -> [lt|
  def #{methodName} (#{genArgs args}):
    self.client.call(#{genArgs vals})
|]
      Just ts -> [lt|
  def #{methodName} (#{genArgs args}):
    retval = self.client.call(#{genArgs vals})
    return #{fromMsgpack ts "retval"}
|]
    where 
      args = T.pack "self" : genUserDefArgs genUnused methodArgs
      method = [lt|'#{methodName}'|]
      vals = map (T.pack . LT.unpack) $ method : (map genVal methodArgs)
      genVal Field {..} = toMsgpack fldType fldName

  genMethodCall _ = ""

genClient _ = ""

boolToText :: Bool -> LT.Text
boolToText True = "True"
boolToText False = "False"

typeObject :: Type -> LT.Text
typeObject (TInt signed bits) = [lt|mpidl.TInt(#{boolToText signed}, #{show bits})|]
typeObject (TFloat _) = "mpidl.TFloat()"
typeObject TBool = "mpidl.TBool()"
typeObject TString = "mpidl.TString()"
typeObject (TNullable t) = [lt|mpidl.TNullable(#{typeObject t})|]
typeObject (TList t) = [lt|mpidl.TList(#{typeObject t})|]
typeObject (TMap t1 t2) = [lt|mpidl.TMap(#{typeObject t1}, #{typeObject t2})|]
typeObject (TTuple ts) =
  [lt|mpidl.TTuple([#{LT.intercalate ", " $ map typeObject ts}])|]
typeObject (TUserDef className _) = [lt|mpidl.TUserDef(#{className})|]
typeObject TObject = "mpidl.TObject()"
typeObject TRaw = "mpidl.TRaw()"

toMsgpack :: Type -> T.Text -> LT.Text
toMsgpack typ name = [lt|#{typeObject typ}.to_msgpack(#{name})|]

fromMsgpack :: Type -> T.Text -> LT.Text
fromMsgpack typ name = [lt|#{typeObject typ}.from_msgpack(#{name})|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]
