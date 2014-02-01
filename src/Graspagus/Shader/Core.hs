{-# LANGUAGE GADTs, ImpredicativeTypes, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graspagus.Shader.Core
-- Copyright   :  (c) Gushcha Anton 2014
-- License     :  GNU GPLv3 (see the file LICENSE)
-- 
-- Maintainer  :  ncrashed@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Graspagus.Shader.Core(
    Shader
  ) where
  
import Control.Wire
import Data.List (init)

type Shader contex t = Wire () String Identity () (ShaderTree t) 

data ShaderTree t where
  ShaderTernaryIf :: ShaderTree Bool -> ShaderTree t -> ShaderTree t -> ShaderTree t
  ShaderBinaryOp  :: String -> ShaderTree ta -> ShaderTree tb -> ShaderTree t
  ShaderUnaryOp   :: String -> ShaderTree ta -> ShaderTree t
  ShaderFunction  :: String -> [SomeShaderTree] -> ShaderTree t
  ShaderVariable  :: String -> ShaderTree t 
  ShaderUniform   :: String -> ShaderTree t
  ShaderConstant  :: String -> ShaderTree t 

data SomeShaderTree = forall t . SomeShaderTree (ShaderTree t)

runShader :: Shader c t -> Either String String
runShader shader = case fst $ runIdentity $ stepWire shader () (Right ()) of
  Left err -> Left err 
  Right tree -> Right $ buildShader tree
  
buildShader :: ShaderTree t -> String
buildShader (ShaderTernaryIf condTree thenTree elseTree) = "(" ++ buildShader condTree ++ "?" ++ buildShader thenTree ++ ":" ++ buildShader elseTree ++ ")"
buildShader (ShaderBinaryOp op firstArg secondArg) = "(" ++ buildShader firstArg ++ op ++ buildShader secondArg ++ ")"
buildShader (ShaderUnaryOp op arg) = "(" ++ op ++ buildShader arg ++ ")"
buildShader (ShaderFunction funcName args) = funcName ++ "(" ++ init (foldl (\a (SomeShaderTree e) -> a ++ buildShader e ++ ",") "" args) ++ ")"
buildShader (ShaderVariable name) = name
buildShader (ShaderUniform name) = name
buildShader (ShaderConstant val) = val

class GPU a where
  type CPU a
  toGPU :: CPU a -> a