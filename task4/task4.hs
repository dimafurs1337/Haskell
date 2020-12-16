import Data.Map as M ((!), Map, fromList)
import Data.Fixed (mod')

data UnOp = UnOpNegate
          | UnOpAbs
          | UnOpLn
  deriving (Show, Eq, Ord)

data BinOp = BinOpAdd
           | BinOpSub
           | BinOpMul
           | BinOpDiv
           | BinOpMod
           | BinOpPow
           | BinOpRoot
  deriving (Show, Eq, Ord)

unOpSemantics :: Map UnOp (Float -> Float)
unOpSemantics = fromList [
    (UnOpNegate, negate),
    (UnOpAbs, abs),
    (UnOpLn, log)
    ]

root :: Float -> Float -> Float
root x y = x ** (1 / y)

binOpSemantics :: Map BinOp (Float -> Float -> Float)
binOpSemantics = fromList [
    (BinOpAdd, (+)),
    (BinOpSub, (-)),
    (BinOpMul, (*)),
    (BinOpDiv, (/)),
    (BinOpMod, mod'),
    (BinOpPow, (**)),
    (BinOpRoot, (root))
    ]

data Expression = ExConst Float
                | ExVar String
                | ExUnary UnOp Expression
                | ExBinary BinOp Expression Expression
  deriving (Show)

data RpnCommand = RpnConst Float
               | RpnUnOp  UnOp
               | RpnBinOp BinOp
               deriving (Show)
 
execRpn :: RpnCommand -> [Float] -> [Float]
execRpn (RpnConst x) zs = x:zs
execRpn (RpnUnOp op) (z:zs) = ((unOpSemantics ! op) z) : zs
execRpn (RpnBinOp op) (u:(v:zs)) = ((binOpSemantics ! op) u v) : zs
 
runRpn :: [RpnCommand] -> [Float]
runRpn = foldl (flip execRpn) []
 
expressionToRpn :: Expression -> [RpnCommand]
expressionToRpn (ExConst x) = [RpnConst x]
expressionToRpn (ExUnary op ex) = (expressionToRpn ex) ++ [RpnUnOp op]
expressionToRpn (ExBinary op ex1 ex2) = (expressionToRpn ex1) ++ (expressionToRpn ex2) ++ [RpnBinOp op]