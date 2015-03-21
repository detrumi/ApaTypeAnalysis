module Expr where

type Var = String
type TypeVar = String

data Statement
    = SBind Bind
    | SOperator [(Operator, Precedence)]
    | SData TypeVar [Var] [DataCon]
    deriving (Eq, Show)

type Operator = String
type Precedence = Int

data Bind = Bind Var [Expr] Expr
    deriving (Eq, Show)
data DataCon = DataCon TypeVar [Type]
    deriving (Eq, Show)

data Expr
    = Const Value
    | Var Var
    | Let [Bind] Expr
    | Lam [Var] Expr
    | App Expr [Expr]
    | If Expr Expr Expr
    | Case Expr [(Expr, Expr)]
    | Con TypeVar [Expr]
    | Infix [Expr] [Operator]
    deriving (Eq, Show)

data Value
    = IntVal { fromIntVal :: Int }
    | Unit
    | List [Expr]
    deriving (Eq, Show)

data Type
    = TUnit
    | TInt
    | TVar Var
    | TCon TypeVar [Type]
    | Top
    | TList Type
    deriving (Eq, Show)