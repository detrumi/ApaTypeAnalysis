module Expr where

type Var = String
type TypeVar = String

-- Statements
data Statement
    = SBind Bind                            -- Variable / function binding
    | SOperator [(Operator, Precedence)]    -- Operator precedence
    | SData TypeVar [Var] [DataCon]         -- Data constructors
    deriving (Eq, Show)

type Operator = String
type Precedence = Int

data Bind = Bind Var Expr
    deriving (Eq, Show)
data DataCon = DataCon TypeVar [Type]
    deriving (Eq, Show)

-- Expressions
data Expr
    = Const Value
    | Var Var
    | Let [Bind] Expr
    | Lam Var Expr
    | App Expr Expr
    | If Expr Expr Expr
    | Case Expr [(Expr, Expr)]
    | Con TypeVar [Expr]
    | Infix [Expr] [Operator]
    deriving (Eq, Show)

data Value
    = IntVal { fromIntVal :: Int }
    deriving (Eq, Show)

data Type
    = TInt
    | TVar Var
    | TCon TypeVar [Type]
    | TFunc Type Type
    deriving (Eq, Show)
