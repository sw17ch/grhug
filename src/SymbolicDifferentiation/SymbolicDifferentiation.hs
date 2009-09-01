
data Expr = Number Rational
          | Sin Expr
          | Cos Expr
          | Neg Expr
          | Var String
          | Pow Expr Rational
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
     deriving (Eq)

instance Show Expr where
  show (Number n) = show (fromRational n::Double)
  show (Sin n) = "sin(" ++ show n ++ ")"
  show (Cos n) = "cos(" ++ show n ++ ")"
  show (Neg n) = "-" ++ show n
  show (Var s) = s
  show (Pow expr n) = show expr ++ "^" ++ show (fromRational n::Double)
  show (Add a b) = show a ++ " + " ++ show b
  show (Sub a b) = show a ++ " - " ++ show b
  show (Mul a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b
  

instance Num Expr where
  a + b = a `Add` b
  a - b = a `Sub` b
  a * b = a `Mul` b
  negate (Neg a) = a
  negate a = Neg a
  abs (Neg a) = a
  abs a = a
  signum a = undefined
  fromInteger x = Number $ fromInteger x 
    
instance Fractional Expr where
  a / b = a `Div` b
  fromRational x = Number x
  
deriv :: String -> Expr -> Expr  
deriv s (Number _) = 0
deriv s (Sin expr) = Cos expr * deriv s expr
deriv s (Cos expr) = - Sin expr * deriv s expr 
deriv s (Neg expr) = - deriv s expr
deriv s (Var name) | name == s = 1
                   | otherwise = 0
deriv s (Pow expr n) | n == 1 = deriv s expr
                     | otherwise = Number n * Pow expr (n-1) * deriv s expr
deriv s (Add a b) = deriv s a + deriv s b
deriv s (Sub a b) = deriv s a - deriv s b
deriv s (Mul a b) = (deriv s a * b) + (a * deriv s b) 
deriv s (Div a b) = (deriv s a * b) - (a * deriv s b) / (Pow b 2)

simplify :: Expr -> Expr  
simplify x = x

