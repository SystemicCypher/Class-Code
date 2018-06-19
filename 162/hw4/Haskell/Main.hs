import Parser
import Ast
import Typechecker

-- To load input, either pipe a file into the program,
-- or type your input manually and press Control+D to signal
-- the end of input.

main = do
  parsed <- fmap parseProgram getContents
  -- print parsed -- in case you need to view the AST structure
  case parsed of
    Left err   -> putStrLn $ "Syntax error: " ++ (show err)
    Right prog -> case (typeOf prog defaultEnv) of
      Left  err -> putStrLn $ "Type error: " ++ err
      Right t   -> print t

