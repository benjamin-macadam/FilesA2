module ParseProg where

import LexAssignment
import ParAssignment
import AbsAssignment
import LayoutAssignment
import qualified AST as A 
import ASTConverter
import ErrM 

myLLexer = resolveLayout True . myLexer


progToAST :: String -> A.Prog String String
progToAST fileConts = do  
    case pTree of
        Bad emsg ->
           error $ "Error in parsing \n" ++ emsg
        Ok fpTree -> 
           transProg fpTree  
      where
          tokens = myLLexer fileConts
          pTree  = pProg tokens

parseFile :: String -> IO (A.Prog String String)
parseFile fname = do 
    fconts <- readFile fname 
    let ast = progToAST fconts
    putStrLn $ show ast 
    return $ ast 

showProg ast = A.show_prog ast  