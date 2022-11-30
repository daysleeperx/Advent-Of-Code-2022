---
to: day0<%=day%>/<%=name%>.idr
---
-- Day <%=day%>: <%=name%>

module <%=name%>
import System
import System.File
import Data.List
import Data.String.Parser
import Debug.Trace

main : IO ()
main = do
        args <- getArgs
        let (file :: _) = drop 1 args | [] => printLn "No file provided!"
        (Right symbols) <- readFile file | (Left error) => printLn error
        pure ()