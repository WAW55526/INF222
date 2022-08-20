module CheckProcCall
    where

import PIPLMeta
import PIPLInterpreter
import BIPL3AST
import BIPL3MetaTypes

-- | Returns True if all procedure calls in the program are well-formed.
--
-- | For a statement `ProcCall pName args` to be well formed, the procedure
-- | pName has to exist in the program, and the types of the arguments have
-- | to match those of the corresponding parameters in the procedure being
-- | called. In addition, output parameters (upd/out) should be variables.
checkProcCall :: (FunSigs, Program) -> Bool
checkProcCall env@(funSigs, program) = error "Not implemented yet"
