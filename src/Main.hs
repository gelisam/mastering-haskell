module Main where
import Control.Concurrent.Async

handle :: Request -> IO Response
handle request = do
  asyncBody <- async $ parseBody request  --  ----.
  logForDebugging      request            --      |
  logForFaultTolerance request            --      |
  logForAudit          request            --      |
  Bearer token <- getBearerToken request  --      |
  userId <- lookupTokenOwner token        --      v
  TransferFunds amount account1 account2 <- wait asyncBody
  userId' <- lookupAccountOwner account1
  if userId == userId'
  then do asyncMutex <- async $ lockAccount account1  --  ----.
          balance <- getBalance account1              --      |
          r <- if balance >= amount                   --      |
               then do debitAmount account1 amount    --      |
                       creditAmount account2 amount   --      |
                       return $ Response 200 "OK"     --      |
               else return $ Response 400 "Nope"      --      |
          unlock =<< wait asyncMutex  --  <-------------------'
          return r
  else return $ Response 401 "Unauthorized"
  





data Request

newtype Amount = Amount Int deriving (Eq, Ord)
data Account
data RequestBody = TransferFunds Amount Account Account

data Token
data BearerToken = Bearer Token

newtype User = User Int deriving Eq

data Mutex

data Response = Response Int String

parseBody :: Request -> IO RequestBody
parseBody = undefined

logForDebugging :: Request -> IO ()
logForDebugging = undefined

logForFaultTolerance :: Request -> IO ()
logForFaultTolerance = undefined

logForAudit :: Request -> IO ()
logForAudit = undefined

getBearerToken :: Request -> IO BearerToken
getBearerToken = undefined

lookupTokenOwner :: Token -> IO User
lookupTokenOwner = undefined

lookupAccountOwner :: Account -> IO User
lookupAccountOwner = undefined

lockAccount :: Account -> IO Mutex
lockAccount = undefined

getBalance :: Account -> IO Amount
getBalance = undefined

debitAmount :: Account -> Amount -> IO ()
debitAmount = undefined

creditAmount :: Account -> Amount -> IO ()
creditAmount = undefined

unlock :: Mutex -> IO ()
unlock = undefined



main :: IO ()
main = return ()
