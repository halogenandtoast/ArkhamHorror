import Prelude     (IO, putStrLn, ($), flip)
import Application (appMain)
import Model
import Control.Monad (forM_)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Postgresql

main :: IO ()
main =
  runStderrLoggingT $ withPostgresqlPool "dbname=arkham-horror-backend" 1 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $
      forM_ cycles $ \cycle -> insertUnique cycle

cycles :: [ArkhamHorrorCycle]
cycles =
  [ ArkhamHorrorCycle "Night of the Zealot"
  , ArkhamHorrorCycle "The Dunwhich Legacy"
  , ArkhamHorrorCycle "The Path to Carcosa"
  ]
