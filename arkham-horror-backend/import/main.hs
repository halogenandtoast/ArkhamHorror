import Arkham.Fixtures
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Prelude (IO, flip, ($))

main :: IO ()
main =
  runStderrLoggingT
    $ withPostgresqlPool "dbname=arkham-horror-backend" 1
    $ \pool -> liftIO $ flip runSqlPersistMPool pool $ do
        forM_ allInvestigators (`upsert` [])
        forM_ allProductSets $ \productSet -> do
          productSetE <- upsert productSet []
          forM_ (encounterSetsFor productSetE) (`upsert` [])
