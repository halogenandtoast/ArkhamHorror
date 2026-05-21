module Arkham.EnemyLocation.Proxy (toEnemyLocationProxy) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor (..))
import Arkham.Classes.RunMessage.Internal (RunMessage (..))
import Arkham.EnemyLocation.Cards (allEnemyLocationCards)
import Arkham.EnemyLocation.Types
import Arkham.Location.Base (LocationAttrs)
import Arkham.Location.Types (IsLocation (..), Location, toLocation)
import Arkham.Prelude

newtype EnemyLocationProxy = EnemyLocationProxy LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasCardCode EnemyLocationProxy where
  toCardCode (EnemyLocationProxy a) = a.cardCode

instance HasCardDef EnemyLocationProxy where
  toCardDef (EnemyLocationProxy a) =
    case lookup a.cardCode allEnemyLocationCards of
      Just def -> def
      Nothing -> error $ "missing card def for enemy-location proxy " <> show a.cardCode

instance HasAbilities EnemyLocationProxy where
  getAbilities _ = []

instance HasModifiersFor EnemyLocationProxy where
  getModifiersFor _ = pure ()

instance RunMessage EnemyLocationProxy where
  runMessage _ p = pure p

instance IsLocation EnemyLocationProxy

toEnemyLocationProxy :: EnemyLocationAttrs -> Location
toEnemyLocationProxy = toLocation . EnemyLocationProxy . enemyLocationBase
