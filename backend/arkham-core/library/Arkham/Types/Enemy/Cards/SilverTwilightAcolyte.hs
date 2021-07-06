module Arkham.Types.Enemy.Cards.SilverTwilightAcolyte
  ( SilverTwilightAcolyte(..)
  , silverTwilightAcolyte
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyCard SilverTwilightAcolyte
silverTwilightAcolyte = enemyWith
  SilverTwilightAcolyte
  Cards.silverTwilightAcolyte
  (2, Static 3, 3)
  (1, 0)
  (preyL .~ SetToBearer)

instance HasModifiersFor env SilverTwilightAcolyte where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SilverTwilightAcolyte where
  getActions i window (SilverTwilightAcolyte attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SilverTwilightAcolyte where
  runMessage msg (SilverTwilightAcolyte attrs@EnemyAttrs {..}) = case msg of
    EnemyAttack _ eid | eid == enemyId -> do
      push PlaceDoomOnAgenda
      SilverTwilightAcolyte <$> runMessage msg attrs
    _ -> SilverTwilightAcolyte <$> runMessage msg attrs
