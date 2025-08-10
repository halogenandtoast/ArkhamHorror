module Arkham.Enemy.Cards.FeatheredSerpent (featheredSerpent) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field(..))
import Arkham.Token
import Arkham.Projection
import Arkham.Matcher

newtype FeatheredSerpent = FeatheredSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

featheredSerpent :: EnemyCard FeatheredSerpent
featheredSerpent = enemy FeatheredSerpent Cards.featheredSerpent (0, Static 3, 3) (1, 0)

instance HasModifiersFor FeatheredSerpent where
  getModifiersFor (FeatheredSerpent a) = do
    selectOne (locationIs Locations.mouthOfKnYanTheCavernsMaw) >>= traverse_ \loc -> do
      pillars <- fieldMap LocationTokens (countTokens Pillar) loc
      modifySelfWhen a (pillars > 0) [EnemyFight pillars]
      modifySelfWhen a (pillars >= 3) [AddKeyword Keyword.Alert, AddKeyword Keyword.Hunter]

instance RunMessage FeatheredSerpent where
  runMessage msg (FeatheredSerpent attrs) = runQueueT $ case msg of
    _ -> FeatheredSerpent <$> liftRunMessage msg attrs
