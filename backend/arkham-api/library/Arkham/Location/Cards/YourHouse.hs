module Arkham.Location.Cards.YourHouse (YourHouse (..), yourHouse) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype YourHouse = YourHouse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourHouse :: LocationCard YourHouse
yourHouse = location YourHouse Cards.yourHouse 2 (PerPlayer 1)

instance HasModifiersFor YourHouse where
  getModifiersFor (YourHouse attrs) = do
    modifySelect attrs (enemyIs Cards.ghoulPriest) [ForceSpawnLocation (be attrs)]

-- Ability 1 is just to trigger in UI, it doesn't change anything since we
-- handle it with the modifier
instance HasAbilities YourHouse where
  getAbilities (YourHouse x) =
    extendRevealed
      x
      [ forcedAbility x 1 $ EnemySpawns #when Anywhere $ enemyIs Cards.ghoulPriest
      , playerLimit PerTurn
          $ restrictedAbility x 2 (Here <> youExist (oneOf [can.draw.cards, can.gain.resources])) actionAbility
      ]

instance RunMessage YourHouse where
  runMessage msg l@(YourHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCardsIfCan iid (attrs.ability 2) 1
      gainResourcesIfCan iid (attrs.ability 2) 1
      pure l
    _ -> YourHouse <$> liftRunMessage msg attrs
