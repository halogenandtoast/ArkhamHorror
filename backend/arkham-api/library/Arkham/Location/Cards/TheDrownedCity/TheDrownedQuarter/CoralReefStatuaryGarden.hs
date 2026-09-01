module Arkham.Location.Cards.TheDrownedCity.TheDrownedQuarter.CoralReefStatuaryGarden (coralReefStatuaryGarden) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.Enemy.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Enemies
import Arkham.Location.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CoralReefStatuaryGarden = CoralReefStatuaryGarden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralReefStatuaryGarden :: LocationCard CoralReefStatuaryGarden
coralReefStatuaryGarden = location CoralReefStatuaryGarden Cards.coralReefStatuaryGarden 4 (PerPlayer 3)

instance HasAbilities CoralReefStatuaryGarden where
  getAbilities (CoralReefStatuaryGarden a) =
    if a.revealed
      then
        extendRevealed1 a
          $ groupLimit PerGame
          $ restricted a 1 Here doubleActionAbility
      else extendUnrevealed1 a $ mkAbility a 2 $ forced $ Enters #when You (be a)

instance RunMessage CoralReefStatuaryGarden where
  runMessage msg l@(CoralReefStatuaryGarden attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 2
      gainResources iid (attrs.ability 1) 2
      createEnemyAt_ Enemies.underseaParasite attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      floodable <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid floodable increaseThisFloodLevel
      pure l
    _ -> CoralReefStatuaryGarden <$> liftRunMessage msg attrs
