module Arkham.Location.Cards.HallOfLoyalty (hallOfLoyalty, HallOfLoyalty (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfLoyalty = HallOfLoyalty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfLoyalty :: LocationCard HallOfLoyalty
hallOfLoyalty = location HallOfLoyalty Cards.hallOfLoyalty 4 (PerPlayer 1)

instance HasAbilities HallOfLoyalty where
  getAbilities (HallOfLoyalty a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> oneOf
              [ exists (HealableInvestigator (a.ability 1) #damage You)
              , can.draw.cards You
              , can.gain.resources You
              , HasRemainingBlessTokens
              ]
        )
      $ actionAbilityWithCost
      $ OrCost
      $ map SpendKeyCost [BlueKey, RedKey, WhiteKey, YellowKey]

instance RunMessage HallOfLoyalty where
  runMessage msg l@(HallOfLoyalty attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure l
    _ -> HallOfLoyalty <$> liftRunMessage msg attrs
