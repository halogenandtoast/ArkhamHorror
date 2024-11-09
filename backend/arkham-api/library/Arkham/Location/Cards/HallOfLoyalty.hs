module Arkham.Location.Cards.HallOfLoyalty (hallOfLoyalty, HallOfLoyalty (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseNM iid 3 do
        whenM (canHaveHorrorHealed (attrs.ability 1) iid) do
          labeled "Heal 1 damage" $ healDamage iid (attrs.ability 1) 1
        whenM (can.draw.cards iid) do
          labeled "Draw 2 cards" $ drawCardsIfCan iid (attrs.ability 1) 2
        whenM (can.gain.resources iid) do
          labeled "Gain 3 resources" $ gainResourcesIfCan iid (attrs.ability 1) 3
        n <- min 4 <$> getRemainingBlessTokens
        when (n > 0) do
          let label = if n == 4 then "Add 4 {bless} tokens" else "Add 4 (actual " <> tshow n <> ") {bless} token"
          labeled label $ repeated n $ addChaosToken #bless
      pure l
    _ -> HallOfLoyalty <$> liftRunMessage msg attrs
