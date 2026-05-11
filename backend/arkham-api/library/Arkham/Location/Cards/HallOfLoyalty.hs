module Arkham.Location.Cards.HallOfLoyalty (hallOfLoyalty, HallOfLoyalty (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator
import Arkham.I18n
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLairOfDagon.Helpers

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
      chooseNM iid 3 $ scenarioI18n $ scope "hallOfLoyalty" do
        whenM (canHaveHorrorHealed (attrs.ability 1) iid) do
          countVar 1 $ labeledI "healDamage" $ healDamage iid (attrs.ability 1) 1
        whenM (can.draw.cards iid) do
          countVar 2 $ labeledI "drawCards" $ drawCardsIfCan iid (attrs.ability 1) 2
        whenM (can.gain.resources iid) do
          countVar 3 $ labeledI "gainResources" $ gainResourcesIfCan iid (attrs.ability 1) 3
        n <- min 4 <$> getRemainingBlessTokens
        when (n > 0) do
          let key = if n == 4 then "addBlessTokens" else "addBlessTokensPartial"
          numberVar "actual" n $ labeled' key $ repeated n $ addChaosToken #bless
      pure l
    _ -> HallOfLoyalty <$> liftRunMessage msg attrs
