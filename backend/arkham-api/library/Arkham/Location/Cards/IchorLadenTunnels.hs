module Arkham.Location.Cards.IchorLadenTunnels (ichorLadenTunnels) where

import Arkham.Helpers.Cost
import Arkham.Helpers.GameValue
import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IchorLadenTunnels = IchorLadenTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichorLadenTunnels :: LocationCard IchorLadenTunnels
ichorLadenTunnels = location IchorLadenTunnels Cards.ichorLadenTunnels 3 (Static 0)

instance HasAbilities IchorLadenTunnels where
  getAbilities (IchorLadenTunnels a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage IchorLadenTunnels where
  runMessage msg l@(IchorLadenTunnels attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ PlaceSeal (toTarget attrs) (Seal SealB False Nothing)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      for_ (nonEmpty $ toList attrs.seals) \(k :| _) -> do
        investigators <- select (investigatorAt attrs)
        n <- getSpendableClueCount investigators
        x <- perPlayer 1

        when (n >= x) do
          chooseOneM iid do
            labeled "Spend 1 {perPlayer} clues as a group to take control of the seal" do
              spendCluesAsAGroup investigators n
              placeSeal iid k
            labeled "Do not spend clues" nothing
      
      pure l
    _ -> IchorLadenTunnels <$> liftRunMessage msg attrs
