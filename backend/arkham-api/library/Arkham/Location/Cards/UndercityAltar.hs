module Arkham.Location.Cards.UndercityAltar (undercityAltar) where

import Arkham.Helpers.Cost
import Arkham.Helpers.GameValue
import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype UndercityAltar = UndercityAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undercityAltar :: LocationCard UndercityAltar
undercityAltar = location UndercityAltar Cards.undercityAltar 3 (Static 0)

instance HasAbilities UndercityAltar where
  getAbilities (UndercityAltar a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage UndercityAltar where
  runMessage msg l@(UndercityAltar attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeSeal attrs (Seal SealC False Nothing)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 2)
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
    _ -> UndercityAltar <$> liftRunMessage msg attrs
