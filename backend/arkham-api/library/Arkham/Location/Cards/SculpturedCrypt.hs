module Arkham.Location.Cards.SculpturedCrypt (sculpturedCrypt) where

import Arkham.Helpers.Cost
import Arkham.Helpers.GameValue
import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SculpturedCrypt = SculpturedCrypt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sculpturedCrypt :: LocationCard SculpturedCrypt
sculpturedCrypt = location SculpturedCrypt Cards.sculpturedCrypt 4 (Static 0)

instance HasAbilities SculpturedCrypt where
  getAbilities (SculpturedCrypt a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage SculpturedCrypt where
  runMessage msg l@(SculpturedCrypt attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeSeal attrs (Seal SealE False Nothing)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 2)
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
    _ -> SculpturedCrypt <$> liftRunMessage msg attrs
