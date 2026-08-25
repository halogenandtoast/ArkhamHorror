module Arkham.Homebrew.CircusExMortis.Locations.CrowdedRow_050 (crowdedRow_050) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.SkillTest (discoverAdditionalClues)
import Arkham.Modifier

newtype CrowdedRow_050 = CrowdedRow_050 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crowdedRow_050 :: LocationCard CrowdedRow_050
crowdedRow_050 = location CrowdedRow_050 Cards.crowdedRow_050 2 (Static 2)

instance HasAbilities CrowdedRow_050 where
  getAbilities (CrowdedRow_050 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ freeReaction (InitiatedSkillTest #when You #any #any $ WhileInvestigating (be a))

instance RunMessage CrowdedRow_050 where
  runMessage msg l@(CrowdedRow_050 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier 3)
      discoverAdditionalClues (attrs.ability 1) iid 1
      pure l
    _ -> CrowdedRow_050 <$> liftRunMessage msg attrs
