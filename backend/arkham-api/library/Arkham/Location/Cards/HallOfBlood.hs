module Arkham.Location.Cards.HallOfBlood (hallOfBlood, HallOfBlood (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfBlood = HallOfBlood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfBlood :: LocationCard HallOfBlood
hallOfBlood = location HallOfBlood Cards.hallOfBlood 3 (PerPlayer 1)

instance HasAbilities HallOfBlood where
  getAbilities (HallOfBlood a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 Here
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      ]

instance RunMessage HallOfBlood where
  runMessage msg l@(HallOfBlood attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs RedKey
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> HallOfBlood <$> liftRunMessage msg attrs
