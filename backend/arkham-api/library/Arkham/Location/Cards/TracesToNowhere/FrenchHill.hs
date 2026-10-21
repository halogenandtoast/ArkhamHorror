module Arkham.Location.Cards.TracesToNowhere.FrenchHill (frenchHill) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrenchHill = FrenchHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill :: LocationCard FrenchHill
frenchHill = location FrenchHill Cards.frenchHill 4 (PerPlayer 1)

instance HasAbilities FrenchHill where
  getAbilities (FrenchHill a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ investigatorAt a)
      $ forced
      $ PhaseEnds #when #investigation

instance RunMessage FrenchHill where
  runMessage msg l@(FrenchHill attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt attrs
      for_ investigators \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed $ attrs.clues + 1)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      randomDiscard iid (attrs.ability 1)
      pure l
    _ -> FrenchHill <$> liftRunMessage msg attrs
