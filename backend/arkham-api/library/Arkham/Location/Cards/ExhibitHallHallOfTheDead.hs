module Arkham.Location.Cards.ExhibitHallHallOfTheDead (exhibitHallHallOfTheDead) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallHallOfTheDead)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallHallOfTheDead = ExhibitHallHallOfTheDead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallHallOfTheDead :: LocationCard ExhibitHallHallOfTheDead
exhibitHallHallOfTheDead = location ExhibitHallHallOfTheDead Cards.exhibitHallHallOfTheDead 3 (PerPlayer 2)

instance HasAbilities ExhibitHallHallOfTheDead where
  getAbilities (ExhibitHallHallOfTheDead x) =
    extendRevealed1 x
      $ mkAbility x 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be x) #failure

instance RunMessage ExhibitHallHallOfTheDead where
  runMessage msg l@(ExhibitHallHallOfTheDead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> ExhibitHallHallOfTheDead <$> liftRunMessage msg attrs
