module Arkham.Location.Cards.ExhibitHallEgyptianExhibit (exhibitHallEgyptianExhibit) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallEgyptianExhibit)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallEgyptianExhibit = ExhibitHallEgyptianExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallEgyptianExhibit :: LocationCard ExhibitHallEgyptianExhibit
exhibitHallEgyptianExhibit =
  location ExhibitHallEgyptianExhibit Cards.exhibitHallEgyptianExhibit 3 (PerPlayer 2)

instance HasAbilities ExhibitHallEgyptianExhibit where
  getAbilities (ExhibitHallEgyptianExhibit x) =
    extendRevealed1 x
      $ mkAbility x 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be x) #failure

instance RunMessage ExhibitHallEgyptianExhibit where
  runMessage msg l@(ExhibitHallEgyptianExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    _ -> ExhibitHallEgyptianExhibit <$> liftRunMessage msg attrs
