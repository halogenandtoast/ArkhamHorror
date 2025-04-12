module Arkham.Location.Cards.ExhibitHallMedievalExhibit (exhibitHallMedievalExhibit) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallMedievalExhibit = ExhibitHallMedievalExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedievalExhibit :: LocationCard ExhibitHallMedievalExhibit
exhibitHallMedievalExhibit = location ExhibitHallMedievalExhibit Cards.exhibitHallMedievalExhibit 3 (PerPlayer 1)

instance HasAbilities ExhibitHallMedievalExhibit where
  getAbilities (ExhibitHallMedievalExhibit a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage ExhibitHallMedievalExhibit where
  runMessage msg l@(ExhibitHallMedievalExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> ExhibitHallMedievalExhibit <$> liftRunMessage msg attrs
