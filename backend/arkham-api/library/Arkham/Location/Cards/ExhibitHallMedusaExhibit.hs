module Arkham.Location.Cards.ExhibitHallMedusaExhibit (exhibitHallMedusaExhibit) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallMedusaExhibit)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: LocationCard ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit = location ExhibitHallMedusaExhibit Cards.exhibitHallMedusaExhibit 2 (PerPlayer 1)

instance HasAbilities ExhibitHallMedusaExhibit where
  getAbilities (ExhibitHallMedusaExhibit x) =
    extendRevealed1 x
      $ mkAbility x 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be x) #failure

instance RunMessage ExhibitHallMedusaExhibit where
  runMessage msg l@(ExhibitHallMedusaExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardAsset iid (attrs.ability 1)
      pure l
    _ -> ExhibitHallMedusaExhibit <$> liftRunMessage msg attrs
