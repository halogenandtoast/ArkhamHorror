module Arkham.Location.Cards.ExhibitHallNatureExhibit (exhibitHallNatureExhibit) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards (exhibitHallNatureExhibit)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallNatureExhibit = ExhibitHallNatureExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallNatureExhibit :: LocationCard ExhibitHallNatureExhibit
exhibitHallNatureExhibit = location ExhibitHallNatureExhibit Cards.exhibitHallNatureExhibit 4 (PerPlayer 1)

instance HasAbilities ExhibitHallNatureExhibit where
  getAbilities (ExhibitHallNatureExhibit x) =
    extendRevealed1 x $ mkAbility x 1 $ forced $ Enters #after You (be x)

instance RunMessage ExhibitHallNatureExhibit where
  runMessage msg l@(ExhibitHallNatureExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscardN iid (attrs.ability 1) 2
      pure l
    _ -> ExhibitHallNatureExhibit <$> liftRunMessage msg attrs
