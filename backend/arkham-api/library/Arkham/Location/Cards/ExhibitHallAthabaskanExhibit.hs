module Arkham.Location.Cards.ExhibitHallAthabaskanExhibit (
  exhibitHallAthabaskanExhibit,
  ExhibitHallAthabaskanExhibit (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallAthabaskanExhibit)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallAthabaskanExhibit = ExhibitHallAthabaskanExhibit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallAthabaskanExhibit :: LocationCard ExhibitHallAthabaskanExhibit
exhibitHallAthabaskanExhibit =
  location ExhibitHallAthabaskanExhibit Cards.exhibitHallAthabaskanExhibit 1 (Static 0)

instance HasModifiersFor ExhibitHallAthabaskanExhibit where
  getModifiersFor (ExhibitHallAthabaskanExhibit a) = do
    modifySelect a (investigatorAt a) [SkillModifier #agility 2]

instance HasAbilities ExhibitHallAthabaskanExhibit where
  getAbilities (ExhibitHallAthabaskanExhibit x) =
    extendRevealed1 x $ mkAbility x 1 $ forced $ Enters #after You (be x)

instance RunMessage ExhibitHallAthabaskanExhibit where
  runMessage msg l@(ExhibitHallAthabaskanExhibit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SetActions iid (attrs.ability 1) 0, ChooseEndTurn iid]
      pure l
    _ -> ExhibitHallAthabaskanExhibit <$> liftRunMessage msg attrs
