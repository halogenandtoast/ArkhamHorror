module Arkham.Location.Cards.ExhibitHallAthabaskanExhibit (
  exhibitHallAthabaskanExhibit,
  ExhibitHallAthabaskanExhibit (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallAthabaskanExhibit)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype ExhibitHallAthabaskanExhibit = ExhibitHallAthabaskanExhibit LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

exhibitHallAthabaskanExhibit :: LocationCard ExhibitHallAthabaskanExhibit
exhibitHallAthabaskanExhibit =
  location ExhibitHallAthabaskanExhibit Cards.exhibitHallAthabaskanExhibit 1 (Static 0)

instance HasModifiersFor ExhibitHallAthabaskanExhibit where
  getModifiersFor (InvestigatorTarget iid) (ExhibitHallAthabaskanExhibit attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [SkillModifier #agility 2 | here]
  getModifiersFor _ _ = pure []

instance HasAbilities ExhibitHallAthabaskanExhibit where
  getAbilities (ExhibitHallAthabaskanExhibit x) =
    withRevealedAbilities x
      $ [mkAbility x 1 $ ForcedAbility $ Enters #after You $ LocationWithId $ toId x]

instance RunMessage ExhibitHallAthabaskanExhibit where
  runMessage msg l@(ExhibitHallAthabaskanExhibit attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SetActions iid (toAbilitySource attrs 1) 0, ChooseEndTurn iid]
      pure l
    _ -> ExhibitHallAthabaskanExhibit <$> runMessage msg attrs
