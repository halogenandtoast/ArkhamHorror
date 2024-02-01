module Arkham.Location.Cards.ExhibitHallNatureExhibit (
  exhibitHallNatureExhibit,
  ExhibitHallNatureExhibit (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (exhibitHallNatureExhibit)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ExhibitHallNatureExhibit = ExhibitHallNatureExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

exhibitHallNatureExhibit :: LocationCard ExhibitHallNatureExhibit
exhibitHallNatureExhibit =
  location
    ExhibitHallNatureExhibit
    Cards.exhibitHallNatureExhibit
    4
    (PerPlayer 1)

instance HasAbilities ExhibitHallNatureExhibit where
  getAbilities (ExhibitHallNatureExhibit x) =
    withBaseAbilities
      x
      [ mkAbility x 1
        $ ForcedAbility
        $ Enters Timing.After You
        $ LocationWithId
        $ toId x
      | locationRevealed x
      ]

instance RunMessage ExhibitHallNatureExhibit where
  runMessage msg l@(ExhibitHallNatureExhibit attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ toMessage $ randomDiscard iid (toAbilitySource attrs 1)
        , toMessage $ randomDiscard iid (toAbilitySource attrs 1)
        ]
      pure l
    _ -> ExhibitHallNatureExhibit <$> runMessage msg attrs
