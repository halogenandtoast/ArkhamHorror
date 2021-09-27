module Arkham.Types.Location.Cards.ExhibitHallAthabaskanExhibit
  ( exhibitHallAthabaskanExhibit
  , ExhibitHallAthabaskanExhibit(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (exhibitHallAthabaskanExhibit)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype ExhibitHallAthabaskanExhibit = ExhibitHallAthabaskanExhibit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallAthabaskanExhibit :: LocationCard ExhibitHallAthabaskanExhibit
exhibitHallAthabaskanExhibit = locationWithRevealedSideConnections
  ExhibitHallAthabaskanExhibit
  Cards.exhibitHallAthabaskanExhibit
  1
  (Static 0)
  NoSymbol
  [Square]
  Plus
  [Square]

instance HasModifiersFor env ExhibitHallAthabaskanExhibit where
  getModifiersFor _ (InvestigatorTarget iid) (ExhibitHallAthabaskanExhibit attrs)
    = pure $ toModifiers attrs [ SkillModifier SkillAgility 2 | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ExhibitHallAthabaskanExhibit where
  getAbilities (ExhibitHallAthabaskanExhibit x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env ExhibitHallAthabaskanExhibit where
  runMessage msg l@(ExhibitHallAthabaskanExhibit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [SetActions iid source 0, ChooseEndTurn iid]
    _ -> ExhibitHallAthabaskanExhibit <$> runMessage msg attrs
