module Arkham.Types.Location.Cards.ExhibitHallHallOfTheDead
  ( exhibitHallHallOfTheDead
  , ExhibitHallHallOfTheDead(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallHallOfTheDead)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ExhibitHallHallOfTheDead = ExhibitHallHallOfTheDead LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallHallOfTheDead :: LocationCard ExhibitHallHallOfTheDead
exhibitHallHallOfTheDead = locationWithRevealedSideConnections
  ExhibitHallHallOfTheDead
  Cards.exhibitHallHallOfTheDead
  3
  (PerPlayer 2)
  NoSymbol
  [Square]
  Squiggle
  [Square, Hourglass]

instance HasModifiersFor env ExhibitHallHallOfTheDead

instance ActionRunner env => HasActions env ExhibitHallHallOfTheDead where
  getActions iid window (ExhibitHallHallOfTheDead attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallHallOfTheDead where
  runMessage msg l@(ExhibitHallHallOfTheDead attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l
      <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1)
    _ -> ExhibitHallHallOfTheDead <$> runMessage msg attrs
