module Arkham.Types.Location.Cards.ExhibitHallHallOfTheDead
  ( exhibitHallHallOfTheDead
  , ExhibitHallHallOfTheDead(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallHallOfTheDead)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype ExhibitHallHallOfTheDead = ExhibitHallHallOfTheDead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
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

instance HasAbilities env ExhibitHallHallOfTheDead where
  getAbilities i w (ExhibitHallHallOfTheDead x) = withBaseAbilities i w x $ do
    pure
      [ mkAbility x 1
        $ ForcedAbility
        $ SkillTestResult
            Timing.After
            You
            (WhileInvestigating $ LocationWithId $ toId x)
        $ FailureResult AnyValue
      | locationRevealed x
      ]

instance LocationRunner env => RunMessage env ExhibitHallHallOfTheDead where
  runMessage msg l@(ExhibitHallHallOfTheDead attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> ExhibitHallHallOfTheDead <$> runMessage msg attrs
