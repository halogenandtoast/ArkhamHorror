module Arkham.Types.Location.Cards.Courtyard
  ( courtyard
  , Courtyard(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype Courtyard = Courtyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtyard :: LocationCard Courtyard
courtyard = location
  Courtyard
  Cards.courtyard
  5
  (Static 0)
  Circle
  [Squiggle, Square, T, Equals, Plus]

instance HasAbilities Courtyard where
  getAbilities (Courtyard attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ForcedAbility $ Enters
        Timing.After
        You
        ThisLocation
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Courtyard where
  runMessage msg l@(Courtyard attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DiscardTopOfEncounterDeck iid 1 (Just $ toTarget attrs))
    DiscardedTopOfEncounterDeck iid [card] target | isTarget attrs target -> do
      l <$ when
        (toCardType card == EnemyType)
        (pushAll
          [ RemoveFromEncounterDiscard card
          , InvestigatorDrewEncounterCard iid card
          ]
        )
    _ -> Courtyard <$> runMessage msg attrs
