module Arkham.Location.Cards.BasementHall
  ( basementHall
  , BasementHall(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype BasementHall = BasementHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basementHall :: LocationCard BasementHall
basementHall = location BasementHall Cards.basementHall 4 (PerPlayer 1)

instance HasModifiersFor BasementHall where
  getModifiersFor (LocationTarget lid) (BasementHall attrs)
    | lid == toId attrs = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

instance HasAbilities BasementHall where
  getAbilities (BasementHall attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.When Anyone
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage BasementHall where
  runMessage msg l@(BasementHall attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      patientConfinements <- shuffleM
        =<< getSetAsideCardsMatching (CardWithTitle "Patient Confinement")

      l <$ pushAll
        (map PlaceLocation patientConfinements
        <> [ SetLocationLabel (toLocationId patientConfinement)
             $ "patientConfinement"
             <> tshow idx
           | (idx, patientConfinement) <- zip
             [1 :: Int ..]
             patientConfinements
           ]
        )
    _ -> BasementHall <$> runMessage msg attrs
