module Arkham.Location.Cards.Courtyard (
  courtyard,
  Courtyard (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Courtyard = Courtyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

courtyard :: LocationCard Courtyard
courtyard = location Courtyard Cards.courtyard 5 (Static 0)

instance HasAbilities Courtyard where
  getAbilities (Courtyard attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 Here
        $ ForcedAbility
        $ Enters
          Timing.After
          You
          ThisLocation
      | locationRevealed attrs
      ]

instance RunMessage Courtyard where
  runMessage msg l@(Courtyard attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ DiscardTopOfEncounterDeck
          iid
          1
          (toAbilitySource attrs 1)
          (Just $ toTarget attrs)
      pure l
    DiscardedTopOfEncounterDeck iid [card] _ (isTarget attrs -> True) -> do
      when (toCardType card == EnemyType) $ do
        push $ InvestigatorDrewEncounterCard iid card
      pure l
    _ -> Courtyard <$> runMessage msg attrs
