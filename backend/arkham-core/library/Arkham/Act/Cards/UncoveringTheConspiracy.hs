module Arkham.Act.Cards.UncoveringTheConspiracy (
  UncoveringTheConspiracy (..),
  uncoveringTheConspiracy,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Trait

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

uncoveringTheConspiracy :: ActCard UncoveringTheConspiracy
uncoveringTheConspiracy = act (1, A) UncoveringTheConspiracy Cards.uncoveringTheConspiracy Nothing

instance HasAbilities UncoveringTheConspiracy where
  getAbilities (UncoveringTheConspiracy a) | onSide A a = do
    [ restrictedAbility a 1 (ScenarioDeckWithCard CultistDeck)
        $ ActionAbility []
        $ ActionCost 1
        <> GroupClueCost (PerPlayer 2) Anywhere
      , mkAbility a 2 (Objective $ ForcedAbility AnyWindow)
          `withCriteria` InVictoryDisplay
            (CardWithTrait Cultist <> CardIsUnique)
            (EqualTo $ Static 6)
      ]
  getAbilities _ = []

instance RunMessage UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ DrawFromScenarioDeck iid CultistDeck (toTarget attrs) 1
      pure a
    DrewFromScenarioDeck iid CultistDeck (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      pushAll $ InvestigatorDrewEncounterCard iid <$> cards
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (InvestigatorSource iid) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs
