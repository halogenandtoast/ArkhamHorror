module Arkham.Act.Cards.UncoveringTheConspiracy (
  UncoveringTheConspiracy (..),
  uncoveringTheConspiracy,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Draw.Types
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Trait

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncoveringTheConspiracy :: ActCard UncoveringTheConspiracy
uncoveringTheConspiracy = act (1, A) UncoveringTheConspiracy Cards.uncoveringTheConspiracy Nothing

instance HasAbilities UncoveringTheConspiracy where
  getAbilities (UncoveringTheConspiracy a) | onSide A a = do
    [ restrictedAbility a 1 (ScenarioDeckWithCard CultistDeck)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
      , mkAbility a 2 (Objective $ forced AnyWindow)
          `withCriteria` InVictoryDisplay
            (CardWithTrait Cultist <> CardIsUnique)
            (EqualTo $ Static 6)
      ]
  getAbilities _ = []

instance RunMessage UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawCards iid $ newCardDraw (attrs.ability 1) CultistDeck 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ AdvanceAct (toId attrs) (InvestigatorSource iid) AdvancedWithOther
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs
