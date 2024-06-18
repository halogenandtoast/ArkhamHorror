module Arkham.Act.Cards.Row (Row (..), row) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype Row = Row ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

row :: ActCard Row
row = act (3, A) Row Cards.row Nothing

instance HasAbilities Row where
  getAbilities (Row x)
    | onSide A x =
        [ mkAbility x 1 $ forced $ WouldDrawEncounterCard #when You AnyPhase
        , restrictedAbility
            x
            2
            (ResourcesOnLocation "Gondola" (AtLeast $ PerPlayer 4))
            $ Objective
            $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage Row where
  runMessage msg a@(Row attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ Instead (DoDrawCards iid)
        $ DiscardTopOfEncounterDeck iid 5 (toSource attrs) (Just $ toTarget attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "Must be at a location") iid
      let writhingAppendages = filter ((== Enemies.writhingAppendage) . toCardDef) cards
      pushAll
        [ SpawnEnemyAtEngagedWith (EncounterCard card) lid iid
        | card <- writhingAppendages
        ]
      pure a
    _ -> Row <$> runMessage msg attrs
