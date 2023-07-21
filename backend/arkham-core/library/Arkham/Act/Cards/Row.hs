module Arkham.Act.Cards.Row (
  Row (..),
  row,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype Row = Row ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

row :: ActCard Row
row = act (3, A) Row Cards.row Nothing

instance HasAbilities Row where
  getAbilities (Row x)
    | onSide A x =
        [ mkAbility x 1 $
            ForcedAbility $
              WouldDrawEncounterCard
                Timing.When
                You
                AnyPhase
        , restrictedAbility
            x
            2
            ( ResourcesOnLocation (LocationWithTitle "Gondola") (AtLeast $ PerPlayer 4)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage Row where
  runMessage msg a@(Row attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      _ <- popMessageMatching $ \case
        InvestigatorDoDrawEncounterCard iid' -> iid == iid'
        _ -> False
      a
        <$ push
          ( DiscardTopOfEncounterDeck
              iid
              5
              (toSource attrs)
              (Just $ toTarget attrs)
          )
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _
      | aid == toId attrs && onSide B attrs ->
          a <$ push (ScenarioResolution $ Resolution 1)
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      lid <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "Must be at a location")
          iid
      let
        writhingAppendages =
          filter ((== Enemies.writhingAppendage) . toCardDef) cards
      pushAll
        [ SpawnEnemyAtEngagedWith (EncounterCard card) lid iid
        | card <- writhingAppendages
        ]
      pure a
    _ -> Row <$> runMessage msg attrs
