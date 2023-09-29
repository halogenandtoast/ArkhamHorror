module Arkham.Act.Cards.EscapeTheCage (
  EscapeTheCage (..),
  escapeTheCage,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Ability
import Arkham.Helpers.Card
import Arkham.Helpers.Query
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (SilverTwilight))

newtype EscapeTheCage = EscapeTheCage ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheCage :: ActCard EscapeTheCage
escapeTheCage = act (3, A) EscapeTheCage Cards.escapeTheCage Nothing

instance HasAbilities EscapeTheCage where
  getAbilities (EscapeTheCage x) =
    withBaseAbilities x
      $ if onSide A x
        then
          [ mkAbility x 1 $ ForcedAbility $ RoundEnds Timing.When
          , restrictedAbility x 2 AllUndefeatedInvestigatorsResigned
              $ Objective
              $ ForcedAbility AnyWindow
          ]
        else []

instance RunMessage EscapeTheCage where
  runMessage msg a@(EscapeTheCage attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      push $ scenarioResolution 1
      pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      entryHall <- selectJust $ LocationWithTitle "Entry Hall"
      entryHallSilverTwilight <-
        selectList $ enemyAt entryHall <> EnemyWithTrait SilverTwilight
      enemiesToMove <-
        selectList
          $ ReadyEnemy
          <> EnemyWithTrait SilverTwilight
          <> NotEnemy
            (enemyAt entryHall)
      enemyCards <- traverse convertToCard entryHallSilverTwilight
      lead <- getLead
      pushAll
        $ map RemoveEnemy entryHallSilverTwilight
        <> [PlaceUnderneath (LocationTarget entryHall) enemyCards]
        <> [ chooseOneAtATime
            lead
            [ targetLabel
              enemy
              [MoveToward (toTarget enemy) (LocationWithId entryHall)]
            | enemy <- enemiesToMove
            ]
           | notNull enemiesToMove
           ]

      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    _ -> EscapeTheCage <$> runMessage msg attrs
