module Arkham.Act.Cards.EscapeTheRuins
  ( EscapeTheRuins(..)
  , escapeTheRuins
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Types ( Field (EnemyTraits) )
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Types
import Arkham.Target
import Arkham.Trait

newtype EscapeTheRuins = EscapeTheRuins ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheRuins :: ActCard EscapeTheRuins
escapeTheRuins = act (3, A) EscapeTheRuins Cards.escapeTheRuins Nothing

instance HasModifiersFor EscapeTheRuins where
  getModifiersFor (EnemyTarget eid) (EscapeTheRuins a) = do
    n <- getVengeanceInVictoryDisplay
    isSerpent <- fieldP EnemyTraits (elem Serpent) eid
    pure $ toModifiers a [ EnemyEvade 1 | n >= 3 && isSerpent ]
  getModifiersFor _ _ = pure []

instance HasAbilities EscapeTheRuins where
  getAbilities (EscapeTheRuins x) = withBaseAbilities
    x
    [ restrictedAbility x 1 AllUndefeatedInvestigatorsResigned
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide A x
    ]

instance RunMessage EscapeTheRuins where
  runMessage msg a@(EscapeTheRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      resignedWithRelicOfAges <- scenarioFieldMap
        ScenarioResignedCardCodes
        (elem "04061")
      let resolution = if resignedWithRelicOfAges then 1 else 3
      push $ ScenarioResolution $ Resolution resolution
      pure a
    _ -> EscapeTheRuins <$> runMessage msg attrs
