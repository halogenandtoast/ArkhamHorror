module Arkham.Act.Cards.InPursuitOfTheLiving (
  InPursuitOfTheLiving (..),
  inPursuitOfTheLiving,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Classes
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (Spectral))

newtype InPursuitOfTheLiving = InPursuitOfTheLiving ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inPursuitOfTheLiving :: ActCard InPursuitOfTheLiving
inPursuitOfTheLiving = act (2, A) InPursuitOfTheLiving Cards.inPursuitOfTheLiving Nothing

instance HasModifiersFor InPursuitOfTheLiving where
  getModifiersFor (InvestigatorTarget iid) (InPursuitOfTheLiving a) =
    pure $ toModifiers a [CannotDiscoverCluesAt $ NotLocation $ LocationWithTrait Spectral]
  getModifiersFor _ _ = pure []

instance HasAbilities InPursuitOfTheLiving where
  getAbilities (InPursuitOfTheLiving a)
    | onSide A a =
        [ restrictedAbility
            a
            2
            (ExtendedCardCount 4 $ VictoryDisplayCardMatch $ CardWithTitle "Unfinished Business")
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage InPursuitOfTheLiving where
  runMessage msg a@(InPursuitOfTheLiving attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide A attrs -> do
      pushAll [recordSetInsert MementosDiscovered [CornHuskDoll], scenarioResolution 1]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    _ -> InPursuitOfTheLiving <$> runMessage msg attrs
