module Arkham.Act.Cards.Repossession (
  Repossession (..),
  repossession,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Draw.Types
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Repossession = Repossession ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

repossession :: ActCard Repossession
repossession = act (3, A) Repossession Cards.repossession Nothing

instance HasAbilities Repossession where
  getAbilities (Repossession a)
    | onSide A a =
        [ mkAbility a 1 $ ActionAbility [Action.Draw] $ ClueCost (Static 1) <> ActionCost 1
        , restrictedAbility a 2 (EachUndefeatedInvestigator $ HandWith $ LengthIs $ AtLeast $ Static 10)
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage Repossession where
  runMessage msg a@(Repossession attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      handSize <- field InvestigatorHandSize iid
      numberOfCardsInHand <- fieldMap InvestigatorHand length iid
      let drawCount = if numberOfCardsInHand > handSize then 3 else 2
      drawing <- newCardDraw iid (toSource attrs) drawCount
      push $ DrawCards drawing
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (InvestigatorSource iid) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ scenarioResolution 1
      pure a
    _ -> Repossession <$> runMessage msg attrs
