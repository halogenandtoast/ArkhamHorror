module Arkham.Location.Cards.ForbiddingShore (
  forbiddingShore,
  ForbiddingShore (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype ForbiddingShore = ForbiddingShore LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddingShore :: LocationCard ForbiddingShore
forbiddingShore = location ForbiddingShore Cards.forbiddingShore 3 (PerPlayer 1)

instance HasAbilities ForbiddingShore where
  getAbilities (ForbiddingShore attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility (Just Action.Circle) $ ActionCost 1
      , haunted "You must either lose 1 action or lose 2 resources" attrs 2
      ]

instance RunMessage ForbiddingShore where
  runMessage msg l@(ForbiddingShore attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $
        chooseOne
          iid
          [ SkillLabel skillType [beginSkillTest iid attrs attrs skillType 3]
          | skillType <- [#willpower, #intellect]
          ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      hasResources <- fieldMap InvestigatorResources (> 0) iid
      push $
        chooseOrRunOne iid $
          Label "Lose 1 action" [LoseActions iid (toSource attrs) 1]
            : [Label "Lose 2 resources" [LoseResources iid (toSource attrs) 2] | hasResources]
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> ForbiddingShore <$> runMessage msg attrs
