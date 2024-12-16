module Arkham.Location.Cards.ForbiddingShore (forbiddingShore) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype ForbiddingShore = ForbiddingShore LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddingShore :: LocationCard ForbiddingShore
forbiddingShore = location ForbiddingShore Cards.forbiddingShore 3 (PerPlayer 1)

instance HasAbilities ForbiddingShore where
  getAbilities (ForbiddingShore attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility [Action.Circle] $ ActionCost 1
      , haunted "You must either lose 1 action or lose 2 resources" attrs 2
      ]

instance RunMessage ForbiddingShore where
  runMessage msg l@(ForbiddingShore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType -> do
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasResources <- fieldMap InvestigatorResources (> 0) iid
      chooseOrRunOneM iid do
        labeled "Lose 1 action" $ loseActions iid (attrs.ability 2) 1
        when hasResources do
          labeled "Lose 2 resources" $ loseResources iid (attrs.ability 2) 2
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> ForbiddingShore <$> liftRunMessage msg attrs
