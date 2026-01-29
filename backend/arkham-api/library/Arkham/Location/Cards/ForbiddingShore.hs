module Arkham.Location.Cards.ForbiddingShore (forbiddingShore) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.I18n
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
  getAbilities (ForbiddingShore a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here $ ActionAbility [Action.Circle] Nothing $ ActionCost 1
      , scenarioI18n $ hauntedI "forbiddingShore.haunted" a 2
      ]

instance RunMessage ForbiddingShore where
  runMessage msg l@(ForbiddingShore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType -> do
          skillLabeled sType $ circleTest sid iid (attrs.ability 1) iid [sType] (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasResources <- fieldMap InvestigatorResources (> 0) iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "loseActions" $ loseActions iid (attrs.ability 2) 1
        when hasResources do
          countVar 2 $ labeled' "loseResources" $ loseResources iid (attrs.ability 2) 2
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> ForbiddingShore <$> liftRunMessage msg attrs
