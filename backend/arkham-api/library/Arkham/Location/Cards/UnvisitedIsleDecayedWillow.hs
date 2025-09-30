module Arkham.Location.Cards.UnvisitedIsleDecayedWillow (unvisitedIsleDecayedWillow) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleDecayedWillow = UnvisitedIsleDecayedWillow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDecayedWillow :: LocationCard UnvisitedIsleDecayedWillow
unvisitedIsleDecayedWillow = location UnvisitedIsleDecayedWillow Cards.unvisitedIsleDecayedWillow 4 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleDecayedWillow where
  getModifiersFor (UnvisitedIsleDecayedWillow a) = whenUnrevealed a $ maybeModifySelf a do
    sidedWithLodge <- lift $ getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- lift $ selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleDecayedWillow where
  getAbilities (UnvisitedIsleDecayedWillow a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here $ ActionAbility [Action.Circle] $ ActionCost 1
      , scenarioI18n $ hauntedI "unvisitedIsleDecayedWillow.haunted" a 2
      ]

instance RunMessage UnvisitedIsleDecayedWillow where
  runMessage msg l@(UnvisitedIsleDecayedWillow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#intellect, #combat] (Fixed 9)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseAndDiscardCard iid (attrs.ability 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleDecayedWillow <$> liftRunMessage msg attrs
