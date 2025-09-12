module Arkham.Event.Events.FlurryOfBlows5 (flurryOfBlows5) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype FlurryOfBlows5 = FlurryOfBlows5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flurryOfBlows5 :: EventCard FlurryOfBlows5
flurryOfBlows5 = event FlurryOfBlows5 Cards.flurryOfBlows5

instance RunMessage FlurryOfBlows5 where
  runMessage msg e@(FlurryOfBlows5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      doStep 3 msg
      whenM (iid <=~> TurnInvestigator) $ endYourTurn iid
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      selectOneToHandle iid attrs
        $ assetControlledBy iid
        <> #melee
        <> AssetWithPerformableAbility #fight [IgnoreActionCost]
      when (n - 1 > 0) do
        chooseOneM iid do
          labeled "Repeat this effect (Flurry of Blows)" $ doStep (n - 1) msg'
          labeled "Done" nothing
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      abilities <-
        map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
          <$> select (PerformableAbility [IgnoreActionCost] <> #fight <> AbilityOnAsset (AssetWithId aid))
      chooseOrRunOneM iid $ for_ abilities \ab -> abilityLabeled iid ab nothing
      pure e
    _ -> FlurryOfBlows5 <$> liftRunMessage msg attrs
