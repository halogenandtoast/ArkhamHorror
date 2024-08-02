module Arkham.Event.Cards.FlurryOfBlows5 (flurryOfBlows5, FlurryOfBlows5 (..)) where

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
    DoStep n (PlayThisEvent iid (is attrs -> True)) -> do
      selectOneToHandle iid attrs
        $ assetControlledBy iid
        <> #melee
        <> AssetWithPerformableAbility (AbilityIsAction #fight) [IgnoreActionCost]
      when (n - 1 > 0) do
        chooseOne iid [Label "Repeat this effect (Flurry of Blows)" [DoStep (n - 1) msg], Label "Done" []]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      abilities <-
        map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
          <$> select
            ( PerformableAbility [IgnoreActionCost]
                <> AbilityIsAction #fight
                <> AbilityOnAsset (AssetWithId aid)
            )
      chooseOrRunOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> FlurryOfBlows5 <$> liftRunMessage msg attrs
