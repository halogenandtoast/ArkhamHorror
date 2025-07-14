module Arkham.Event.Events.HandEyeCoordination1 (handEyeCoordination1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Taboo

newtype HandEyeCoordination1 = HandEyeCoordination1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handEyeCoordination1 :: EventCard HandEyeCoordination1
handEyeCoordination1 = event HandEyeCoordination1 Cards.handEyeCoordination1

instance RunMessage HandEyeCoordination1 where
  runMessage msg e@(HandEyeCoordination1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let
        tabooModify =
          if tabooed TabooList24 attrs
            then (<> AssetCardMatch (mapOneOf CardWithLevel [0 .. 3]))
            else id
      abilities <-
        map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
          <$> select
            ( PerformableAbility [IgnoreActionCost]
                <> AbilityIsActionAbility
                <> AbilityOnAsset (tabooModify $ assetControlledBy iid <> oneOf [#tool, #weapon])
            )
      chooseOrRunOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> HandEyeCoordination1 <$> liftRunMessage msg attrs
