module Arkham.Event.Cards.HandEyeCoordination1 (
  handEyeCoordination1,
  HandEyeCoordination1 (..),
)
where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype HandEyeCoordination1 = HandEyeCoordination1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handEyeCoordination1 :: EventCard HandEyeCoordination1
handEyeCoordination1 = event HandEyeCoordination1 Cards.handEyeCoordination1

instance RunMessage HandEyeCoordination1 where
  runMessage msg e@(HandEyeCoordination1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      abilities <-
        map (`applyAbilityModifiers` [IgnoreActionCost])
          <$> select
            ( PerformableAbility [IgnoreActionCost]
                <> AbilityIsActionAbility
                <> AbilityOnAsset (assetControlledBy iid <> oneOf [#tool, #weapon])
            )
      chooseOrRunOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> HandEyeCoordination1 <$> liftRunMessage msg attrs
