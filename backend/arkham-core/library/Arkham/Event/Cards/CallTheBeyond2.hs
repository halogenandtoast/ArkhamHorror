module Arkham.Event.Cards.CallTheBeyond2 (callTheBeyond2, CallTheBeyond2 (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Use (toStartingUses)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token

newtype CallTheBeyond2 = CallTheBeyond2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callTheBeyond2 :: EventCard CallTheBeyond2
callTheBeyond2 = event CallTheBeyond2 Cards.callTheBeyond2

instance RunMessage CallTheBeyond2 where
  runMessage msg e@(CallTheBeyond2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ assetControlledBy iid
        <> oneOf [AssetWithUseType Charge, AssetWithUseType Secret]
        <> oneOf
          [ AssetNotAtUsesX
          , AssetWithPerformableAbility
              (oneOf [AbilityIsActionAbility, AbilityIsFastAbility])
              [IgnoreActionCost]
          ]
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (AssetTarget aid) -> do
      void $ runMaybeT do
        (uType, n) <-
          MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
        lift do
          current <- findWithDefault 0 uType <$> field AssetUses aid
          let toAdd = n - current
          when (toAdd > 0) $ placeTokens attrs (toTarget aid) uType toAdd
      doStep 1 msg
      pure e
    DoStep 1 (HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid)) -> do
      abilities <-
        map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
          <$> select
            ( PerformableAbility [IgnoreActionCost]
                <> AbilityIsActionAbility
                <> AbilityOnAsset (AssetWithId aid)
            )
      when (notNull abilities) do
        chooseOne iid $ Label "Do not resolve an ability" []
          : [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> CallTheBeyond2 <$> liftRunMessage msg attrs
