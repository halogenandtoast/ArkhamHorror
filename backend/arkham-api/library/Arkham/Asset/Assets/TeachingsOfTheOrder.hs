module Arkham.Asset.Assets.TeachingsOfTheOrder (
  teachingsOfTheOrder,
  TeachingsOfTheOrder (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Log
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Sanctum))

newtype TeachingsOfTheOrder = TeachingsOfTheOrder AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teachingsOfTheOrder :: AssetCard TeachingsOfTheOrder
teachingsOfTheOrder = asset TeachingsOfTheOrder Cards.teachingsOfTheOrder

instance HasAbilities TeachingsOfTheOrder where
  getAbilities (TeachingsOfTheOrder a) = [restricted a 1 (ControlsThis <> criteria) $ FastAbility (exhaust a)]
   where
    criteria =
      oneOf
        [ NotYetRecorded Teachings1 <> ChaosTokenCountIs #curse (atLeast 1)
        , NotYetRecorded Teachings2 <> exists (not_ (withTrait Sanctum) <> FloodedLocation)
        , NotYetRecorded Teachings3
            <> exists (EnemyAt YourLocation <> NonEliteEnemy <> EnemyCanBeDefeatedBy (a.ability 1))
        ]

instance RunMessage TeachingsOfTheOrder where
  runMessage msg a@(TeachingsOfTheOrder attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        unlessHasRecord Teachings1 do
          whenAny (ChaosTokenFaceIs #curse) do
            labeled "Remove all {curse} tokens from the chaos bag." $ doStep 1 msg

        unlessHasRecord Teachings2 do
          whenAny (not_ (withTrait Sanctum) <> FloodedLocation) do
            labeled "Remove a flood token from a non-_Sanctum_ location." $ doStep 2 msg

        unlessHasRecord Teachings3 do
          whenAny (enemyAtLocationWith iid <> NonEliteEnemy <> EnemyCanBeDefeatedBy (attrs.ability 1)) do
            labeled "Defeat a non-_Elite_ enemy at your location." $ doStep 3 msg

      pure a
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      record Teachings1
      removeAllChaosTokens #curse
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      record Teachings2
      locations <- select $ not_ (withTrait Sanctum) <> FloodedLocation
      chooseTargetM iid locations \lid -> push $ SetFloodLevel lid Unflooded
      pure a
    DoStep 3 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      record Teachings3
      enemies <-
        select $ enemyAtLocationWith iid <> NonEliteEnemy <> EnemyCanBeDefeatedBy (attrs.ability 1)
      chooseTargetM iid enemies \eid -> defeatEnemy eid iid (attrs.ability 1)
      pure a
    _ -> TeachingsOfTheOrder <$> liftRunMessage msg attrs
