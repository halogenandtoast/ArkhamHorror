module Arkham.Asset.Cards.TriggerMan3 (triggerMan3, TriggerMan3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card (playIsValidAfterSeal)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection

newtype TriggerMan3 = TriggerMan3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

triggerMan3 :: AssetCard TriggerMan3
triggerMan3 = ally TriggerMan3 Cards.triggerMan3 (2, 1)

instance HasModifiersFor TriggerMan3 where
  getModifiersFor (AssetTarget aid) (TriggerMan3 a) | aid /= toId a = do
    field AssetController a.id >>= \case
      Nothing -> pure []
      Just iid -> do
        placement <- field AssetPlacement aid
        pure case placement of
          AttachedToAsset aid' _ | aid' == toId a -> toModifiers a [AsIfUnderControlOf iid]
          _ -> []
  getModifiersFor _ _ = pure []

instance HasAbilities TriggerMan3 where
  getAbilities (TriggerMan3 x) =
    [ controlledAbility x 1 (exists $ InHandOf You <> basic (#illicit <> #asset))
        $ freeReaction
        $ AssetEntersPlay #when (be x)
    , controlledAbility
        x
        2
        ( exists
            $ PerformableAbility [IgnoreActionCost]
            <> AbilityOnAsset (AssetAttachedToAsset $ be x)
            <> AbilityIsActionAbility
        )
        $ FastAbility (exhaust x <> ResourceCost 1)
    ]

instance RunMessage TriggerMan3 where
  runMessage msg a@(TriggerMan3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      field InvestigatorHand iid
        >>= filterM (playIsValidAfterSeal iid)
        . filterCards (card_ $ #asset <> #illicit)
        >>= chooseOneToHandle iid (attrs.ability 1)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      c <- getCard cid
      assetId <- getRandom
      push $ ObtainCard c
      push $ CreateAssetAt assetId c $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      abilities <-
        selectMap ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
          $ PerformableAbility [IgnoreActionCost]
          <> AbilityOnAsset (AssetAttachedToAsset $ be attrs)
          <> #action
      chooseOrRunOne
        iid
        [ AbilityLabel iid ab [] [Msg.abilityModifier ab.ref (attrs.ability 2) iid (BaseSkill 4)] []
        | ab <- abilities
        ]
      pure a
    _ -> TriggerMan3 <$> liftRunMessage msg attrs
