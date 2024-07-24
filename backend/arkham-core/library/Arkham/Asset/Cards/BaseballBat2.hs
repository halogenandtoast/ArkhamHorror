module Arkham.Asset.Cards.BaseballBat2 (BaseballBat2 (..), baseballBat2, baseballBat2Effect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Effect.Types qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype BaseballBat2 = BaseballBat2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat2 :: AssetCard BaseballBat2
baseballBat2 = asset BaseballBat2 Cards.baseballBat2

instance HasAbilities BaseballBat2 where
  getAbilities (BaseballBat2 a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat2 where
  runMessage msg a@(BaseballBat2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      createCardEffect Cards.baseballBat2 (effectMetaTarget sid) source iid
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> BaseballBat2 <$> liftRunMessage msg attrs

newtype BaseballBat2Effect = BaseballBat2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat2Effect :: EffectArgs -> BaseballBat2Effect
baseballBat2Effect = cardEffect BaseballBat2Effect Cards.baseballBat2

instance RunMessage BaseballBat2Effect where
  runMessage msg e@(BaseballBat2Effect attrs) = runQueueT case msg of
    RevealChaosToken _ iid token | isTarget iid attrs.target && token.face `elem` [Skull, AutoFail] -> do
      withSkillTest \sid ->
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          case attrs.source.asset of
            Just assetId ->
              chooseOne
                iid
                [ Label "Return baseball Bat to your hand after this attack" [ReturnToHand iid (toTarget assetId)]
                , Label
                    "This attack deals an additional +1 damage. Discard Baseball Bat after this attack"
                    [ Msg.skillTestModifier sid attrs.source iid (DamageDealt 1)
                    , Msg.toDiscardBy iid attrs.source assetId
                    , Msg.disable attrs
                    ]
                ]
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> BaseballBat2Effect <$> liftRunMessage msg attrs
