module Arkham.Asset.Assets.MistsOfRlyeh4 (mistsOfRlyeh4, MistsOfRlyeh4 (..), mistsOfRlyeh4Effect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Message.Discard (chooseAndDiscardCard)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.Window qualified as Window

newtype MistsOfRlyeh4 = MistsOfRlyeh4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4 :: AssetCard MistsOfRlyeh4
mistsOfRlyeh4 = asset MistsOfRlyeh4 Cards.mistsOfRlyeh4

instance HasAbilities MistsOfRlyeh4 where
  getAbilities (MistsOfRlyeh4 a) = [restricted a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage MistsOfRlyeh4 where
  runMessage msg a@(MistsOfRlyeh4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      createCardEffect Cards.mistsOfRlyeh4 (effectInt 1) source sid
      createCardEffect Cards.mistsOfRlyeh4 (effectInt 2) source sid
      skillTestModifier sid source iid (SkillModifier #willpower 3)
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    _ -> MistsOfRlyeh4 <$> liftRunMessage msg attrs

newtype MistsOfRlyeh4Effect = MistsOfRlyeh4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4Effect :: EffectArgs -> MistsOfRlyeh4Effect
mistsOfRlyeh4Effect = cardEffect MistsOfRlyeh4Effect Cards.mistsOfRlyeh4

instance RunMessage MistsOfRlyeh4Effect where
  runMessage msg e@(MistsOfRlyeh4Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | attrs.metaInt == Just 1 -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              and
                [ token.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
                , iid == st.investigator
                , isTarget st attrs.target
                ]
        when triggers do
          push
            $ If
              (Window.RevealChaosTokenEffect iid token attrs.id)
              [toMessage $ chooseAndDiscardCard iid attrs.source]
          disable attrs
      pure e
    SkillTestEnds sid _ _ | attrs.metaInt == Just 2 && isTarget sid attrs.target -> do
      whenJustM getSkillTestInvestigator \iid -> do
        mSkillTestResult <- fmap skillTestResult <$> getSkillTest
        case mSkillTestResult of
          Just (SucceededBy _ _) -> do
            unblockedConnectedLocations <- getAccessibleLocations iid attrs
            chooseOrRunOneM iid do
              labeled "Do not move to a connecting location" nothing
              targets unblockedConnectedLocations $ moveTo attrs iid
            disable attrs
          _ -> disable attrs
      pure e
    _ -> MistsOfRlyeh4Effect <$> liftRunMessage msg attrs
