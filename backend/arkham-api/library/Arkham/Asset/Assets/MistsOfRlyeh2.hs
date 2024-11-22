module Arkham.Asset.Assets.MistsOfRlyeh2 (mistsOfRlyeh2, MistsOfRlyeh2 (..), mistsOfRlyeh2Effect) where

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

newtype MistsOfRlyeh2 = MistsOfRlyeh2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh2 :: AssetCard MistsOfRlyeh2
mistsOfRlyeh2 = asset MistsOfRlyeh2 Cards.mistsOfRlyeh2

instance HasAbilities MistsOfRlyeh2 where
  getAbilities (MistsOfRlyeh2 a) = [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage MistsOfRlyeh2 where
  runMessage msg a@(MistsOfRlyeh2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      createCardEffect Cards.mistsOfRlyeh2 (effectInt 1) source sid
      createCardEffect Cards.mistsOfRlyeh2 (effectInt 2) source sid
      skillTestModifier sid source iid (SkillModifier #willpower 1)
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    _ -> MistsOfRlyeh2 <$> liftRunMessage msg attrs

newtype MistsOfRlyeh2Effect = MistsOfRlyeh2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh2Effect :: EffectArgs -> MistsOfRlyeh2Effect
mistsOfRlyeh2Effect = cardEffect MistsOfRlyeh2Effect Cards.mistsOfRlyeh2

instance RunMessage MistsOfRlyeh2Effect where
  runMessage msg e@(MistsOfRlyeh2Effect attrs) = runQueueT $ case msg of
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
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> MistsOfRlyeh2Effect <$> liftRunMessage msg attrs
