module Arkham.Asset.Cards.MistsOfRlyeh2 (mistsOfRlyeh2, MistsOfRlyeh2 (..), mistsOfRlyeh2Effect) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Evade
import Arkham.Movement
import Arkham.Prelude
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
  runMessage msg a@(MistsOfRlyeh2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pushAll
        $ [ createCardEffect Cards.mistsOfRlyeh2 (effectInt 1) source sid
          , createCardEffect Cards.mistsOfRlyeh2 (effectInt 2) source sid
          , skillTestModifier sid source iid (SkillModifier #willpower 1)
          ]
        <> chooseEvade
      pure a
    _ -> MistsOfRlyeh2 <$> runMessage msg attrs

newtype MistsOfRlyeh2Effect = MistsOfRlyeh2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh2Effect :: EffectArgs -> MistsOfRlyeh2Effect
mistsOfRlyeh2Effect = cardEffect MistsOfRlyeh2Effect Cards.mistsOfRlyeh2

instance RunMessage MistsOfRlyeh2Effect where
  runMessage msg e@(MistsOfRlyeh2Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | attrs.metaInt == Just 1 -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              token.face
                `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
                && iid
                == st.investigator
                && isTarget st attrs.target
        when triggers do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [toMessage $ chooseAndDiscardCard iid effectSource]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds sid _ _ | attrs.metaInt == Just 2 && isTarget sid attrs.target -> do
      whenJustM getSkillTestInvestigator \iid -> do
        mSkillTestResult <- fmap skillTestResult <$> getSkillTest
        case mSkillTestResult of
          Just (SucceededBy _ _) -> do
            unblockedConnectedLocationIds <- getAccessibleLocations iid attrs
            player <- getPlayer iid
            let
              moveOptions =
                chooseOrRunOne player
                  $ [Label "Do not move to a connecting location" []]
                  <> [ targetLabel lid [Move $ move attrs iid lid]
                     | lid <- unblockedConnectedLocationIds
                     ]
            pushAll [moveOptions, DisableEffect effectId]
          _ -> push (DisableEffect effectId)
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push $ DisableEffect effectId
      pure e
    _ -> MistsOfRlyeh2Effect <$> runMessage msg attrs
