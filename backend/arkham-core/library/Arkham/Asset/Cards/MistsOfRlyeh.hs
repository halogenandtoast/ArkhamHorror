module Arkham.Asset.Cards.MistsOfRlyeh (mistsOfRlyeh, MistsOfRlyeh (..), mistsOfRlyehEffect) where

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

newtype MistsOfRlyeh = MistsOfRlyeh AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh :: AssetCard MistsOfRlyeh
mistsOfRlyeh = asset MistsOfRlyeh Cards.mistsOfRlyeh

instance HasAbilities MistsOfRlyeh where
  getAbilities (MistsOfRlyeh a) = [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage MistsOfRlyeh where
  runMessage msg a@(MistsOfRlyeh attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade iid source)
      pushAll
        $ [ createCardEffect Cards.mistsOfRlyeh (Just $ EffectInt 1) source iid
          , createCardEffect Cards.mistsOfRlyeh (Just $ EffectInt 2) source iid
          ]
        <> chooseEvade
      pure a
    _ -> MistsOfRlyeh <$> runMessage msg attrs

newtype MistsOfRlyehEffect = MistsOfRlyehEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyehEffect :: EffectArgs -> MistsOfRlyehEffect
mistsOfRlyehEffect = cardEffect MistsOfRlyehEffect Cards.mistsOfRlyeh

instance RunMessage MistsOfRlyehEffect where
  runMessage msg e@(MistsOfRlyehEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | effectMetadata == Just (EffectInt 1) -> do
      when (effectTarget == InvestigatorTarget iid) $ do
        when (token.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
          $ pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [toMessage $ chooseAndDiscardCard iid effectSource]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds _ _ | effectMetadata == Just (EffectInt 2) -> do
      case effectTarget of
        InvestigatorTarget iid -> do
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
            _ -> push $ DisableEffect effectId
        _ -> error "Invalid Target"
      pure e
    SkillTestEnds _ _ -> do
      push $ DisableEffect effectId
      pure e
    _ -> MistsOfRlyehEffect <$> runMessage msg attrs
