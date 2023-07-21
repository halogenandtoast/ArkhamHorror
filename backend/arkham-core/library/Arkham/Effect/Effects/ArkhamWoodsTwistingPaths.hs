module Arkham.Effect.Effects.ArkhamWoodsTwistingPaths (
  arkhamWoodsTwistingPaths,
  ArkhamWoodsTwistingPaths (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Name

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: EffectArgs -> ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths . uncurry4 (baseAttrs "01151")

instance HasModifiersFor ArkhamWoodsTwistingPaths

instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg e@(ArkhamWoodsTwistingPaths attrs) = case msg of
    PassedSkillTest _ _ (LocationSource lid) SkillTestInitiatorTarget {} _ _ ->
      do
        arkhamWoodsTwistingPathsId <-
          getJustLocationIdByName
            ("Arkham Woods" <:> "Twisting Paths")
        let disable = DisableEffect (effectId attrs)
        e
          <$ when
            (lid == arkhamWoodsTwistingPathsId)
            ( case effectMetadata attrs of
                Just (EffectMessages msgs) -> pushAll (msgs <> [disable])
                _ -> push disable
            )
    FailedSkillTest _ _ (LocationSource lid) SkillTestInitiatorTarget {} _ _ ->
      do
        arkhamWoodsTwistingPathsId <-
          getJustLocationIdByName
            ("Arkham Woods" <:> "Twisting Paths")
        e
          <$ when
            (lid == arkhamWoodsTwistingPathsId)
            (push $ DisableEffect $ effectId attrs)
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
