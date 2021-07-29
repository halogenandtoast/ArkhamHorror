module Arkham.Types.Effect.Effects.ArkhamWoodsTwistingPaths
  ( arkhamWoodsTwistingPaths
  , ArkhamWoodsTwistingPaths(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Source
import Arkham.Types.Target

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: EffectArgs -> ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths . uncurry4 (baseAttrs "01151")

instance HasModifiersFor env ArkhamWoodsTwistingPaths

instance (HasId (Maybe LocationId) env LocationMatcher, HasQueue env) => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg e@(ArkhamWoodsTwistingPaths attrs) = case msg of
    PassedSkillTest _ _ (LocationSource lid) SkillTestInitiatorTarget{} _ _ ->
      do
        arkhamWoodsTwistingPathsId <- getJustLocationIdByName
          (mkFullName "Arkham Woods" "Twisting Paths")
        let disable = DisableEffect (effectId attrs)
        e <$ when
          (lid == arkhamWoodsTwistingPathsId)
          (case effectMetadata attrs of
            Just (EffectMessages msgs) -> pushAll (msgs <> [disable])
            _ -> push disable
          )
    FailedSkillTest _ _ (LocationSource lid) SkillTestInitiatorTarget{} _ _ ->
      do
        arkhamWoodsTwistingPathsId <- getJustLocationIdByName
          (mkFullName "Arkham Woods" "Twisting Paths")
        e <$ when
          (lid == arkhamWoodsTwistingPathsId)
          (push $ DisableEffect $ effectId attrs)
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
