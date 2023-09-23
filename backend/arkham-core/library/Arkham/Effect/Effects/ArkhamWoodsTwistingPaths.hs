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
arkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths . uncurry4 (baseAttrs "01151")

instance HasModifiersFor ArkhamWoodsTwistingPaths

instance RunMessage ArkhamWoodsTwistingPaths where
  runMessage msg e@(ArkhamWoodsTwistingPaths attrs) = case msg of
    PassedThisSkillTest _ (LocationSource lid) -> do
      arkhamWoodsTwistingPathsId <- getJustLocationByName ("Arkham Woods" <:> "Twisting Paths")
      when (lid == arkhamWoodsTwistingPathsId)
        $ case effectMetadata attrs of
          Just (EffectMessages msgs) -> pushAll (msgs <> [disable attrs])
          _ -> push $ disable attrs
      pure e
    FailedThisSkillTest _ (LocationSource lid) -> do
      arkhamWoodsTwistingPathsId <- getJustLocationByName ("Arkham Woods" <:> "Twisting Paths")
      when (lid == arkhamWoodsTwistingPathsId) (push $ disable attrs)
      pure e
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
