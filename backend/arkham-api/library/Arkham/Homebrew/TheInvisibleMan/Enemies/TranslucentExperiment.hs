module Arkham.Homebrew.TheInvisibleMan.Enemies.TranslucentExperiment (translucentExperiment) where

import Arkham.ChaosToken.Types
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isFighting)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Cards

newtype TranslucentExperiment = TranslucentExperiment EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

translucentExperiment :: EnemyCard TranslucentExperiment
translucentExperiment = enemy TranslucentExperiment Cards.translucentExperiment

worsenedFaces :: [ChaosTokenFace]
worsenedFaces = [Skull, Cultist, Tablet, ElderThing]

instance HasModifiersFor TranslucentExperiment where
  getModifiersFor (TranslucentExperiment a) = unless a.exhausted do
    getSkillTest >>= traverse_ \st -> whenM (isFighting a) do
      let worsened = filter ((`elem` worsenedFaces) . (.face)) st.revealedChaosTokens
      modifyEach a (map ChaosTokenTarget worsened) [ChaosTokenValueModifier (-2)]

instance RunMessage TranslucentExperiment where
  runMessage msg (TranslucentExperiment attrs) =
    TranslucentExperiment <$> runMessage msg attrs
