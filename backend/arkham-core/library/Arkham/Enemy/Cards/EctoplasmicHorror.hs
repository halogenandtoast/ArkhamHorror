module Arkham.Enemy.Cards.EctoplasmicHorror (ectoplasmicHorror, EctoplasmicHorror (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EctoplasmicHorror = EctoplasmicHorror (EnemyAttrs `With` Meta)
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ectoplasmicHorror :: EnemyCard EctoplasmicHorror
ectoplasmicHorror = enemy (EctoplasmicHorror . (`with` Meta True)) Cards.ectoplasmicHorror (2, Static 2, 2) (0, 1)

instance RunMessage EctoplasmicHorror where
  runMessage msg e@(EctoplasmicHorror (With attrs meta)) = case msg of
    After (RevealChaosToken _ iid _) -> do
      n <-
        fromMaybe 0 <$> runMaybeT do
          EnemyTarget eid <- MaybeT getSkillTestTarget
          guard $ eid == attrs.id
          action <- MaybeT getSkillTestAction
          guard $ action `elem` [#evade, #fight]
          lift $ fieldMap InvestigatorSlots (count isEmptySlot . findWithDefault [] #arcane) iid

      if n > 0 && active meta
        then do
          withSkillTest \s ->
            push $ RequestChaosTokens (toSource s) (Just iid) (Reveal n) SetAside
          pure $ EctoplasmicHorror $ attrs `with` Meta False
        else pure e
    SkillTestEnded {} -> pure $ EctoplasmicHorror $ attrs `with` Meta True
    _ -> EctoplasmicHorror . (`with` meta) <$> runMessage msg attrs
