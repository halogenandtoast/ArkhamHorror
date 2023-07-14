module Arkham.Enemy.Cards.HuntingHorror (
  huntingHorror,
  HuntingHorror (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Placement
import Arkham.RequestedChaosTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Token qualified as Token

newtype HuntingHorror = HuntingHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingHorror :: EnemyCard HuntingHorror
huntingHorror = enemy HuntingHorror Cards.huntingHorror (2, Static 3, 2) (1, 1)

instance HasAbilities HuntingHorror where
  getAbilities (HuntingHorror x) =
    withBaseAbilities
      x
      [ mkAbility x 1 $
          ForcedAbility $
            PhaseBegins Timing.When $
              PhaseIs
                EnemyPhase
      , mkAbility x 2 $
          ForcedAbility $
            EnemyLeavesPlay Timing.When $
              EnemyWithId $
                toId x
      ]

instance RunMessage HuntingHorror where
  runMessage msg e@(HuntingHorror attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ RequestChaosTokens source Nothing (Reveal 1) SetAside
      pure e
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      when
        ( any
            (`elem` map chaosTokenFace tokens)
            [Skull, Cultist, Tablet, ElderThing, AutoFail]
        )
        (push (Ready $ toTarget attrs))
      pure e
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      pushAll (resolve $ PlaceEnemyInVoid enemyId)
      pure e
    When (PlaceEnemyInVoid eid) | eid == enemyId -> do
      pure
        . HuntingHorror
        $ attrs
          & (tokensL %~ removeAllTokens Doom . removeAllTokens Clue . removeAllTokens Token.Damage)
          & (placementL .~ OutOfPlay VoidZone)
    _ -> HuntingHorror <$> runMessage msg attrs
