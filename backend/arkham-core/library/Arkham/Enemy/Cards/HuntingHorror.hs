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
import Arkham.Placement
import Arkham.RequestedChaosTokenStrategy
import Arkham.Token
import Arkham.Token qualified as Token

newtype HuntingHorror = HuntingHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingHorror :: EnemyCard HuntingHorror
huntingHorror = enemy HuntingHorror Cards.huntingHorror (2, Static 3, 2) (1, 1)

instance HasAbilities HuntingHorror where
  getAbilities (HuntingHorror x) =
    extend
      x
      [ mkAbility x 1 $ forced $ PhaseBegins #when #enemy
      , mkAbility x 2 $ forced $ EnemyLeavesPlay #when (be x)
      ]

instance RunMessage HuntingHorror where
  runMessage msg e@(HuntingHorror attrs@EnemyAttrs {..}) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (toAbilitySource attrs 1) Nothing (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ (map chaosTokenFace -> tokens) -> do
      push $ ResetChaosTokens (toSource attrs)
      pushWhen (any (`elem` tokens) [#skull, #cultist, #tablet, #elderthing, #autofail])
        $ Ready (toTarget attrs)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pushAll $ resolve $ PlaceEnemyInVoid enemyId
      pure e
    When (PlaceEnemyInVoid eid) | eid == enemyId -> do
      pure
        . HuntingHorror
        $ attrs
        & (tokensL %~ removeAllTokens Doom . removeAllTokens Clue . removeAllTokens Token.Damage)
        & (placementL .~ OutOfPlay VoidZone)
    _ -> HuntingHorror <$> runMessage msg attrs
