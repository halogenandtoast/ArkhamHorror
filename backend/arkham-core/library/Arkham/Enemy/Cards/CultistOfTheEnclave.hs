module Arkham.Enemy.Cards.CultistOfTheEnclave (
  cultistOfTheEnclave,
  CultistOfTheEnclave (..),
)
where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype CultistOfTheEnclave = CultistOfTheEnclave EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

cultistOfTheEnclave :: EnemyCard CultistOfTheEnclave
cultistOfTheEnclave =
  enemyWith
    CultistOfTheEnclave
    Cards.cultistOfTheEnclave
    (3, Static 2, 3)
    (1, 0)
    (spawnAtL ?~ "Basement")

instance HasAbilities CultistOfTheEnclave where
  getAbilities (CultistOfTheEnclave attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyAttacks #after You AnyEnemyAttack
          $ EnemyWithId
          $ toId attrs
      ]

instance RunMessage CultistOfTheEnclave where
  runMessage msg e@(CultistOfTheEnclave attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) _ (map chaosTokenFace -> tokens) -> do
      push $ ResetChaosTokens (toAbilitySource attrs 1)
      when (any (`elem` tokens) [#skull, #cultist, #tablet, #elderthing, #autofail]) $ do
        agenda <- selectJust AnyAgenda
        push $ PlaceDoom (toAbilitySource attrs 1) (toTarget agenda) 1
      pure e
    _ -> CultistOfTheEnclave <$> runMessage msg attrs
