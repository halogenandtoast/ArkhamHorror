module Arkham.Enemy.Cards.SinsOfThePast.VengefulSpecter (
  vengefulSpecter,
  VengefulSpecter (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.CardDefs.SinsOfThePast qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Trait (Trait (Charm, Relic, Spell))

newtype VengefulSpecter = VengefulSpecter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengefulSpecter :: EnemyCard VengefulSpecter
vengefulSpecter = enemy VengefulSpecter Cards.vengefulSpecter

instance HasAbilities VengefulSpecter where
  getAbilities (VengefulSpecter attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when Matcher.AttackDamageEffect (EnemyWithId $ toId attrs) (atLeast 2)
          $ NotSource
          $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic, SourceWithTrait Charm]
      ]

instance RunMessage VengefulSpecter where
  runMessage msg e@(VengefulSpecter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reduceDamageTakenTo attrs 1
      pure e
    _ -> VengefulSpecter <$> liftRunMessage msg attrs
