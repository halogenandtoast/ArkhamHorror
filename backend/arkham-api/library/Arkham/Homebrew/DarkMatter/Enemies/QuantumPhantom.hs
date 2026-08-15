module Arkham.Homebrew.DarkMatter.Enemies.QuantumPhantom (quantumPhantom) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Relic, Spell))

newtype QuantumPhantom = QuantumPhantom EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumPhantom :: EnemyCard QuantumPhantom
quantumPhantom = enemy QuantumPhantom Cards.quantumPhantom

{- | "Forced - When Quantum Phantom is defeated by damage (except from a [[Spell]]
or [[Relic]]): Instead of discarding it, place it in your threat area,
face-down."
-}
instance HasAbilities QuantumPhantom where
  getAbilities (QuantumPhantom a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated
        #when
        You
        ( DefeatedByMatches
            [ ByDamage
            , NotBy $ BySource $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic]
            ]
        )
        (be a)

instance RunMessage QuantumPhantom where
  runMessage msg e@(QuantumPhantom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      push $ HealAllDamage (EnemyTarget attrs.id) (attrs.ability 1)
      place attrs (FacedownInThreatArea iid)
      pure e
    _ -> QuantumPhantom <$> liftRunMessage msg attrs
