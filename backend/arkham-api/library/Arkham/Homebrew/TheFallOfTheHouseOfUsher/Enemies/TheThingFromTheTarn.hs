module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Enemies.TheThingFromTheTarn (theThingFromTheTarn) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype TheThingFromTheTarn = TheThingFromTheTarn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingFromTheTarn :: EnemyCard TheThingFromTheTarn
theThingFromTheTarn = enemy TheThingFromTheTarn Cards.theThingFromTheTarn

instance HasModifiersFor TheThingFromTheTarn where
  getModifiersFor (TheThingFromTheTarn a) = modifySelf a [CannotMove, CannotBeMoved]

instance HasAbilities TheThingFromTheTarn where
  -- The exempted source is Ancestral Corruption's ability 2; `AbilityOnCard` cannot
  -- name an act (`sourceCard` has no `ActSource` arm), so it is matched by type+index.
  getAbilities (TheThingFromTheTarn a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a)
      $ SourceUsedBy You
      <> NotSource (SourceIsType ActType <> SourceIsAbility (AbilityWithIndex 2))

totalDamageDealt :: [Window.Window] -> Int
totalDamageDealt ws = sum [n | Window.DealtDamage _ _ _ n <- map Window.windowType ws]

instance RunMessage TheThingFromTheTarn where
  runMessage msg e@(TheThingFromTheTarn attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (totalDamageDealt -> n) _ -> do
      -- The window is post-defeat, so a killing blow leaves nothing to ready.
      whenM (selectAny $ EnemyWithId attrs.id) do
        readyThis attrs
        when (n >= 3) $ enemyAttackModifiers (attrs.ability 1) attrs [DamageDealt 1, HorrorDealt 1]
        initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> TheThingFromTheTarn <$> liftRunMessage msg attrs
