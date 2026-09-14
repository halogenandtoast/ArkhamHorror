module Arkham.Homebrew.DarkMatter.Enemies.Rats (rats) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Rats = Rats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rats :: EnemyCard Rats
rats = enemy Rats Cards.rats

-- "Forced - After you defeat Rats?: Draw the top card of the encounter deck."
instance HasAbilities Rats where
  getAbilities (Rats a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage Rats where
  runMessage msg e@(Rats attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure e
    _ -> Rats <$> liftRunMessage msg attrs
