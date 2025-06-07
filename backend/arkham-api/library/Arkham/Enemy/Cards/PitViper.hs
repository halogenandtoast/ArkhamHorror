module Arkham.Enemy.Cards.PitViper (pitViper) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype PitViper = PitViper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitViper :: EnemyCard PitViper
pitViper = enemy PitViper Cards.pitViper (3, Static 1, 3) (1, 0)

instance HasAbilities PitViper where
  getAbilities (PitViper a) =
    extend1 a
      $ restricted a 1 (youExist $ not_ (HasMatchingTreachery $ treacheryIs Treacheries.poisoned))
      $ forced
      $ DealtDamage #after (SourceIsEnemyAttack $ be a) You

instance RunMessage PitViper where
  runMessage msg e@(PitViper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      poisoned <- getSetAsidePoisoned
      createWeaknessInThreatArea poisoned iid
      pure e
    _ -> PitViper <$> liftRunMessage msg attrs
