module Arkham.Enemy.Cards.ScreechingBanshee (screechingBanshee) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Trait (Trait (Relic, Spell))

newtype ScreechingBanshee = ScreechingBanshee EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

screechingBanshee :: EnemyCard ScreechingBanshee
screechingBanshee = enemy ScreechingBanshee Cards.screechingBanshee (3, Static 3, 1) (2, 0)

instance HasAbilities ScreechingBanshee where
  getAbilities (ScreechingBanshee a) =
    extend1 a
      $ restricted a 1 (thisExists a (EnemyAt HauntedLocation))
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a)
      $ SourceOwnedBy You
      <> NotSource (mapOneOf SourceWithTrait [Spell, Relic])

instance RunMessage ScreechingBanshee where
  runMessage msg e@(ScreechingBanshee attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs.id $ runLocationHauntedAbilities iid
      pure e
    _ -> ScreechingBanshee <$> liftRunMessage msg attrs
