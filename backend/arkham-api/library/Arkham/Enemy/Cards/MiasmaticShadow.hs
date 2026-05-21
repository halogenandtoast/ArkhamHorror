module Arkham.Enemy.Cards.MiasmaticShadow (miasmaticShadow) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Trait

newtype MiasmaticShadow = MiasmaticShadow EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miasmaticShadow :: EnemyCard MiasmaticShadow
miasmaticShadow = enemy MiasmaticShadow Cards.miasmaticShadow (3, Static 3, 3) (1, 1)

instance HasModifiersFor MiasmaticShadow where
  getModifiersFor (MiasmaticShadow a) =
    modifySelf
      a
      [ CannotBeDamagedByPlayerSourcesExcept
          $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic, SourceWithTrait Science]
      ]

instance HasAbilities MiasmaticShadow where
  getAbilities (MiasmaticShadow a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ DiscardedFromHand #after (You <> InvestigatorAt (locationWithEnemy a)) #any #any

instance RunMessage MiasmaticShadow where
  runMessage msg e@(MiasmaticShadow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      engageEnemy iid attrs
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> MiasmaticShadow <$> liftRunMessage msg attrs
