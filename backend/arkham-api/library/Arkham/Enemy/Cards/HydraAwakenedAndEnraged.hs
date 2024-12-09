module Arkham.Enemy.Cards.HydraAwakenedAndEnraged (
  hydraAwakenedAndEnraged,
  HydraAwakenedAndEnraged (..),
)
where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Trait (Trait (Sanctum))
import Arkham.Window qualified as Window

newtype HydraAwakenedAndEnraged = HydraAwakenedAndEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydraAwakenedAndEnraged :: EnemyCard HydraAwakenedAndEnraged
hydraAwakenedAndEnraged =
  enemy
    HydraAwakenedAndEnraged
    Cards.hydraAwakenedAndEnraged
    (7, Static 1, 4)
    (2, 1)

instance HasModifiersFor HydraAwakenedAndEnraged where
  getModifiersFor (HydraAwakenedAndEnraged a) = do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    modifySelfWhen a (n > 0) [EnemyFight (-n)]

instance HasAbilities HydraAwakenedAndEnraged where
  getAbilities (HydraAwakenedAndEnraged a) =
    extend
      a
      [ restricted a 1 (exists $ enemyIs Cards.hydrasBrood)
          $ forced
          $ oneOf [EnemyDealtDamage #after AnyDamageEffect (be a) AnySource, EnemyEvaded #after Anyone (be a)]
      ]

instance RunMessage HydraAwakenedAndEnraged where
  runMessage msg e@(HydraAwakenedAndEnraged attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (map Window.windowType -> ws) _ -> do
      brood <- select $ enemyIs Cards.hydrasBrood
      for_ ws \case
        Window.EnemyEvaded iid _ -> for_ brood $ push . Msg.EnemyEvaded iid
        Window.DealtDamage source damageEffect _ n -> for_ brood \target -> push $ EnemyDamage target $ DamageAssignment source n damageEffect False False
        _ -> pure ()

      pure e
    _ -> HydraAwakenedAndEnraged <$> liftRunMessage msg attrs
