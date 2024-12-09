module Arkham.Enemy.Cards.DagonAwakenedAndEnragedIntoTheMaelstrom (
  dagonAwakenedAndEnragedIntoTheMaelstrom,
  DagonAwakenedAndEnragedIntoTheMaelstrom (..),
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

newtype DagonAwakenedAndEnragedIntoTheMaelstrom = DagonAwakenedAndEnragedIntoTheMaelstrom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dagonAwakenedAndEnragedIntoTheMaelstrom :: EnemyCard DagonAwakenedAndEnragedIntoTheMaelstrom
dagonAwakenedAndEnragedIntoTheMaelstrom =
  enemy
    DagonAwakenedAndEnragedIntoTheMaelstrom
    Cards.dagonAwakenedAndEnragedIntoTheMaelstrom
    (7, Static 1, 4)
    (1, 2)

instance HasModifiersFor DagonAwakenedAndEnragedIntoTheMaelstrom where
  getModifiersFor (DagonAwakenedAndEnragedIntoTheMaelstrom a) = do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    modifySelfWhen a (n > 0) [EnemyFight (-n)]

instance HasAbilities DagonAwakenedAndEnragedIntoTheMaelstrom where
  getAbilities (DagonAwakenedAndEnragedIntoTheMaelstrom a) =
    extend
      a
      [ restricted a 1 (exists $ enemyIs Cards.dagonsBrood)
          $ forced
          $ oneOf [EnemyDealtDamage #after AnyDamageEffect (be a) AnySource, EnemyEvaded #after Anyone (be a)]
      ]

instance RunMessage DagonAwakenedAndEnragedIntoTheMaelstrom where
  runMessage msg e@(DagonAwakenedAndEnragedIntoTheMaelstrom attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (map Window.windowType -> ws) _ -> do
      brood <- select $ enemyIs Cards.dagonsBrood
      for_ ws \case
        Window.EnemyEvaded iid _ -> for_ brood $ push . Msg.EnemyEvaded iid
        Window.DealtDamage source damageEffect _ n -> for_ brood \target -> push $ EnemyDamage target $ DamageAssignment source n damageEffect False False
        _ -> pure ()

      pure e
    _ -> DagonAwakenedAndEnragedIntoTheMaelstrom <$> liftRunMessage msg attrs
