module Arkham.Enemy.Cards.HumbleSupplicant (humbleSupplicant) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Brotherhood, Cairo))

newtype HumbleSupplicant = HumbleSupplicant EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humbleSupplicant :: EnemyCard HumbleSupplicant
humbleSupplicant =
  enemyWith HumbleSupplicant Cards.humbleSupplicant
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ LocationWithTrait Cairo <> LocationWithEnemy (EnemyWithTrait Brotherhood)
      , SpawnAt $ LocationWithTrait Cairo
      ]

instance HasModifiersFor HumbleSupplicant where
  getModifiersFor (HumbleSupplicant a) = do
    modifySelect
      a
      (EnemyWithTrait Brotherhood <> EnemyAt (locationWithEnemy a))
      [CannotBeAttacked, CannotBeDamaged]

instance HasAbilities HumbleSupplicant where
  getAbilities (HumbleSupplicant a) =
    extend1 a
      $ restricted a 1 (exists $ EnemyWithTrait Brotherhood <> not_ (be a))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage HumbleSupplicant where
  runMessage msg e@(HumbleSupplicant attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ MoveToward (toTarget attrs) (LocationWithEnemy $ EnemyWithTrait Brotherhood)
      pure e
    _ -> HumbleSupplicant <$> liftRunMessage msg attrs
