module Arkham.Homebrew.AgesUnwound.Enemies.EternitysSentinel_016 (eternitysSentinel_016) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Window qualified as Window

newtype EternitysSentinel_016 = EternitysSentinel_016 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eternitysSentinel_016 :: EnemyCard EternitysSentinel_016
eternitysSentinel_016 =
  enemyWith EternitysSentinel_016 Cards.eternitysSentinel_016 (healthL .~ Nothing)

instance HasModifiersFor EternitysSentinel_016 where
  getModifiersFor (EternitysSentinel_016 a) = modifySelf a [CannotBeDamaged]

instance HasAbilities EternitysSentinel_016 where
  getAbilities (EternitysSentinel_016 a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)
      , restricted a 2 (thisExists a $ EnemyWithPlacement InTheShadows)
          $ silent
          $ LocationEntersPlay #after Anywhere
      , restricted a 3 (thisExists a $ EnemyWithPlacement InTheShadows) $ forced $ RoundEnds #when
      ]

instance RunMessage EternitysSentinel_016 where
  runMessage msg e@(EternitysSentinel_016 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disengageFromAll attrs
      place attrs InTheShadows
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let mlid = listToMaybe [lid | (Window.windowType -> Window.LocationEntersPlay lid) <- ws]
      for_ mlid (place attrs . AtLocation)
      pure e
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      placeDoom (attrs.ability 3) attrs 1
      pure e
    _ -> EternitysSentinel_016 <$> liftRunMessage msg attrs
