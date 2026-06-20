module Arkham.Enemy.Cards.ThingInTheSarcophagus (thingInTheSarcophagus) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype ThingInTheSarcophagus = ThingInTheSarcophagus EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thingInTheSarcophagus :: EnemyCard ThingInTheSarcophagus
thingInTheSarcophagus =
  enemyWith ThingInTheSarcophagus Cards.thingInTheSarcophagus
    $ spawnAtL
    ?~ SpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor ThingInTheSarcophagus where
  getModifiersFor (ThingInTheSarcophagus a) = do
    modifySelfWhenM
      a
      (selectAny $ locationWithEnemy a <> LocationWithoutClues)
      [AddKeyword Keyword.Hunter]

instance HasAbilities ThingInTheSarcophagus where
  getAbilities (ThingInTheSarcophagus a) =
    extend1 a
      $ restricted a 1 (thisIs a ReadyEnemy)
      $ forced
      $ EnemyAttacked
        #when
        You
        (NotSource $ oneOf [SourceWithTrait Ranged, SourceWithTrait Firearm, SourceWithTrait Spell])
        (be a)

instance RunMessage ThingInTheSarcophagus where
  runMessage msg e@(ThingInTheSarcophagus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> ThingInTheSarcophagus <$> liftRunMessage msg attrs
