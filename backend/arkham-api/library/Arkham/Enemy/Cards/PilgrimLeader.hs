module Arkham.Enemy.Cards.PilgrimLeader (pilgrimLeader) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))

newtype PilgrimLeader = PilgrimLeader EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "Spawn - Any location (empty, if able)." FirstLocation falls through to every
location once no empty one is left.
-}
pilgrimLeader :: EnemyCard PilgrimLeader
pilgrimLeader =
  enemyWith PilgrimLeader Cards.pilgrimLeader
    $ spawnAtL
    ?~ SpawnAt (FirstLocation [EmptyLocation, Anywhere])

instance HasModifiersFor PilgrimLeader where
  getModifiersFor (PilgrimLeader a) =
    -- "While Pilgrim Leader is ready, each other Cultist enemy gets +2 fight and
    -- gains hunter and relentless."
    unless a.exhausted
      $ modifySelect
        a
        (EnemyWithTrait Cultist <> not_ (be a))
        [EnemyFight 2, AddKeyword Keyword.Hunter, AddKeyword Keyword.Relentless]

instance RunMessage PilgrimLeader where
  runMessage msg (PilgrimLeader attrs) = runQueueT $ PilgrimLeader <$> liftRunMessage msg attrs
