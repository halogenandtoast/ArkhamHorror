module Arkham.Enemy.Cards.UnsealedPhantasm (unsealedPhantasm) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype UnsealedPhantasm = UnsealedPhantasm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsealedPhantasm :: EnemyCard UnsealedPhantasm
unsealedPhantasm =
  enemyWith
    UnsealedPhantasm
    Cards.unsealedPhantasm
    (5, Static 5, 4)
    (2, 2)
    ( spawnAtL
        ?~ SpawnAtFirst
          [ SpawnAt (NearestLocationToYou $ "Mist-Pylon" <> LocationWithoutModifier (ScenarioModifier "collapsed"))
          , SpawnAt YourLocation
          ]
    )

instance HasModifiersFor UnsealedPhantasm where
  getModifiersFor (UnsealedPhantasm a) = do
    mpylon <- selectOne $ "Mist-Pylon" <> LocationWithEnemy (be a <> ReadyEnemy)
    for_ mpylon \lid -> modified_ a lid [CannotBeDamaged]

instance HasAbilities UnsealedPhantasm where
  getAbilities (UnsealedPhantasm a) =
    extend1 a
      $ restricted
        a
        1
        ( thisExists
            a
            ( ReadyEnemy
                <> not_ (EnemyAt $ "Mist-Pylon" <> LocationWithoutModifier (ScenarioModifier "collapsed"))
            )
            <> exists
              ( "Mist-Pylon"
                  <> LocationWithoutModifier (ScenarioModifier "collapsed")
                  <> not_ (LocationWithEnemy (be a))
              )
        )
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage UnsealedPhantasm where
  runMessage msg e@(UnsealedPhantasm attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      mlid <- field EnemyLocation attrs.id
      for_ mlid \lid -> do
        xs <-
          select
            $ NearestLocationToLocation lid
            $ "Mist-Pylon"
            <> LocationWithoutModifier (ScenarioModifier "collapsed")
            <> not_ (LocationWithEnemy (be attrs))
        lead <- getLead
        chooseOrRunOneM lead do
          targets xs (moveTowards (attrs.ability 1) attrs)
      pure e
    _ -> UnsealedPhantasm <$> liftRunMessage msg attrs
