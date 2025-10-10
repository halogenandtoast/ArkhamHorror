module Arkham.Enemy.Cards.Azathoth (azathoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message (pattern R1)
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Azathoth = Azathoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azathoth :: EnemyCard Azathoth
azathoth =
  enemyWith
    Azathoth
    Cards.azathoth
    (0, Static 1, 0)
    (3, 3)
    (\a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing})

instance HasAbilities Azathoth where
  getAbilities (Azathoth a) =
    [ groupLimit PerGame
        $ restricted a 1 (exists $ IncludeOmnipotent $ be a <> EnemyWithDoom (atLeast 10))
        $ forced AnyWindow
    ]

instance HasModifiersFor Azathoth where
  getModifiersFor (Azathoth attrs) = do
    noAgenda <- selectNone AnyAgenda
    modifySelf attrs $ Omnipotent : [CountAllDoomInPlay | noAgenda]

instance RunMessage Azathoth where
  runMessage msg e@(Azathoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure e
    PlaceDoomOnAgenda n _ -> do
      noAgenda <- selectNone AnyAgenda
      when noAgenda $ placeDoom attrs attrs n
      pure e
    _ -> Azathoth <$> liftRunMessage msg attrs
