module Arkham.Enemy.Cards.Azathoth (azathoth, Azathoth (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

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
    [restricted a 1 (exists $ be a <> EnemyWithDoom (atLeast 10)) $ forced AnyWindow]

instance HasModifiersFor Azathoth where
  getModifiersFor (Azathoth attrs) = do
    noAgenda <- selectNone AnyAgenda
    modifySelf attrs $ Omnipotent : [CountAllDoomInPlay | noAgenda]

instance RunMessage Azathoth where
  runMessage msg e@(Azathoth attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure e
    PlaceDoomOnAgenda n _ -> do
      noAgenda <- selectNone AnyAgenda
      pushWhen noAgenda $ placeDoom attrs attrs n
      pure e
    _ -> Azathoth <$> runMessage msg attrs
