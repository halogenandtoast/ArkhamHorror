module Arkham.Enemy.Cards.Azathoth (azathoth, Azathoth (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Azathoth = Azathoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azathoth :: EnemyCard Azathoth
azathoth = enemy Azathoth Cards.azathoth (0, Static 1, 0) (0, 0)

instance HasAbilities Azathoth where
  getAbilities (Azathoth a) =
    [restricted a 1 (exists $ be a <> EnemyWithDoom (atLeast 10)) $ forced AnyWindow]

instance HasModifiersFor Azathoth where
  getModifiersFor target (Azathoth attrs) | isTarget attrs target = do
    noAgenda <- selectNone AnyAgenda
    pure $ toModifiers attrs $ Omnipotent : [CountAllDoomInPlay | noAgenda]
  getModifiersFor _ _ = pure []

instance RunMessage Azathoth where
  runMessage msg e@(Azathoth attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure e
    PlaceDoomOnAgenda -> do
      noAgenda <- selectNone AnyAgenda
      pushWhen noAgenda $ placeDoom attrs attrs 1
      pure e
    _ -> Azathoth <$> runMessage msg attrs
