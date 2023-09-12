module Arkham.Enemy.Cards.Azathoth (
  azathoth,
  Azathoth (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message

newtype Azathoth = Azathoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azathoth :: EnemyCard Azathoth
azathoth = enemy Azathoth Cards.azathoth (0, Static 1, 0) (0, 0)

instance HasAbilities Azathoth where
  getAbilities (Azathoth a) =
    [ restricted (enemyExists $ EnemyWithId (toId a) <> EnemyWithDoom (AtLeast $ Static 10))
        $ forcedAbility a 1 AnyWindow
    ]

instance HasModifiersFor Azathoth where
  getModifiersFor target (Azathoth attrs) | isTarget attrs target = do
    noAgenda <- selectNone AnyAgenda
    pure $ toModifiers attrs $ Omnipotent : [CountAllDoomInPlay | noAgenda]
  getModifiersFor _ _ = pure []

instance RunMessage Azathoth where
  runMessage msg e@(Azathoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push R1
      pure e
    PlaceDoomOnAgenda -> do
      noAgenda <- selectNone AnyAgenda
      pushWhen noAgenda $ PlaceDoom (toSource attrs) (toTarget attrs) 1
      pure e
    _ -> Azathoth <$> runMessage msg attrs
