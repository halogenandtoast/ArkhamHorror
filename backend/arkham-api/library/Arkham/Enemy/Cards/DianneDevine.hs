module Arkham.Enemy.Cards.DianneDevine (
  dianneDevine,
  DianneDevine (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait

newtype DianneDevine = DianneDevine EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevine :: EnemyCard DianneDevine
dianneDevine = enemy DianneDevine Cards.dianneDevine (2, Static 3, 2) (0, 0)

instance HasModifiersFor DianneDevine where
  getModifiersFor (DianneDevine a) = do
    modifySelect
      a
      (InvestigatorAt $ locationWithEnemy a)
      [CannotDiscoverClues, CannotTakeControlOfClues]

instance HasAbilities DianneDevine where
  getAbilities (DianneDevine a) =
    withBaseAbilities a [mkAbility a 1 $ ForcedAbility $ PhaseBegins #when #enemy]

instance RunMessage DianneDevine where
  runMessage msg e@(DianneDevine attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      locations <- select $ LocationWithAsset $ AssetWithFewestClues $ AssetWithTrait Bystander
      pushWhen (notNull locations)
        $ chooseOne lead
        $ targetLabels locations (only . EnemyMove (toId attrs))
      pure e
    _ -> DianneDevine <$> runMessage msg attrs
