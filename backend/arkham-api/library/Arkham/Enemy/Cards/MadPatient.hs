module Arkham.Enemy.Cards.MadPatient (madPatient, MadPatient (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype MadPatient = MadPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madPatient :: EnemyCard MadPatient
madPatient =
  enemyWith
    MadPatient
    Cards.madPatient
    (2, Static 2, 3)
    (1, 0)
    $ (preyL .~ Prey MostRemainingSanity)
    . (spawnAtL ?~ SpawnAt (NearestLocationToYou "Asylum Halls"))

instance HasAbilities MadPatient where
  getAbilities (MadPatient a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacked #when You AnySource (be a)]

instance RunMessage MadPatient where
  runMessage msg e@(MadPatient attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ assignHorror iid source 1
      pure e
    _ -> MadPatient <$> runMessage msg attrs
