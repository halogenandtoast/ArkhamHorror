module Arkham.Enemy.Cards.NyarlathotepMessengerOfTheOuterGods
  ( nyarlathotepMessengerOfTheOuterGods
  , NyarlathotepMessengerOfTheOuterGods(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NyarlathotepMessengerOfTheOuterGods = NyarlathotepMessengerOfTheOuterGods EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepMessengerOfTheOuterGods :: EnemyCard NyarlathotepMessengerOfTheOuterGods
nyarlathotepMessengerOfTheOuterGods = enemy NyarlathotepMessengerOfTheOuterGods Cards.nyarlathotepMessengerOfTheOuterGods (3, Static 4, 4) (0, 1)

instance RunMessage NyarlathotepMessengerOfTheOuterGods where
  runMessage msg (NyarlathotepMessengerOfTheOuterGods attrs) =
    NyarlathotepMessengerOfTheOuterGods <$> runMessage msg attrs
