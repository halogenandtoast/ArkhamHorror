module Arkham.Enemy.Cards.NyarlathotepMessengerOfTheOuterGods (
  nyarlathotepMessengerOfTheOuterGods,
  NyarlathotepMessengerOfTheOuterGods (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Placement
import Arkham.Prelude

newtype NyarlathotepMessengerOfTheOuterGods = NyarlathotepMessengerOfTheOuterGods EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepMessengerOfTheOuterGods :: EnemyCard NyarlathotepMessengerOfTheOuterGods
nyarlathotepMessengerOfTheOuterGods =
  enemy
    NyarlathotepMessengerOfTheOuterGods
    Cards.nyarlathotepMessengerOfTheOuterGods
    (3, Static 4, 4)
    (0, 1)

instance RunMessage NyarlathotepMessengerOfTheOuterGods where
  runMessage msg e@(NyarlathotepMessengerOfTheOuterGods attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceEnemy attrs.id (StillInHand iid)
      pure e
    _ -> NyarlathotepMessengerOfTheOuterGods <$> runMessage msg attrs
