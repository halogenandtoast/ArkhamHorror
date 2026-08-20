module Arkham.Enemy.Cards.TheDrownedCity.TheInescapable.TheInescapable (theInescapable) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.ForMovement (ForMovement (NotForMovement))
import Arkham.Matcher

newtype TheInescapable = TheInescapable EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInescapable :: EnemyCard TheInescapable
theInescapable =
  enemyWith TheInescapable Cards.theInescapable
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ ConnectedLocation NotForMovement <> EmptyLocation
      , SpawnAt $ ConnectedLocation NotForMovement
      ]

instance HasAbilities TheInescapable where
  getAbilities (TheInescapable a) =
    extend1 a
      $ groupLimit PerRound
      $ mkAbility a 1
      $ forced
      $ RevealChaosToken #after Anyone #elderthing

instance RunMessage TheInescapable where
  runMessage msg e@(TheInescapable attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      push $ HunterMove attrs.id
      pure e
    _ -> TheInescapable <$> liftRunMessage msg attrs
