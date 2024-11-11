module Arkham.Location.Cards.SunkenHalls (sunkenHalls, SunkenHalls (..)) where

import Arkham.Ability
import Arkham.Helpers.Window
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SunkenHalls = SunkenHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenHalls :: LocationCard SunkenHalls
sunkenHalls =
  locationWith SunkenHalls Cards.sunkenHalls 2 (PerPlayer 1)
    $ connectsToAdjacent
    . (floodLevelL ?~ PartiallyFlooded)

instance HasAbilities SunkenHalls where
  getAbilities (SunkenHalls a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist (InvestigatorWithKey BlackKey)) actionAbility
      , restricted a 2 (thisExists a (LocationWithKey BlackKey))
          $ forced
          $ EnemyEnters #after (be a) AnyEnemy
      ]

instance RunMessage SunkenHalls where
  runMessage msg l@(SunkenHalls attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      nonAttackEnemyDamage (attrs.ability 2) 2 eid
      pure l
    _ -> SunkenHalls <$> liftRunMessage msg attrs
