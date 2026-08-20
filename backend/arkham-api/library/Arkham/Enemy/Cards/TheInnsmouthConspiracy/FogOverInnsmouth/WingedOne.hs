module Arkham.Enemy.Cards.TheInnsmouthConspiracy.FogOverInnsmouth.WingedOne (
  wingedOne,
  WingedOne (..),
)
where

import Arkham.DamageEffect
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.FogOverInnsmouth qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Helpers.Source
import Arkham.Location.Projection
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype WingedOne = WingedOne EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

wingedOne :: EnemyCard WingedOne
wingedOne = enemy WingedOne Cards.wingedOne

instance HasModifiersFor WingedOne where
  getModifiersFor (WingedOne a) = modifySelfMaybe a do
    location <- MaybeT $ field EnemyLocation a.id
    shroud <- MaybeT $ field LocationShroud location
    pure [EnemyEvade shroud]

instance RunMessage WingedOne where
  runMessage msg (WingedOne attrs) = runQueueT $ case msg of
    Msg.DealDamage (EnemyTarget eid) damage | eid == attrs.id -> fmap WingedOne do
      traits <- sourceTraits damage.source
      let shouldUpdate = damage.amount > 1 && none (`elem` traits) [Ranged, Firearm, Spell]
      let damage' = if shouldUpdate then damage {damageAssignmentAmount = damage.amount - 1} else damage
      liftRunMessage (Msg.DealDamage (EnemyTarget eid) damage') attrs
    _ -> WingedOne <$> liftRunMessage msg attrs
