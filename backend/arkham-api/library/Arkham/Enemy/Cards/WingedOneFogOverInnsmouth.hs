module Arkham.Enemy.Cards.WingedOneFogOverInnsmouth (
  wingedOneFogOverInnsmouth,
  WingedOneFogOverInnsmouth (..),
)
where

import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Helpers.Source
import Arkham.Location.Projection
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype WingedOneFogOverInnsmouth = WingedOneFogOverInnsmouth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

wingedOneFogOverInnsmouth :: EnemyCard WingedOneFogOverInnsmouth
wingedOneFogOverInnsmouth = enemy WingedOneFogOverInnsmouth Cards.wingedOneFogOverInnsmouth (3, Static 5, 0) (1, 1)

instance HasModifiersFor WingedOneFogOverInnsmouth where
  getModifiersFor (WingedOneFogOverInnsmouth a) = modifySelfMaybe a do
    location <- MaybeT $ field EnemyLocation a.id
    shroud <- MaybeT $ field LocationShroud location
    pure [EnemyEvade shroud]

instance RunMessage WingedOneFogOverInnsmouth where
  runMessage msg (WingedOneFogOverInnsmouth attrs) = runQueueT $ case msg of
    Msg.EnemyDamage eid damage | eid == attrs.id -> fmap WingedOneFogOverInnsmouth do
      traits <- sourceTraits damage.source
      let shouldUpdate = damage.amount > 1 && none (`elem` traits) [Ranged, Firearm, Spell]
      let damage' = if shouldUpdate then damage {damageAssignmentAmount = damage.amount - 1} else damage
      liftRunMessage (Msg.EnemyDamage eid damage') attrs
    _ -> WingedOneFogOverInnsmouth <$> liftRunMessage msg attrs
