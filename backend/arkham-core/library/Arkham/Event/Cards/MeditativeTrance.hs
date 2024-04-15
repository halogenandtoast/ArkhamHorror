module Arkham.Event.Cards.MeditativeTrance (meditativeTrance, MeditativeTrance (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Slot (isEmptySlot)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype MeditativeTrance = MeditativeTrance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meditativeTrance :: EventCard MeditativeTrance
meditativeTrance = event MeditativeTrance Cards.meditativeTrance

instance RunMessage MeditativeTrance where
  runMessage msg e@(MeditativeTrance attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      slots <- fieldMap InvestigatorSlots (findWithDefault [] #arcane) iid
      let n = count (not . isEmptySlot) slots
      push $ DoStep n msg
      pure e
    DoStep n msg'@(PlayThisEvent iid eid) | eid == toId attrs && n > 0 -> do
      let source = toSource eid
      withHorror <- canHaveHorrorHealed source iid
      withDamage <- canHaveDamageHealed source iid
      when (withHorror || withDamage) $ do
        chooseOrRunOne iid
          $ [DamageLabel iid [HealDamage (toTarget iid) source 1, DoStep (n - 1) msg'] | withDamage]
          <> [HorrorLabel iid [HealHorror (toTarget iid) source 1, DoStep (n - 1) msg'] | withHorror]
      pure e
    _ -> MeditativeTrance <$> lift (runMessage msg attrs)
