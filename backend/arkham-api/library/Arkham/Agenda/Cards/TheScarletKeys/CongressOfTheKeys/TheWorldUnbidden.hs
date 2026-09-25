module Arkham.Agenda.Cards.TheScarletKeys.CongressOfTheKeys.TheWorldUnbidden (theWorldUnbidden) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.Window (getTotalDamageAmounts)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheScarletKeys.CongressOfTheKeys.Helpers
import Data.Map.Strict qualified as Map

newtype TheWorldUnbidden = TheWorldUnbidden AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWorldUnbidden :: AgendaCard TheWorldUnbidden
theWorldUnbidden = agenda (2, A) TheWorldUnbidden Cards.theWorldUnbidden (Static 10)

instance HasAbilities TheWorldUnbidden where
  getAbilities (TheWorldUnbidden a) =
    [mkAbility a 1 $ silent $ DealtDamageOrHorror #when (SourceIs $ toSource a) You]

instance RunMessage TheWorldUnbidden where
  runMessage msg a@(TheWorldUnbidden attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioSpecific_ "shuffleAllConcealed"
      eachInvestigator \iid -> assignDamageAndHorror iid attrs 2 2
      advanceAgendaDeck attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (getTotalDamageAmounts iid -> amounts) _ -> do
      let (dmg, hrr) = Map.findWithDefault (0, 0) (toSource attrs) amounts
      when (dmg > 0 || hrr > 0) do
        unstableKeys <- select $ UnstableScarletKey <> ScarletKeyWithBearer (InvestigatorWithId iid)
        stableKeys <- select $ StableScarletKey <> ScarletKeyWithBearer (InvestigatorWithId iid)
        chooseOrRunOneM iid $ scenarioI18n do
          labeledValidate (notNull unstableKeys) "theWorldUnbidden.unstable" do
            chooseTargetM iid unstableKeys \k ->
              temporaryModifier k attrs CannotBeFlipped $ shift k
            pushAll [CancelDamage iid dmg, CancelHorror iid hrr]
          labeledValidate (notNull stableKeys) "theWorldUnbidden.stable" do
            doStep dmg $ DoStep hrr msg
          unscoped skip_

      pure a
    -- "Flip any number of Stable keys" — each flip cancels one more point, so the
    -- step re-enters itself with the remaining damage/horror until the investigator
    -- runs out of either stable keys or damage to cancel.
    DoStep dmg (DoStep hrr inner@(UseThisAbility iid (isSource attrs -> True) 1)) | dmg > 0 || hrr > 0 -> do
      stableKeys <- select $ StableScarletKey <> ScarletKeyWithBearer (InvestigatorWithId iid)
      chooseOrRunOneM iid $ scenarioI18n do
        targets stableKeys \k -> do
          flipOver iid k
          chooseOneM iid do
            labeledValidate (dmg > 0) "theWorldUnbidden.damage" do
              push $ CancelDamage iid 1
              doStep (dmg - 1) $ DoStep hrr inner
            labeledValidate (hrr > 0) "theWorldUnbidden.horror" do
              push $ CancelHorror iid 1
              doStep dmg $ DoStep (hrr - 1) inner
        unscoped skip_
      pure a
    _ -> TheWorldUnbidden <$> liftRunMessage msg attrs
