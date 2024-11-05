module Arkham.Treachery.Cards.TakenCaptive (takenCaptive, TakenCaptive (..)) where

import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TakenCaptive = TakenCaptive TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takenCaptive :: TreacheryCard TakenCaptive
takenCaptive = treachery TakenCaptive Cards.takenCaptive

instance RunMessage TakenCaptive where
  runMessage msg t@(TakenCaptive attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      holdingCells <- selectJust $ locationIs Locations.holdingCells
      ks <- iid.keys
      for_ ks (placeKey holdingCells)
      forInvestigator iid $ ScenarioSpecific "captured" Null
      pure t
    _ -> TakenCaptive <$> liftRunMessage msg attrs
