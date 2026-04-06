module Arkham.Event.Events.UltimateSacrifice4 (ultimateSacrifice4, ultimateSacrifice4Effect) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getPhase)

newtype UltimateSacrifice4 = UltimateSacrifice4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimateSacrifice4 :: EventCard UltimateSacrifice4
ultimateSacrifice4 = event UltimateSacrifice4 Cards.ultimateSacrifice4

instance RunMessage UltimateSacrifice4 where
  runMessage msg e@(UltimateSacrifice4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      matchingDon't (== EndPhase)
      matchingDon't (== After EndPhase)
      createCardEffect Cards.ultimateSacrifice4 Nothing attrs (InvestigatorTarget iid)
      pushAll [Again (Begin #investigation), Begin #investigation]
      pure e
    _ -> UltimateSacrifice4 <$> liftRunMessage msg attrs

newtype UltimateSacrifice4Effect = UltimateSacrifice4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimateSacrifice4Effect :: EffectArgs -> UltimateSacrifice4Effect
ultimateSacrifice4Effect = cardEffect UltimateSacrifice4Effect Cards.ultimateSacrifice4

instance RunMessage UltimateSacrifice4Effect where
  runMessage msg e@(UltimateSacrifice4Effect attrs) = runQueueT $ case msg of
    EndPhase -> do
      phase <- getPhase
      if phase == #investigation
        then do
          case attrs.target of
            InvestigatorTarget iid -> do
              sufferPhysicalTrauma iid 1
              investigatorDefeated attrs iid
            _ -> pure ()
          disableReturn e
        else pure e
    _ -> UltimateSacrifice4Effect <$> liftRunMessage msg attrs
