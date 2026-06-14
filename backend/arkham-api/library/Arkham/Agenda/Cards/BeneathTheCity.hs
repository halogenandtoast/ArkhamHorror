module Arkham.Agenda.Cards.BeneathTheCity (beneathTheCity) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype BeneathTheCity = BeneathTheCity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheCity :: AgendaCard BeneathTheCity
beneathTheCity = agendaWith (1, A) BeneathTheCity Cards.beneathTheCity (Static 0) (doomThresholdL .~ Nothing)

instance HasAbilities BeneathTheCity where
  getAbilities (BeneathTheCity a)
    | onSide A a =
        -- [Forced] When doom would be placed on a card other than a story asset,
        -- place 1 Disturbance instead.
        [forcedAbility a 1 $ WouldPlaceDoomCounter #when AnySource (NotTarget $ AssetTargetMatches StoryAsset)]
    | otherwise = []

instance RunMessage BeneathTheCity where
  runMessage msg a@(BeneathTheCity attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      -- Replacement effect: cancel the doom placement and raise Disturbance instead.
      cancelWindowBatch ws
      scenarioSpecific "increaseDisturbance" ()
      -- Threshold to advance: 6, or 8 with 1-2 investigators. The scenario applies
      -- the increase asynchronously, so compare against this placement's result (+1).
      disturbance <- getDisturbance
      playerCount <- getPlayerCount
      let threshold = if playerCount <= 2 then 8 else 6
      when (disturbance + 1 >= threshold) $ advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Objective: the artifacts are prepared for the showdown. Flip to the back;
      -- Cthulhu's awakening proceeds from the scenario/act side.
      advanceAgendaDeck attrs
      pure a
    _ -> BeneathTheCity <$> liftRunMessage msg attrs
