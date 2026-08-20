module Arkham.Act.Cards.NightOfTheZealot.TheDevourerBelow.InvestigatingTheTrail (investigatingTheTrail) where

import Arkham.Act.CardDefs.NightOfTheZealot.TheDevourerBelow qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.NightOfTheZealot.Key
import Arkham.Card
import Arkham.EncounterCard
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Location.CardDefs.NightOfTheZealot.TheDevourerBelow qualified as Locations

newtype InvestigatingTheTrail = InvestigatingTheTrail ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

investigatingTheTrail :: ActCard InvestigatingTheTrail
investigatingTheTrail =
  act (1, A) InvestigatingTheTrail Cards.investigatingTheTrail (groupClueCost (PerPlayer 3))

instance RunMessage InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mRitualSite <- getLocationByName "Ritual Site"
      when (isNothing mRitualSite) $ placeSetAsideLocation_ Locations.ritualSite

      cultistsWhoGotAway <-
        traverse (genCard . lookupEncounterCardDef)
          =<< getRecordedCardCodes CultistsWhoGotAway

      mainPath <- getJustLocationByName "Main Path"
      for_ cultistsWhoGotAway (`createEnemyAt_` mainPath)

      advanceActDeck attrs
      pure a
    _ -> InvestigatingTheTrail <$> liftRunMessage msg attrs
