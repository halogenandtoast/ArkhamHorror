module Arkham.Act.Cards.InvestigatingTheTrail where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.EncounterCard
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message

newtype InvestigatingTheTrail = InvestigatingTheTrail ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

investigatingTheTrail :: ActCard InvestigatingTheTrail
investigatingTheTrail = act
  (1, A)
  InvestigatingTheTrail
  Cards.investigatingTheTrail
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      mRitualSiteId <- getLocationIdByName "Ritual Site"
      mainPathId <- getJustLocationIdByName "Main Path"
      when (isNothing mRitualSiteId) $ do
        placeRitualSite <- placeSetAsideLocation_ Locations.ritualSite
        push placeRitualSite
      cultistsWhoGotAway <- traverse (genCard . lookupEncounterCardDef)
        =<< getRecordedCardCodes CultistsWhoGotAway
      createEnemies <- for cultistsWhoGotAway
        $ \card -> createEnemyAt_ card mainPathId Nothing

      pushAll $ createEnemies <> [AdvanceActDeck actDeckId (toSource attrs)]
      pure a
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
