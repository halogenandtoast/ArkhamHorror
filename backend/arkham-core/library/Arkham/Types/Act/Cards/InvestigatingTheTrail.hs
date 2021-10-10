module Arkham.Types.Act.Cards.InvestigatingTheTrail where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.EncounterCard
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype InvestigatingTheTrail = InvestigatingTheTrail ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

investigatingTheTrail :: ActCard InvestigatingTheTrail
investigatingTheTrail = act
  (1, A)
  InvestigatingTheTrail
  Cards.investigatingTheTrail
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance ActRunner env => RunMessage env InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      mRitualSiteId <- getLocationIdByName "Ritual Site"
      mainPathId <- getJustLocationIdByName "Main Path"
      when (isNothing mRitualSiteId) $ do
        ritualSite <- getSetAsideCard Locations.ritualSite
        push (PlaceLocation ritualSite)
      cultistsWhoGotAwayDefs <-
        mapMaybe
            (\case
              Recorded cCode -> Just $ lookupEncounterCardDef cCode
              CrossedOut _ -> Nothing
            )
          <$> getRecordSet CultistsWhoGotAway
      cultistsWhoGotAway <- traverse genEncounterCard cultistsWhoGotAwayDefs
      a <$ pushAll
        ([ CreateEnemyAt (EncounterCard card) mainPathId Nothing
         | card <- cultistsWhoGotAway
         ]
        <> [AdvanceActDeck actDeckId (toSource attrs)]
        )
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
