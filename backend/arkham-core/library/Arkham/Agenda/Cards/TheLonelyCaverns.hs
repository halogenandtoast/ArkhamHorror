module Arkham.Agenda.Cards.TheLonelyCaverns (TheLonelyCaverns (..), theLonelyCaverns) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Prelude

newtype TheLonelyCaverns = TheLonelyCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theLonelyCaverns :: AgendaCard TheLonelyCaverns
theLonelyCaverns =
  agendaWith (1, A) TheLonelyCaverns Cards.theLonelyCaverns (Static 7)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

instance HasAbilities TheLonelyCaverns where
  getAbilities (TheLonelyCaverns a) =
    [ restrictedAbility a 1 (exists $ YourLocation <> LocationWithoutClues) exploreAction_
    , mkAbility a 2 $ ForcedAbility $ AgendaAdvances #when $ AgendaWithId $ toId a
    ]

instance RunMessage TheLonelyCaverns where
  runMessage msg a@(TheLonelyCaverns attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      harbingerAlive <- getHasRecord TheHarbingerIsStillAlive
      yigsFury <- getRecordCount YigsFury
      harbinger <- genCard Enemies.harbingerOfValusia
      locationId <-
        if yigsFury >= 8
          then getJustLocation =<< getLeadInvestigatorId
          else selectJust $ FarthestLocationFromAll Anywhere
      createHarbinger <- createEnemyAt_ harbinger locationId (Just $ toTarget attrs)
      pushAll
        $ [createHarbinger | harbingerAlive]
        <> [advanceAgendaDeck attrs]
      pure a
    CreatedEnemyAt harbingerId _ (isTarget attrs -> True) -> do
      startingDamage <- getRecordCount TheHarbingerIsStillAlive
      pushWhen (startingDamage > 0)
        $ PlaceDamage (toSource attrs) (toTarget harbingerId) startingDamage
      pure a
    _ -> TheLonelyCaverns <$> runMessage msg attrs
