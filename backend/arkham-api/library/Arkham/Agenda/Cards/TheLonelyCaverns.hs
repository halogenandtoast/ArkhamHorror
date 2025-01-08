module Arkham.Agenda.Cards.TheLonelyCaverns (theLonelyCaverns) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Log (getRecordCount, whenHasRecord)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheLonelyCaverns = TheLonelyCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLonelyCaverns :: AgendaCard TheLonelyCaverns
theLonelyCaverns =
  agendaWith (1, A) TheLonelyCaverns Cards.theLonelyCaverns (Static 7)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

instance HasAbilities TheLonelyCaverns where
  getAbilities (TheLonelyCaverns a) =
    [ restricted a 1 (exists $ YourLocation <> LocationWithoutClues) exploreAction_
    , mkAbility a 2 $ forced $ AgendaAdvances #when (be a)
    ]

instance RunMessage TheLonelyCaverns where
  runMessage msg a@(TheLonelyCaverns attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      explore
        iid
        (attrs.ability 1)
        (mapOneOf CardWithPrintedLocationSymbol locationSymbols)
        PlaceExplored
        1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenHasRecord TheHarbingerIsStillAlive do
        yigsFury <- getRecordCount YigsFury
        lead <- getLead
        locations <-
          select
            $ if yigsFury >= 8
              then locationWithInvestigator lead
              else FarthestLocationFromAll Anywhere
        harbinger <- genCard Enemies.harbingerOfValusia
        chooseOrRunOneM lead do
          targets locations \location ->
            createEnemyWithAfter_ harbinger location \harbingerId -> do
              startingDamage <- getRecordCount TheHarbingerIsStillAlive
              when (startingDamage > 0) $ placeTokens attrs harbingerId #damage startingDamage
      advanceAgendaDeck attrs
      pure a
    _ -> TheLonelyCaverns <$> liftRunMessage msg attrs
