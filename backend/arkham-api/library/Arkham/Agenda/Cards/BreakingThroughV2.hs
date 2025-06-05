module Arkham.Agenda.Cards.BreakingThroughV2 (breakingThroughV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EncounterCardSource)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype BreakingThroughV2 = BreakingThroughV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingThroughV2 :: AgendaCard BreakingThroughV2
breakingThroughV2 = agenda (3, A) BreakingThroughV2 Cards.breakingThroughV2 (Static 6)

instance HasAbilities BreakingThroughV2 where
  getAbilities (BreakingThroughV2 x) =
    [mkAbility x 1 $ forced $ MovedBy #after You EncounterCardSource]

instance RunMessage BreakingThroughV2 where
  runMessage msg a@(BreakingThroughV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      yogMoveTo <-
        fromMaybeM
          (getJustLocationByName "Another Dimension")
          (getLocationByName "The Edge of the Universe")
      yog <- selectJust $ enemyIs Enemies.yogSothoth
      selectOne (locationIs Locations.realmsBeyondAllInOne) >>= \case
        Nothing -> enemyMoveTo yog yogMoveTo
        Just loc -> temporaryModifier loc attrs Blank $ enemyMoveTo yog yogMoveTo
      selectOne (locationIs Locations.realmsBeyondAllInOne) >>= traverse_ removeLocation
      advanceAgendaDeck attrs
      pure a
    _ -> BreakingThroughV2 <$> liftRunMessage msg attrs
