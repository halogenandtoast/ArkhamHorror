module Arkham.Act.Cards.TheGameIsAfoot (theGameIsAfoot) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (..))
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Projection

newtype TheGameIsAfoot = TheGameIsAfoot ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGameIsAfoot :: ActCard TheGameIsAfoot
theGameIsAfoot = act (2, A) TheGameIsAfoot Cards.theGameIsAfoot Nothing

instance HasAbilities TheGameIsAfoot where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyEngaged #after Anyone (enemyIs Enemies.theRedGlovedManShroudedInMystery)
    ]

instance RunMessage TheGameIsAfoot where
  runMessage msg a@(TheGameIsAfoot attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      doom <- field AgendaDoom =<< selectJust AnyAgenda
      selectEach (enemyIs Enemies.theRedGlovedManShroudedInMystery) (`place` SetAsideZone)
      placeSetAsideLocations_ [Locations.theTowerBridge, Locations.towerOfLondon]
      shuffleEncounterDiscardBackIn
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.CrimsonConspiracy
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.Outsiders
      advanceToAgendaA attrs Agendas.theConnection
      advanceActDeck attrs
      placeDoomOnAgenda doom
      pure a
    _ -> TheGameIsAfoot <$> liftRunMessage msg attrs
