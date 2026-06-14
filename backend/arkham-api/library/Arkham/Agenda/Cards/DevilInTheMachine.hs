module Arkham.Agenda.Cards.DevilInTheMachine (devilInTheMachine) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (struggleForAir)
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (needsAir)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMapM)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Projection
import Arkham.Trait (Trait (StarSpawn))
import Arkham.Treachery.Cards qualified as Treacheries

newtype DevilInTheMachine = DevilInTheMachine AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilInTheMachine :: AgendaCard DevilInTheMachine
devilInTheMachine = agenda (2, A) DevilInTheMachine Cards.devilInTheMachine (Static 5)

instance HasModifiersFor DevilInTheMachine where
  getModifiersFor (DevilInTheMachine a) = do
    healthModifier <- perPlayer 1
    modifySelect a (enemyIs Enemies.theInescapable) [HealthModifier healthModifier]

    -- "When The Inescapable moves via its hunter keyword, each location is
    -- considered to be connected to each adjacent location." In this scenario
    -- The Inescapable only moves via its hunter keyword, and there is no flag to
    -- distinguish a hunter move from other movement during a modifier query, so
    -- we gate on it currently moving (mirrors EverShiftingWalls). A location's
    -- grid neighbors are its adjacent positions.
    inescapableMoving <- selectAny (MovingEnemy <> enemyIs Enemies.theInescapable)
    when inescapableMoving do
      modifySelectMapM a Anywhere \loc -> do
        mpos <- field LocationPosition loc
        pure case mpos of
          Nothing -> []
          Just pos ->
            [ ConnectedToWhen
                (LocationWithId loc)
                (mapOneOf LocationInPosition pos.adjacents)
            ]

instance HasAbilities DevilInTheMachine where
  getAbilities (DevilInTheMachine a) = [needsAir a 1]

instance RunMessage DevilInTheMachine where
  runMessage msg a@(DevilInTheMachine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Search all in- and out-of-play areas for each copy of Still Behind You,
      -- shuffle them, the set-aside Star Spawn enemy, and the encounter discard
      -- pile into the encounter deck.
      stillBehindYou <- getSetAsideCardsMatching $ cardIs Treacheries.stillBehindYou
      starSpawns <- getSetAsideCardsMatching $ CardWithTrait StarSpawn
      randomStarSpawn <- maybe (pure []) (fmap pure . sample) (nonEmpty starSpawns)
      shuffleCardsIntoDeck Deck.EncounterDeck (stillBehindYou <> randomStarSpawn)
      shuffleEncounterDiscardBackIn

      -- Move each unengaged Persistent Construct to the Moving Platform,
      -- exhausted and unengaged.
      selectForMaybeM (locationIs Locations.movingPlatformObservationStation) \platform ->
        selectEach (enemyIs Enemies.persistentConstruct <> UnengagedEnemy) \construct -> do
          enemyMoveTo attrs construct platform
          disengageEnemyFromAll construct
          exhaustThis construct

      -- If The Inescapable is in play, heal 1 [per_investigator] damage from it
      -- and move it once toward the nearest investigator (via its hunter
      -- keyword; the adjacency-during-movement is handled in HasModifiersFor).
      whenJustM (selectOne $ enemyIs Enemies.theInescapable) \inescapable -> do
        healAmount <- perPlayer 1
        healDamage inescapable attrs healAmount
        push $ HunterMove inescapable

      advanceAgendaDeck attrs
      pure a
    _ -> DevilInTheMachine <$> liftRunMessage msg attrs
