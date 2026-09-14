module Arkham.Agenda.Cards.TheDrownedCity.TheGrandVault.DevilInTheMachine (devilInTheMachine) where

import Arkham.Agenda.CardDefs.TheDrownedCity.TheGrandVault qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (struggleForAir, strugglesForAir)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheDrownedCity.AlienMachinery qualified as Enemies
import Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable qualified as Enemies
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMapM)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.CardDefs.TheDrownedCity.TheGrandVault qualified as Locations
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Projection
import Arkham.Trait (Trait (StarSpawn))
import Arkham.Treachery.CardDefs.TheDrownedCity.TheInescapable qualified as Treacheries

newtype DevilInTheMachine = DevilInTheMachine AgendaAttrs
  deriving anyclass IsAgenda
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
  getAbilities (DevilInTheMachine a) = [strugglesForAir a 1]

instance RunMessage DevilInTheMachine where
  runMessage msg a@(DevilInTheMachine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "Search all in- and out-of-play areas for each copy of the Still Behind
      You treachery." Unlike Bowels of the City, which only takes the set-aside
      copies, this one has to sweep the victory display too: Still Behind You adds
      ITSELF there whenever its revelation test is failed, and its difficulty
      scales with the copies sitting in it. The discard pile is folded in by
      'shuffleEncounterDiscardBackIn' below, and copies already in the encounter
      deck stay put. 'shuffleCardsIntoDeck' filters the cards out of whatever area
      they came from, so the victory display is emptied of them as they go back in.
      -}
      stillBehindYou <-
        (<>)
          <$> getSetAsideCardsMatching (cardIs Treacheries.stillBehindYou)
          <*> select (VictoryDisplayCardMatch $ basic $ cardIs Treacheries.stillBehindYou)
      -- The Inescapable is a Star Spawn but is never one of the set-aside random Star
      -- Spawn enemies (see Bowels of the City).
      starSpawns <-
        getSetAsideCardsMatching
          $ CardWithTrait StarSpawn
          <> not_ (cardIs Enemies.theInescapable)
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
