module Arkham.Agenda.Cards.TheDoomOfArkham (theDoomOfArkham) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Location.Types (Field (LocationName))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Name (nameTitle)
import Arkham.Projection
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Trait (Trait (Ruined))

newtype TheDoomOfArkham = TheDoomOfArkham AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfArkham :: AgendaCard TheDoomOfArkham
theDoomOfArkham = agenda (1, A) TheDoomOfArkham Cards.theDoomOfArkham (Static 3)

instance HasAbilities TheDoomOfArkham where
  getAbilities (TheDoomOfArkham a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #enemy
    , restricted a 2 (exists $ InvestigatorAt FullyFloodedLocation) $ forced $ RoundEnds #when
    ]

instance RunMessage TheDoomOfArkham where
  runMessage msg a@(TheDoomOfArkham attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withCthulhuLocation increaseFloodLevel
      lead <- getLead
      drawCthulhuDeckCard lead (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt FullyFloodedLocation) \iid ->
        assignDamageOrHorror iid (attrs.ability 2) 1 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "Move Cthulhu to the nearest fully flooded location that is not [[Ruined]]
      and decrease that location's flood level. Discard each clue and attachment at
      that location, and swap it with its set-aside [[Ruined]] version, unrevealed
      side faceup."

      "The nearest" can be a tie, so the candidates are gathered and chosen between
      rather than picked arbitrarily. Everything downstream — the swap itself and
      then the Ruined tally — has to wait until that choice is answered, so the rest
      of the agenda hangs off the chosen target. -}
      let drownable = FullyFloodedLocation <> not_ (LocationWithTrait Ruined)
      candidates <-
        getCthulhuLocation >>= maybe (pure []) \cthulhuLocation ->
          select
            $ FirstLocation
              [ LocationWithId cthulhuLocation <> FullyFloodedLocation
              , NearestLocationToLocation cthulhuLocation drownable
              ]

      if null candidates
        then do
          -- "If no location was swapped by this effect, draw the top card of the
          -- Cthulhu deck."
          lead <- getLead
          drawCthulhuDeckCard lead attrs
          doStep 1 msg
        else do
          lead <- getLead
          chooseOrRunOneM lead $ scenarioI18n do
            questionLabeled' "chooseLocationToRuin"
            targets candidates \lid -> push $ ForTarget (toTarget lid) msg
      pure a
    ForTarget (LocationTarget lid) advance@(AdvanceAgenda (isSide B attrs -> True)) -> do
      {- Cthulhu's own 'EnemyMove' handler drags the board's facets along, so the
      three of them arrive with him. -}
      selectEach (enemyIs Enemies.cthulhuAncientEvil) \eid -> push $ EnemyMove eid lid
      decreaseFloodLevel lid
      removeAllClues attrs lid
      selectEach (treacheryAt lid) (toDiscard attrs)
      selectEach (assetAt lid) (toDiscard attrs)

      {- The Ruined version shares its title with the location it replaces — which is
      what makes this work across the two-variant Downtown and Southside cards.
      'Swap' is what keeps the map intact: it carries position, label, and
      connections over, where 'DefaultReplace' would drop them. -}
      title <- fieldMap LocationName nameTitle lid
      ruined <- getSetAsideCardsMatching (#location <> CardWithTrait Ruined <> CardWithTitle title)
      case ruined of
        -- No Ruined version left to swap in, so nothing was swapped after all.
        [] -> do
          lead <- getLead
          drawCthulhuDeckCard lead attrs
        (card : _) -> do
          push $ ReplaceLocation lid card Swap
          {- 'Swap' carries `revealed` across, so the Ruined card would come in
          already revealed; the unreveal is what makes it "unrevealed side faceup".
          Anyone still standing there then reveals it the same way entering would,
          firing the usual reveal windows rather than silently flipping it. -}
          push $ UnrevealLocation lid
          selectOne (investigatorAt lid) >>= traverse_ \iid ->
            push $ Msg.RevealLocation (Just iid) lid

      -- The Ruined count has to be read after the swap lands, so the rest waits a step.
      doStep 1 advance
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      ruinedCount <- selectCount $ LocationWithTrait Ruined
      if ruinedCount >= 7
        then
          -- "If there are 7 [[Ruined]] locations in play, each investigator is killed."
          selectEach UneliminatedInvestigator (kill attrs)
        else do
          -- "Otherwise, place clues on each revealed location without Victory X up
          -- to its clue value."
          selectEach (RevealedLocation <> not_ LocationWithVictory) (placeCluesUpToClueValue attrs)

          {- "Shuffle 1 set-aside [[Star Spawn]] enemy into the encounter deck, along
          with the encounter discard pile." One message for both, so the Star Spawn
          cannot be shuffled into an empty deck and left sitting on top. -}
          starSpawn <- getSetAsideCardsMatching $ CardFromEncounterSet Set.StarSpawn
          for_ (take 1 starSpawn) \card -> do
            obtainCard card
            discards <- map toCard <$> getEncounterDiscard RegularEncounterDeck
            shuffleCardsIntoDeck Deck.EncounterDeck (card : discards)

      -- "Flip this agenda."
      revertAgenda attrs
      pure a
    _ -> TheDoomOfArkham <$> liftRunMessage msg attrs
