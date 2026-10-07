module Arkham.Act.Cards.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.TheSearchForAgentHarper (theSearchForAgentHarper) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Acts
import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.Helpers
import Arkham.Story.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Stories

newtype TheSearchForAgentHarper = TheSearchForAgentHarper ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForAgentHarper :: ActCard TheSearchForAgentHarper
theSearchForAgentHarper = act (1, A) TheSearchForAgentHarper Cards.theSearchForAgentHarper Nothing

instance HasAbilities TheSearchForAgentHarper where
  getAbilities (TheSearchForAgentHarper a) =
    extend
      a
      [ groupLimit PerRound $ mkAbility a 1 $ FastAbility' (GroupClueCostX Anywhere) #parley
      , mkAbility a 2 $ Objective $ freeReaction $ RoundEnds #when
      ]

circle :: (ReverseQueue m, ToJSON a, IsCampaignLogKey k) => k -> a -> m ()
circle k x = do
  let x' = toJSON x
  recordSetReplace (toCampaignLogKey k) (recorded x') (circled x')

instance RunMessage TheSearchForAgentHarper where
  runMessage msg a@(TheSearchForAgentHarper attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      {- "Scenario Interlude: The Accusation". Read before the accusation, since it is
      what tells the players how to make one; the reveal below is read once they have. -}
      scenarioI18n $ scope "interlude" $ flavor do
        h "title"
        p "instructions"
      lead <- getLead
      possibleSuspects <- getPossibleSuspects
      kidnapper <- getKidnapper

      chooseOneM lead do
        for_ possibleSuspects \suspect -> do
          cardLabeled suspect do
            if suspect == toCardDef kidnapper
              then doStep 1 msg
              else nothing

      possibleHideouts <- getPossibleHideouts
      hideout <- getHideout

      chooseOneM lead do
        for_ possibleHideouts \possibleHideout -> do
          cardLabeled possibleHideout do
            if possibleHideout == toCardDef hideout
              then doStep 4 msg
              else nothing

      circle PossibleSuspects (asSuspect kidnapper)
      circle PossibleHideouts (asHideout hideout)

      doStep 2 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> matched "suspect"
    DoStep 4 (AdvanceAct (isSide B attrs -> True) _ _) -> matched "hideout"
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- The cards from beneath Finding Agent Harper, shown now that the accusation is in.
      kidnapper <- getKidnapper
      hideout <- getHideout
      let correct = toResultDefault ([] :: [Text]) attrs.meta
      {- The whole second half of the interlude, with the branch the accusation actually
      took ticked and the other two crossed. The set-up list is only shown when the game
      goes on: on a miss everyone resigns and none of it happens. -}
      scenarioI18n $ scope "interlude" $ flavor do
        h "title"
        cols do
          smallImgValidate ("suspect" `elem` correct) kidnapper
          smallImgValidate ("hideout" `elem` correct) hideout
        ul do
          li.nested "accusationMade" do
            li "reveal"
            li.validate (null correct) "neither"
            li.validate (length correct == 1) "one"
            li.validate (length correct == 2) "both"
          when (notNull correct) $ li.nested "finalActAndAgenda" do
            li "advanceAct"
            li "advanceAgenda"
            li "putHideoutIntoPlay"
            li "spawnKidnapper"
            li "removeLeadsDeck"
            li "readyToProceed"
      case length correct of
        0 -> eachInvestigator resign
        1 -> do
          lead <- getLead
          flipOverBy lead attrs =<< selectJust (storyIs Stories.findingAgentHarper.cardCode)
          doStep 3 msg'
        _ -> doStep 3 msg'
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      theRescue <- getSetAsideCard Acts.theRescue
      -- franticPursuit <- getSetAsideCard Agendas.franticPursuit
      push $ SetCurrentActDeck 1 [theRescue]
      push $ AdvanceToAgenda 1 Agendas.franticPursuit Agendas.A (toSource attrs)
      hideout <- placeLocation =<< getHideout
      placeClues attrs hideout =<< perPlayer 1
      elinaHarper <- getSetAsideCard Assets.elinaHarperKnowsTooMuch
      placeUnderneath hideout [elinaHarper]
      kidnapper <- getKidnapper
      kidnapperEnemy <- createEnemyAt kidnapper hideout
      gameModifier ScenarioSource kidnapperEnemy (ScenarioModifier "kidnapper")
      getScenarioDeck LeadsDeck >>= traverse_ obtainCard
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> clues) -> do
      topOfEncounterDeck <- take 1 . unDeck <$> getEncounterDeck
      n <- perPlayer 1
      revealed <- take (min 3 $ clues `div` n) <$> getScenarioDeck LeadsDeck
      for_ revealed crossOutLead
      focusCards revealed do
        chooseOneM iid do
          for_ (eachWithRest revealed) \(lead, rest') -> do
            targeting lead do
              unfocusCards
              drawCard iid lead
              shuffleIntoLeadsDeck $ map toCard topOfEncounterDeck <> rest'
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheSearchForAgentHarper <$> liftRunMessage msg attrs
   where
    -- Which of the two guesses matched, not just how many: the reveal marks each card.
    matched k = do
      let ks = toResultDefault ([] :: [Text]) attrs.meta
      pure . TheSearchForAgentHarper $ attrs & metaL .~ toJSON (k : ks)
