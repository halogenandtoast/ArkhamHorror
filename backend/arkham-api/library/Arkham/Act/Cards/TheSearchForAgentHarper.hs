module Arkham.Act.Cards.TheSearchForAgentHarper (TheSearchForAgentHarper (..), theSearchForAgentHarper) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers
import Arkham.Story.Cards qualified as Stories

newtype TheSearchForAgentHarper = TheSearchForAgentHarper ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForAgentHarper :: ActCard TheSearchForAgentHarper
theSearchForAgentHarper = act (1, A) TheSearchForAgentHarper Cards.theSearchForAgentHarper Nothing

instance HasAbilities TheSearchForAgentHarper where
  getAbilities (TheSearchForAgentHarper a) =
    extend
      a
      [ groupLimit PerRound $ mkAbility a 1 $ FastAbility' GroupClueCostX [#parley]
      , mkAbility a 2 $ Objective $ freeReaction $ RoundEnds #when
      ]

circle :: (ReverseQueue m, ToJSON a) => CampaignLogKey -> a -> m ()
circle k x = do
  let x' = toJSON x
  recordSetReplace k (recorded x') (circled x')

instance RunMessage TheSearchForAgentHarper where
  runMessage msg a@(TheSearchForAgentHarper attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
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
              then doStep 1 msg
              else nothing

      circle PossibleSuspects (asSuspect kidnapper)
      circle PossibleHideouts (asHideout hideout)

      doStep 2 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      let n = toResultDefault (0 :: Int) attrs.meta
      pure . TheSearchForAgentHarper $ attrs & metaL .~ toJSON (n + 1)
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      case toResultDefault (0 :: Int) attrs.meta of
        0 -> eachInvestigator resign
        1 -> do
          lead <- getLead
          flipOverBy lead attrs =<< selectJust (storyIs Stories.findingAgentHarper.cardCode)
          doStep 3 msg'
        _ -> doStep 3 msg'
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      theRescue <- getSetAsideCard Acts.theRescue
      franticPursuit <- getSetAsideCard Agendas.franticPursuit
      push $ SetCurrentActDeck 1 [theRescue]
      push $ SetCurrentAgendaDeck 1 [franticPursuit]
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
      focusCards revealed \unfocus -> do
        chooseOneM iid do
          for_ (eachWithRest revealed) \(lead, rest') -> do
            targeting lead do
              push unfocus
              drawCard iid lead
              shuffleIntoLeadsDeck $ map toCard topOfEncounterDeck <> rest'
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheSearchForAgentHarper <$> liftRunMessage msg attrs
