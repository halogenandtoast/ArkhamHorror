module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.TheSearchForAgentHarperV2 (theSearchForAgentHarperV2) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Acts
import Arkham.Act.Import.Lifted
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.Helpers
import Arkham.Story.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Stories
import Arkham.Trait (Trait (Hybrid, Suspect))

newtype TheSearchForAgentHarperV2 = TheSearchForAgentHarperV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForAgentHarperV2 :: ActCard TheSearchForAgentHarperV2
theSearchForAgentHarperV2 = act (1, A) TheSearchForAgentHarperV2 Cards.theSearchForAgentHarperV2 Nothing

instance HasAbilities TheSearchForAgentHarperV2 where
  getAbilities (TheSearchForAgentHarperV2 a) =
    extend
      a
      [ groupLimit PerRound $ mkAbility a 1 $ FastAbility' (GroupClueCostX Anywhere) #parley
      , mkAbility a 2 $ Objective $ freeReaction $ RoundEnds #when
      ]

circle :: (ReverseQueue m, ToJSON a, IsCampaignLogKey k) => k -> a -> m ()
circle k x = do
  let x' = toJSON x
  recordSetReplace (toCampaignLogKey k) (recorded x') (circled x')

instance RunMessage TheSearchForAgentHarperV2 where
  runMessage msg a@(TheSearchForAgentHarperV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Remove all Hybrid story allies you control from the game."
      selectEach (AssetWithTrait Hybrid <> AssetControlledBy Anyone) removeFromGame
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
      pure . TheSearchForAgentHarperV2 $ attrs & metaL .~ toJSON (n + 1)
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
      -- Roderick: "Activating the {fast} ability on the act costs 1 fewer clues (to a
      -- minimum of 1 clue)". The cost is an X paid in clues, so the discount is credited
      -- here, where clues paid become cards revealed.
      hasRoderick <- selectAny $ assetIs HBAssets.roderick
      let paid = if hasRoderick then clues + 1 else clues
      revealed <- take (min 3 $ paid `div` n) <$> getScenarioDeck LeadsDeck
      for_ revealed crossOutLead
      focusCards revealed do
        chooseOneM iid do
          for_ (eachWithRest revealed) \(lead, rest') -> do
            targeting lead do
              unfocusCards
              drawCard iid lead
              -- v2: the encounter card only joins the Leads deck when the card drawn was
              -- a location or a Suspect enemy (designer's note).
              let addsEncounterCard =
                    lead
                      `cardMatch` oneOf
                        [ CardWithType LocationType
                        , CardWithType EnemyType <> CardWithTrait Suspect
                        ]
              shuffleIntoLeadsDeck
                $ (if addsEncounterCard then map toCard topOfEncounterDeck else [])
                <> rest'
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheSearchForAgentHarperV2 <$> liftRunMessage msg attrs
