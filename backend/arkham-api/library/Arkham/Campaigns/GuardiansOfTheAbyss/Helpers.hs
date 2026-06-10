module Arkham.Campaigns.GuardiansOfTheAbyss.Helpers where

import Arkham.Asset.Types (Field (AssetCard))
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCardCode))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log hiding (crossOutRecordSetEntries, recordSetInsert, recordSetReplace)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Scenario (scenarioField, standaloneI18n)
import Arkham.I18n ()
import Arkham.Id
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (pattern InvestigatorKilled, Message (ScenarioCountDecrementBy, ScenarioCountIncrementBy), toMessage)
import Arkham.Message.Story (StoryMessage (RemoveStory))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioCardsUnderScenarioReference))
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Cultist))
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n = standaloneI18n "guardiansOfTheAbyss"

getStrengthOfTheAbyss :: (HasGame m, Tracing m) => m Int
getStrengthOfTheAbyss = scenarioCount StrengthOfTheAbyss

addStrengthOfTheAbyss :: ReverseQueue m => Int -> m ()
addStrengthOfTheAbyss n = push $ ScenarioCountIncrementBy StrengthOfTheAbyss n

removeStrengthOfTheAbyss :: ReverseQueue m => Int -> m ()
removeStrengthOfTheAbyss n = push $ ScenarioCountDecrementBy StrengthOfTheAbyss n

-- | An investigator or unique Ally asset is /"taken by the abyss."/ First add
-- 1 strength to the abyss, then record the card in the campaign log if it is
-- unique.
investigatorTakenByTheAbyss :: ReverseQueue m => InvestigatorId -> m ()
investigatorTakenByTheAbyss iid = do
  addStrengthOfTheAbyss 1
  recordSetInsert WasTakenByTheAbyss [unInvestigatorId iid]

assetTakenByTheAbyss :: ReverseQueue m => AssetId -> m ()
assetTakenByTheAbyss aid = do
  addStrengthOfTheAbyss 1
  card <- field AssetCard aid
  when (cdUnique $ toCardDef card) do
    recordSetInsert WasTakenByTheAbyss [toCardCode card]

brotherhoodEnemies :: [CardDef]
brotherhoodEnemies =
  [ Enemies.drLaylaElMasri
  , Enemies.drWentworthMoore
  , Enemies.nadiaNimr
  , Enemies.farid
  , Enemies.nassor
  , Enemies.professorNathanielTaylor
  ]

-- | The story card on the reverse side of each [[Brotherhood]] enemy.
evidenceFor :: CardCode -> Maybe CardDef
evidenceFor = \case
  "83031a" -> Just Stories.theTranslatorsEvidence
  "83032a" -> Just Stories.theSupplicantsEvidence
  "83033a" -> Just Stories.thePriestesssEvidence
  "83034a" -> Just Stories.theSalesmansEvidence
  "83035a" -> Just Stories.theAssassinsEvidence
  "83036a" -> Just Stories.theProfessorsEvidence
  _ -> Nothing

-- | The task an investigator must complete before the indicated Evidence
-- story card's benefit can be read.
evidenceTask :: CardCode -> Maybe ScenarioLogKey
evidenceTask = \case
  "83031b" -> Just DiscoveredAnAncientTablet
  "83032b" -> Just SabotagedTheTrain
  "83033b" -> Just FoundADoorMarkedWithBlood
  "83034b" -> Just BrokenIntoADesertedTemple
  "83035b" -> Nothing
  "83036b" -> Just BoughtAnOddTrinket
  _ -> Nothing

-- | Read an Evidence story card's benefit, remove 1 strength from the abyss,
-- and remove the story from play.
resolveEvidence
  :: (ReverseQueue m, AsId story, IdOf story ~ StoryId) => story -> Text -> m ()
resolveEvidence s key = do
  campaignI18n $ flavor $ p key
  removeStrengthOfTheAbyss 1
  push $ toMessage $ RemoveStory (asId s)

-- | Each unique [[Cultist]] enemy that was in play or beneath the scenario
-- reference card when the game ended.
recordBrotherhoodAgentsWhoEscaped :: ReverseQueue m => m ()
recordBrotherhoodAgentsWhoEscaped = do
  inPlay <- selectField EnemyCardCode (InPlayEnemy $ EnemyWithTrait Cultist <> UniqueEnemy)
  underneath <- scenarioField ScenarioCardsUnderScenarioReference
  let escaped = inPlay <> [toCardCode c | c <- underneath, cdUnique (toCardDef c)]
  unless (null escaped) $ recordSetInsert BrotherhoodAgentsWhoEscaped escaped

-- | Cross off all /"(card name) was taken by the abyss"/ notes.
crossOutTakenByTheAbyss :: ReverseQueue m => m ()
crossOutTakenByTheAbyss = do
  taken <- recordedCardCodes <$> getRecordSet WasTakenByTheAbyss
  unless (null taken) $ crossOutRecordSetEntries WasTakenByTheAbyss taken

-- | An investigator who has been /taken by the abyss/ is treated as if they
-- were killed for the remainder of the campaign.
killRemainingTakenByTheAbyss :: (ReverseQueue m, Sourceable source) => source -> m ()
killRemainingTakenByTheAbyss source = do
  taken <- recordedCardCodes <$> getRecordSet WasTakenByTheAbyss
  investigators <- allInvestigators
  for_ investigators \iid ->
    when (unInvestigatorId iid `elem` taken) do
      push $ InvestigatorKilled (toSource source) iid
