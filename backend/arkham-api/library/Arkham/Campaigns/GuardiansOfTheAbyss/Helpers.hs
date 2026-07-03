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
import Arkham.Helpers.Scenario (scenarioField, standaloneI18n)
import Arkham.I18n ()
import Arkham.Id
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (
  Message (
    AddCampaignModifiersForAll,
    RemoveCampaignModifiersForAll,
    ScenarioCountDecrementBy,
    ScenarioCountIncrementBy
  ),
  pattern InvestigatorKilled,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Modifier (ModifierType (CannotPlay, CannotPutIntoPlay))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioCardsUnderScenarioReference))
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Tracing
import Arkham.Trait (Trait (Cultist))

campaignI18n :: (HasI18n => a) -> a
campaignI18n = standaloneI18n "guardiansOfTheAbyss"

getStrengthOfTheAbyss :: (HasGame m, Tracing m) => m Int
getStrengthOfTheAbyss = scenarioCount StrengthOfTheAbyss

addStrengthOfTheAbyss :: ReverseQueue m => Int -> m ()
addStrengthOfTheAbyss n = push $ ScenarioCountIncrementBy StrengthOfTheAbyss n

removeStrengthOfTheAbyss :: ReverseQueue m => Int -> m ()
removeStrengthOfTheAbyss n = push $ ScenarioCountDecrementBy StrengthOfTheAbyss n

{- | An investigator or unique Ally asset is /"taken by the abyss."/ First add
1 strength to the abyss, then record the card in the campaign log if it is
unique.
-}
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
    -- For the remainder of the campaign, no one may play copies of a unique
    -- Ally taken by the abyss. Stored campaign-wide so it persists across
    -- scenarios regardless of which campaign these are played in.
    push $ AddCampaignModifiersForAll [CannotPlay (cardIs card), CannotPutIntoPlay (cardIs card)]

brotherhoodEnemies :: [CardDef]
brotherhoodEnemies =
  [ Enemies.drLaylaElMasri
  , Enemies.drWentworthMoore
  , Enemies.nadiaNimr
  , Enemies.farid
  , Enemies.nassor
  , Enemies.professorNathanielTaylor
  ]

{- | The i18n flavor key holding the benefit text on each Evidence story card,
keyed by the story-side card code.
-}
evidenceBenefitKey :: CardCode -> Maybe Text
evidenceBenefitKey = \case
  "83031b" -> Just "theTranslatorsEvidence.benefit"
  "83032b" -> Just "theSupplicantsEvidence.benefit"
  "83033b" -> Just "thePriestesssEvidence.benefit"
  "83034b" -> Just "theSalesmansEvidence.benefit"
  "83035b" -> Just "theAssassinsEvidence.benefit"
  "83036b" -> Just "theProfessorsEvidence.benefit"
  _ -> Nothing

{- | The task an investigator must complete before the indicated Evidence
story card's benefit can be read.
-}
evidenceTask :: CardCode -> Maybe ScenarioLogKey
evidenceTask = \case
  "83031b" -> Just DiscoveredAnAncientTablet
  "83032b" -> Just SabotagedTheTrain
  "83033b" -> Just FoundADoorMarkedWithBlood
  "83034b" -> Just BrokenIntoADesertedTemple
  "83035b" -> Nothing
  "83036b" -> Just BoughtAnOddTrinket
  _ -> Nothing

{- | Read the indicated Evidence card's benefit: its card image alongside the
benefit text (card on the left, text on the right), with the instruction to
remove 1 strength from the abyss as a plain text entry, then remove it.
-}
readEvidence :: ReverseQueue m => CardCode -> m ()
readEvidence code = for_ (evidenceBenefitKey code) \key -> do
  campaignI18n $ flavor $ cols do
    img code
    compose do
      p key
      p.basic "removeStrengthFromTheAbyss"
  removeStrengthOfTheAbyss 1

{- | Each unique [[Cultist]] enemy that was in play or beneath the scenario
reference card when the game ended.
-}
recordBrotherhoodAgentsWhoEscaped :: ReverseQueue m => m ()
recordBrotherhoodAgentsWhoEscaped = do
  inPlay <- selectField EnemyCardCode (EnemyWithTrait Cultist <> UniqueEnemy)
  underneath <- scenarioField ScenarioCardsUnderScenarioReference
  let escaped = inPlay <> [toCardCode c | c <- underneath, cdUnique (toCardDef c)]
  unless (null escaped) $ recordSetInsert BrotherhoodAgentsWhoEscaped escaped

-- | Cross off all /"(card name) was taken by the abyss"/ notes.
crossOutTakenByTheAbyss :: ReverseQueue m => m ()
crossOutTakenByTheAbyss = do
  taken <- recordedCardCodes <$> getRecordSet WasTakenByTheAbyss
  unless (null taken) do
    crossOutRecordSetEntries WasTakenByTheAbyss taken
    -- The allies are no longer "taken by the abyss", so lift the campaign-wide
    -- restriction on playing copies of them.
    push $ RemoveCampaignModifiersForAll [CannotPlay (CardWithCardCode code) | code <- taken]
    push $ RemoveCampaignModifiersForAll [CannotPutIntoPlay (CardWithCardCode code) | code <- taken]

{- | An investigator who has been /taken by the abyss/ is treated as if they
were killed for the remainder of the campaign.
-}
killRemainingTakenByTheAbyss :: (ReverseQueue m, Sourceable source) => source -> m ()
killRemainingTakenByTheAbyss source = do
  taken <- recordedCardCodes <$> getRecordSet WasTakenByTheAbyss
  eachInvestigator \iid -> do
    when (unInvestigatorId iid `elem` taken) do
      push $ InvestigatorKilled (toSource source) iid
