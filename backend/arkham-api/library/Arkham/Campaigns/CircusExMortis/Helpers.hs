module Arkham.Campaigns.CircusExMortis.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign (getCompletedSteps, getOwner)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message (ShuffleIn (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "circusExMortis" a

-- * Moon tokens

-- | Moon tokens sealed on an investigator's investigator card (guide p1).
getSealedMoonTokens :: (HasGame m, Tracing m) => InvestigatorId -> m [ChaosToken]
getSealedMoonTokens iid =
  filter ((== MoonToken) . (.face)) <$> field InvestigatorSealedChaosTokens iid

-- | Release a sealed moon token: it returns to the chaos bag.
releaseMoonToken :: ReverseQueue m => ChaosToken -> m ()
releaseMoonToken = unsealChaosToken

-- * Story-asset versions (Amalthea Weaver / De Cultus Bestiae)

-- | Every printing of Amalthea Weaver, base version first.
amaltheaWeaverVersions :: [CardDef]
amaltheaWeaverVersions =
  [ Assets.amaltheaWeaverCircusFortuneTellerCircusExMortis
  , Assets.amaltheaWeaverAspirantOfCourageCircusExMortis
  , Assets.amaltheaWeaverAspirantOfWisdomCircusExMortis
  , Assets.amaltheaWeaverOracleOfPurityCircusExMortis
  , Assets.amaltheaWeaverOracleOfResolveCircusExMortis
  , Assets.amaltheaWeaverOracleOfEnlightenmentCircusExMortis
  , Assets.amaltheaWeaverOracleOfMysteryCircusExMortis
  ]

-- | Every printing of De Cultus Bestiae, base version first.
deCultusBestiaeVersions :: [CardDef]
deCultusBestiaeVersions =
  [ Assets.deCultusBestiaeForgottenWorkOfApuleiusCircusExMortis
  , Assets.deCultusBestiaeInterpretationOfConvictionCircusExMortis
  , Assets.deCultusBestiaeInterpretationOfObsessionCircusExMortis
  , Assets.deCultusBestiaeProphecyOfTheBeyondCircusExMortis
  , Assets.deCultusBestiaeProphecyOfTheEternalCircusExMortis
  , Assets.deCultusBestiaeProphecyOfTheHordeCircusExMortis
  , Assets.deCultusBestiaeProphecyOfTheBehemothCircusExMortis
  ]

-- | Find the owner and current version of a versioned story asset.
findVersionOwner
  :: (HasGame m, Tracing m) => [CardDef] -> m (Maybe (InvestigatorId, CardDef))
findVersionOwner defs =
  listToMaybe . catMaybes <$> for defs \def -> fmap (,def) <$> getOwner def

getAmaltheaWeaverOwner :: (HasGame m, Tracing m) => m (Maybe (InvestigatorId, CardDef))
getAmaltheaWeaverOwner = findVersionOwner amaltheaWeaverVersions

getDeCultusBestiaeOwner :: (HasGame m, Tracing m) => m (Maybe (InvestigatorId, CardDef))
getDeCultusBestiaeOwner = findVersionOwner deCultusBestiaeVersions

{- | Swap a versioned campaign story card for its next version in the same
investigator's deck (Relic of Ages pattern: remove the old def, add the new
one without counting toward deck size). No-op when nobody owns the old
version.
-}
swapCampaignCard :: ReverseQueue m => CardDef -> CardDef -> m ()
swapCampaignCard old new =
  getOwner old >>= traverse_ \iid -> do
    removeCampaignCard old
    addCampaignCardToDeck iid DoNotShuffleIn new

-- * Curse of the Rougarou side story

{- | The guide offers Curse of the Rougarou between Harm's Way and All Points
West; All Points West reads its Back on Track intro when the side story was
the most recently completed scenario. Completion itself is recorded by the
official scenario's resolutions (the TheRougarou* campaign log keys).
-}
playedCurseOfTheRougarouEnRoute :: (HasGame m, Tracing m) => m Bool
playedCurseOfTheRougarouEnRoute = do
  steps <- getCompletedSteps
  -- completed steps are stored most-recent-first
  pure $ case mapMaybe (.scenario) steps of
    (sid : _) -> sid == curseOfTheRougarouId
    _ -> False

curseOfTheRougarouId :: ScenarioId
curseOfTheRougarouId = "81001"
