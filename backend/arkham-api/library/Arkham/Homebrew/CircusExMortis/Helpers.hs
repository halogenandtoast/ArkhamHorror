module Arkham.Homebrew.CircusExMortis.Helpers where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Campaign (getCompletedSteps, getOwner)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (ShuffleIn (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "circusExMortis" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

-- * Moon tokens

-- | Moon tokens sealed on an investigator's investigator card (guide p1).
getSealedMoonTokens :: HasGame m => InvestigatorId -> m [ChaosToken]
getSealedMoonTokens iid =
  filter ((== MoonToken) . (.face)) <$> field InvestigatorSealedChaosTokens iid

moonToken :: ChaosTokenMatcher
moonToken = ChaosTokenFaceIs MoonToken

{- | The ☾ token's printed modifier is 0, but 'NoModifier' is inert: effects that
reduce a token's modifier (Primordial Evils) would slide off it.
-}
moonTokenValue :: ChaosTokenValue
moonTokenValue = ChaosTokenValue MoonToken (NegativeModifier 0)

hasSealedMoonToken :: InvestigatorMatcher
hasSealedMoonToken = InvestigatorWithSealedChaosToken moonToken

-- | "Search the chaos bag for a ☾ token and seal it on your investigator card."
sealMoonTokenOn :: ReverseQueue m => InvestigatorId -> m ()
sealMoonTokenOn iid = selectOne moonToken >>= traverse_ (sealChaosToken iid iid)

-- | Release a sealed moon token: it returns to the chaos bag.
releaseMoonToken :: ReverseQueue m => ChaosToken -> m ()
releaseMoonToken = unsealChaosToken

{- | The "release a ☾ token sealed on your investigator card" ability shared by
'Smoke and Mirrors' and 'Out and Away'.
-}
releaseAMoonToken :: ReverseQueue m => InvestigatorId -> m ()
releaseAMoonToken iid = do
  moons <- getSealedMoonTokens iid
  chooseOneM iid $ for_ moons \token ->
    targeting (ChaosTokenTarget token) $ releaseMoonToken token

-- * One Night Only

{- | Each 'Rats in a Cage' hides the Illusory Locus at a different location and
permanently adds a different chaos token when it advances. Resolution 1 reads
the token back off whichever variant was chosen for the scenario.
-}
ratsInACageVariants :: NonEmpty (CardDef, (CardDef, ChaosTokenFace))
ratsInACageVariants =
  (Acts.ratsInACage_005, (Locations.animalCages, Tablet))
    :| [ (Acts.ratsInACage_006, (Locations.carousel, Tablet))
       , (Acts.ratsInACage_007, (Locations.gamesGallery, Cultist))
       , (Acts.ratsInACage_008, (Locations.performerTrailers, Cultist))
       ]

lookupRatsInACage :: CardDef -> Maybe (CardDef, ChaosTokenFace)
lookupRatsInACage def = lookup def (toList ratsInACageVariants)

-- * Story-asset versions (Amalthea Weaver / De Cultus Bestiae)

-- | Every printing of Amalthea Weaver, base version first.
amaltheaWeaverVersions :: [CardDef]
amaltheaWeaverVersions =
  [ Assets.amaltheaWeaverCircusFortuneTeller
  , Assets.amaltheaWeaverAspirantOfCourage
  , Assets.amaltheaWeaverAspirantOfWisdom
  , Assets.amaltheaWeaverOracleOfPurity
  , Assets.amaltheaWeaverOracleOfResolve
  , Assets.amaltheaWeaverOracleOfEnlightenment
  , Assets.amaltheaWeaverOracleOfMystery
  ]

-- | Every printing of De Cultus Bestiae, base version first.
deCultusBestiaeVersions :: [CardDef]
deCultusBestiaeVersions =
  [ Assets.deCultusBestiaeForgottenWorkOfApuleius
  , Assets.deCultusBestiaeInterpretationOfConviction
  , Assets.deCultusBestiaeInterpretationOfObsession
  , Assets.deCultusBestiaeProphecyOfTheBeyond
  , Assets.deCultusBestiaeProphecyOfTheEternal
  , Assets.deCultusBestiaeProphecyOfTheHorde
  , Assets.deCultusBestiaeProphecyOfTheBehemoth
  ]

-- | Find the owner and current version of a versioned story asset.
findVersionOwner
  :: HasGame m => [CardDef] -> m (Maybe (InvestigatorId, CardDef))
findVersionOwner defs =
  listToMaybe . catMaybes <$> for defs \def -> fmap (,def) <$> getOwner def

getAmaltheaWeaverOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
getAmaltheaWeaverOwner = findVersionOwner amaltheaWeaverVersions

getDeCultusBestiaeOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
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
playedCurseOfTheRougarouEnRoute :: HasGame m => m Bool
playedCurseOfTheRougarouEnRoute = do
  steps <- getCompletedSteps
  -- completed steps are stored most-recent-first
  pure $ case mapMaybe (.scenario) steps of
    (sid : _) -> sid == curseOfTheRougarouId
    _ -> False

curseOfTheRougarouId :: ScenarioId
curseOfTheRougarouId = "81001"
