{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.ChaosBag where

import Arkham.ChaosBag.RevealStrategy (RevealStrategy)
import Arkham.ChaosBagStepState (ChaosBagStep)
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenFace)
import Arkham.Id
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy (RequestedChaosTokenStrategy)
import Arkham.Source (Source)
import Arkham.Target (Target)
import Data.Aeson.TH

-- | Messages dealing with chaos-bag draws, reveals, sealing-flow choices,
-- request/resolve cycles, and bag state mutation. Sealing of chaos tokens onto
-- specific entities lives in "Arkham.Message.Seal"; this module handles the
-- bag itself and the in-flight draws.
data ChaosBagMessage
  = DrawChaosToken_ InvestigatorId ChaosToken
  | ResolveChaosToken_ ChaosToken ChaosTokenFace InvestigatorId
  | TargetResolveChaosToken_ Target ChaosToken ChaosTokenFace InvestigatorId
  | RevealChaosToken_ Source InvestigatorId ChaosToken
  | SilentRevealChaosToken_ Source InvestigatorId ChaosToken
  | SwapChaosToken_ ChaosTokenFace ChaosTokenFace
  | RemoveChaosToken_ ChaosTokenFace
  | ResetTokenPool_
  | RequestChaosTokens_ Source (Maybe InvestigatorId) RevealStrategy RequestedChaosTokenStrategy
  | RequestedChaosTokens_ Source (Maybe InvestigatorId) [ChaosToken]
  | ReturnChaosTokens_ [ChaosToken]
  | ReturnChaosTokensToPool_ [ChaosToken]
  | RunBag_ Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunDrawFromBag_ Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | FinalizeRequestedChaosTokens_ Source (Maybe InvestigatorId)
  | NextChaosBagStep_ Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | ChooseChaosTokenGroups_ Source InvestigatorId ChaosBagStep
  | BeforeRevealChaosTokens_
  | AfterRevealChaosTokens_
  | ReplaceCurrentDraw_ Source InvestigatorId ChaosBagStep
  | ReplaceEntireDraw_ Source InvestigatorId ChaosBagStep
  | SetChaosBagChoice_ Source InvestigatorId ChaosBagStep
  | ResetChaosTokens_ Source
  | ChaosTokenSelected_ InvestigatorId Source ChaosToken
  | ChaosTokenIgnored_ InvestigatorId Source ChaosToken
  | ChaosTokenCanceled_ InvestigatorId Source ChaosToken
  | ForceChaosTokenDraw_ ChaosTokenFace
  | ForceChaosTokenDrawToken_ ChaosToken
  | SetChaosTokens_ [ChaosTokenFace]
  | SetChaosTokensForScenario_
  | ObtainChaosToken_ ChaosToken
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ChaosBagMessage)
