module Arkham.Internal.ChaosToken where

import Arkham.Internal.Investigator
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Util
import ClassyPrelude
import Lens.Micro

modifier :: Int -> (a -> b -> ArkhamChaosTokenResult)
modifier = const . const . Modifier

numberToken :: Int -> ArkhamChaosTokenInternal
numberToken n = token { tokenToResult = modifier n }

plusOneToken :: ArkhamChaosTokenInternal
plusOneToken = numberToken 1

zeroToken :: ArkhamChaosTokenInternal
zeroToken = token

minusOneToken :: ArkhamChaosTokenInternal
minusOneToken = numberToken (-1)

minusTwoToken :: ArkhamChaosTokenInternal
minusTwoToken = numberToken (-2)

minusThreeToken :: ArkhamChaosTokenInternal
minusThreeToken = numberToken (-3)

minusFourToken :: ArkhamChaosTokenInternal
minusFourToken = numberToken (-4)

minusFiveToken :: ArkhamChaosTokenInternal
minusFiveToken = numberToken (-5)

minusSixToken :: ArkhamChaosTokenInternal
minusSixToken = numberToken (-6)

minusSevenToken :: ArkhamChaosTokenInternal
minusSevenToken = numberToken (-7)

minusEightToken :: ArkhamChaosTokenInternal
minusEightToken = numberToken (-7)

autoFailToken :: ArkhamChaosTokenInternal
autoFailToken = token { tokenToResult = const (const Failure) }

playerElderSignToken :: ArkhamGameState -> ArkhamChaosTokenInternal
playerElderSignToken g = investigatorElderSignToken internalInvestigator
 where
  internalInvestigator = toInternalInvestigator arkhamInvestigator
  arkhamInvestigator = g ^. player . investigator

elderSignToken :: ArkhamChaosTokenInternal
elderSignToken = ArkhamChaosTokenInternal
  { tokenToResult = tokenToResult =<< playerElderSignToken
  , tokenOnFail = tokenOnFail =<< playerElderSignToken
  , tokenOnSuccess = tokenOnSuccess =<< playerElderSignToken
  , tokenOnReveal = tokenOnReveal =<< playerElderSignToken
  }
