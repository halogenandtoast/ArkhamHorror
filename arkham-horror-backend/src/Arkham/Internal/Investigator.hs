module Arkham.Internal.Investigator
  ( toInternalInvestigator
  , locationFor
  )
where

import Arkham.Constructors
import Arkham.Internal.Types
import Arkham.Types hiding (investigator)
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

locationFor :: ArkhamInvestigator -> ArkhamGameState -> ArkhamLocation
locationFor investigator' g =
  fromJustNote "the investigator appears to be nowhere"
    $ find (investigatorIsAtLocation investigator')
    $ HashMap.elems (g ^. locations)

investigatorIsAtLocation :: ArkhamInvestigator -> ArkhamLocation -> Bool
investigatorIsAtLocation investigator' = elem investigator' . alInvestigators

allInvestigators :: HashMap ArkhamCardCode ArkhamInvestigatorInternal
allInvestigators = HashMap.fromList
  [(ArkhamCardCode "01001", rolandBanks), (ArkhamCardCode "01002", daisyWalker)]

toInternalInvestigator :: ArkhamInvestigator -> ArkhamInvestigatorInternal
toInternalInvestigator ArkhamInvestigator {..} =
  fromJustNote "Missing internal investigator"
    $ HashMap.lookup aiCardCode allInvestigators

investigator :: ArkhamInvestigatorInternal
investigator = ArkhamInvestigatorInternal
  { investigatorElderSignToken = error "you must defined an elder sign token"
  , investigatorOnDefeatEnemy = id
  , investigatorAvailableActions = replicate 3 AnyAction
  }

rolandBanks :: ArkhamInvestigatorInternal
rolandBanks = investigator
  { investigatorElderSignToken = (token ElderSign)
    { tokenToResult = \state investigator' ->
      Modifier $ locationFor investigator' state ^. clues
    }
  }

daisyWalker :: ArkhamInvestigatorInternal
daisyWalker = investigator
