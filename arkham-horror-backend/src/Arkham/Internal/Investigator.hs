module Arkham.Internal.Investigator
  ( toInternalInvestigator
  )
where

import Arkham.Internal.Types
import Arkham.Types hiding (investigator)
import Arkham.Util
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

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
  { investigatorElderSignToken = token
    { tokenToResult = \state investigator' ->
      Modifier $ locationFor investigator' state ^. clues
    }
  }

daisyWalker :: ArkhamInvestigatorInternal
daisyWalker = investigator
