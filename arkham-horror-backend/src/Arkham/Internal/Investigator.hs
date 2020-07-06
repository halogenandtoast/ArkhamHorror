module Arkham.Internal.Investigator
  ( toInternalInvestigator
  )
where

import Arkham.Constructors
import Arkham.Internal.Location
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Investigator
import Arkham.Types.Player
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

allInvestigators :: HashMap ArkhamCardCode ArkhamInvestigatorInternal
allInvestigators =
  HashMap.fromList [("01001", rolandBanks), ("01002", daisyWalker)]

toInternalInvestigator :: ArkhamPlayer -> ArkhamInvestigatorInternal
toInternalInvestigator p =
  fromJustNote "Missing internal investigator"
    $ HashMap.lookup (aiCardCode $ _investigator p) allInvestigators

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
