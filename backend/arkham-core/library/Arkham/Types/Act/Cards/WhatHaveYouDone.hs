module Arkham.Types.Act.Cards.WhatHaveYouDone where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance ActRunner env => RunMessage env WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        (chooseOne
          leadInvestigatorId
          [ Label
            "It was never much of a home. Burn it down! (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "This \"hell-pit\" is my home! No way we are burning it! (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
      pure $ WhatHaveYouDone $ attrs & sequenceL .~ Act 3 B
    EnemyDefeated _ _ _ "01116" _ _ ->
      a <$ push (AdvanceAct actId $ toSource attrs)
    _ -> WhatHaveYouDone <$> runMessage msg attrs
