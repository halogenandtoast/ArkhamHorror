module Arkham.Types.Act.Cards.WhatHaveYouDone where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: WhatHaveYouDone
whatHaveYouDone =
  WhatHaveYouDone $ baseAttrs "01110" "What Have You Done?" (Act 3 A) Nothing

instance ActionRunner env => HasActions env WhatHaveYouDone where
  getActions i window (WhatHaveYouDone x) = getActions i window x

instance ActRunner env => RunMessage env WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
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
      a <$ unshiftMessage (AdvanceAct actId $ toSource attrs)
    _ -> WhatHaveYouDone <$> runMessage msg attrs
