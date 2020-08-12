{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.WhatHaveYouDone where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype WhatHaveYouDone = WhatHaveYouDone Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatHaveYouDone :: WhatHaveYouDone
whatHaveYouDone =
  WhatHaveYouDone $ baseAttrs "01110" "What Have You Done?" "Act 3a"

instance (ActRunner env) => RunMessage env WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne
          [ Label
            "It was never much of a home. Burn it down! (-> R1)"
            [Resolution 1]
          , Label
            "This \"hell-pit\" is my home! No way we are burning it! (-> R2)"
            [Resolution 2]
          ]
        )
      pure $ WhatHaveYouDone $ attrs & sequence .~ "Act 3b" & flipped .~ True
    EnemyDefeated _ _ "01116" _ -> a <$ unshiftMessage (AdvanceAct actId)
    _ -> WhatHaveYouDone <$> runMessage msg attrs
