module Arkham.Types.Act.Cards.WhatHaveYouDone where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Timing qualified as Timing

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone x) =
    [ mkAbility x 1
      $ Objective
      $ ForcedAbility
      $ EnemyDefeated Timing.After Anyone
      $ enemyIs Cards.ghoulPriest
    ]

instance ActRunner env => RunMessage env WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) $ InvestigatorSource iid)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
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
    _ -> WhatHaveYouDone <$> runMessage msg attrs
