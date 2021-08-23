module Arkham.Types.Act.Cards.WhatHaveYouDone where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance HasAbilities env WhatHaveYouDone where
  getAbilities _ _ (WhatHaveYouDone x) = pure
    [ mkAbility x 1
      $ Objective
      $ ForcedAbility
      $ EnemyDefeated Timing.When Anyone
      $ enemyIs Cards.ghoulPriest
    ]

instance ActRunner env => RunMessage env WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct actId $ InvestigatorSource iid)
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
    _ -> WhatHaveYouDone <$> runMessage msg attrs
