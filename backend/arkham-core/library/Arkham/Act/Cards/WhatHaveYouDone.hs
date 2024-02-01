module Arkham.Act.Cards.WhatHaveYouDone where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone x) =
    [ mkAbility x 1
        $ Objective
        $ ForcedAbility
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Cards.ghoulPriest
    ]

instance RunMessage WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ advanceVia #other attrs iid
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      push
        $ chooseOne lead
        $ [ Label "It was never much of a home. Burn it down! (→ _R1_)" [R1]
          , Label "This \"hell-pit\" is my home! No way we are burning it! (→ _R2_)" [R2]
          ]
      pure a
    _ -> WhatHaveYouDone <$> runMessage msg attrs
