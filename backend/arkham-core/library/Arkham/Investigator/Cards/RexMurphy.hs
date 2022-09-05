module Arkham.Investigator.Cards.RexMurphy
  ( RexMurphy(..)
  , rexMurphy
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message hiding (PassSkillTest)
import Arkham.Timing qualified as Timing

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexMurphy :: InvestigatorCard RexMurphy
rexMurphy = investigator
  RexMurphy
  Cards.rexMurphy
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }

instance HasAbilities RexMurphy where
  getAbilities (RexMurphy x) =
    [ restrictedAbility
          x
          1
          (OnLocation LocationWithAnyClues <> CanDiscoverCluesAt YourLocation)
        $ ReactionAbility
            (SkillTestResult
              Timing.After
              You
              (WhileInvestigating Anywhere)
              (SuccessResult $ AtLeast $ Static 2)
            )
            Free
    ]

instance HasTokenValue RexMurphy where
  getTokenValue iid ElderSign (RexMurphy attrs) | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage RexMurphy where
  runMessage msg i@(RexMurphy attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> i <$ push
      (InvestigatorDiscoverClues
        (toId attrs)
        (investigatorLocation attrs)
        1
        Nothing
      )
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> i <$ push
      (chooseOne
        iid
        [ Label
          "Automatically fail to draw 3"
          [FailSkillTest, DrawCards iid 3 False]
        , Label "Resolve normally" []
        ]
      )
    _ -> RexMurphy <$> runMessage msg attrs
