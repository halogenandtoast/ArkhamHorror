module Arkham.Types.Investigator.Cards.RexMurphy
  ( RexMurphy(..)
  , rexMurphy
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (PassSkillTest)
import Arkham.Types.Modifier
import qualified Arkham.Types.Timing as Timing

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

rexMurphy :: RexMurphy
rexMurphy = RexMurphy $ baseAttrs
  "02002"
  "Rex Murphy"
  Seeker
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }
  [Reporter]

instance HasAbilities RexMurphy where
  getAbilities (RexMurphy x) =
    [ restrictedAbility
          x
          1
          (Self
          <> LocationExists (YourLocation <> LocationWithAnyClues)
          <> Negate (SelfHasModifier CannotDiscoverClues)
          )
        $ ReactionAbility
            (SkillTestResult
              Timing.After
              You
              (WhileInvestigating Anywhere)
              (SuccessResult $ AtLeast $ Static 2)
            )
            Free
    ]

instance HasTokenValue env RexMurphy where
  getTokenValue (RexMurphy attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance (InvestigatorRunner env) => RunMessage env RexMurphy where
  runMessage msg i@(RexMurphy attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> i <$ push
      (DiscoverCluesAtLocation
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
