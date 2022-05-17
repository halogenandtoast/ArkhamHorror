module Arkham.Investigator.Cards.MinhThiPhan
  ( minhThiPhan
  , MinhThiPhan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Card hiding (CommittedCard)
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

minhThiPhan :: InvestigatorCard MinhThiPhan
minhThiPhan = investigator
  MinhThiPhan
  Cards.minhThiPhan
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }

instance HasAbilities MinhThiPhan where
  getAbilities (MinhThiPhan attrs) =
    [ restrictedAbility
          attrs
          1
          Self
          (ReactionAbility
            (CommittedCard Timing.After (InvestigatorAt YourLocation) AnyCard)
            Free
          )
        & (abilityLimitL .~ PerInvestigatorLimit PerRound 1)
    ]

instance HasTokenValue env MinhThiPhan where
  getTokenValue iid ElderSign (MinhThiPhan attrs)
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

-- TODO: Should we let card selection for ability
instance (InvestigatorRunner env) => RunMessage env MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.CommittedCard _ card)] 1 _
      | isSource attrs source -> i <$ push
        (CreateEffect
          (unInvestigatorId $ toId attrs)
          Nothing
          (toSource attrs)
          (CardIdTarget $ toCardId card)
        )
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      investigatorIds <- getInvestigatorIds
      skills <-
        concat
          <$> traverse
                (fmap (map unCommitedSkillId) . getSetList @CommittedSkillId)
                investigatorIds
      i <$ when
        (notNull skills)
        (push $ chooseOne
          iid
          [ TargetLabel
              (SkillTarget skill)
              [ CreateEffect
                  (unInvestigatorId $ toId attrs)
                  Nothing
                  (TokenEffectSource ElderSign)
                  (SkillTarget skill)
              ]
          | skill <- skills
          ]
        )
    _ -> MinhThiPhan <$> runMessage msg attrs
