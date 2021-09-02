module Arkham.Types.Investigator.Cards.MinhThiPhan
  ( minhThiPhan
  , MinhThiPhan(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

minhThiPhan :: MinhThiPhan
minhThiPhan = MinhThiPhan $ baseAttrs
  "03002"
  "Minh Thi Phan"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Assistant]

instance HasAbilities MinhThiPhan where
  getAbilities (MinhThiPhan attrs) =
    [ restrictedAbility
          attrs
          1
          Self
          (ReactionAbility
            (CommittedCards Timing.After (InvestigatorAt YourLocation) AnyCards)
            Free
          )
        & (abilityLimitL .~ PerInvestigatorLimit PerRound 1)
    ]

instance HasTokenValue env MinhThiPhan where
  getTokenValue (MinhThiPhan attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

-- TODO: Should we let card selection for ability
instance (InvestigatorRunner env) => RunMessage env MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.CommittedCards _ (card : _))] 1 _
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
        (push
          (chooseOne
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
        )
    _ -> MinhThiPhan <$> runMessage msg attrs
