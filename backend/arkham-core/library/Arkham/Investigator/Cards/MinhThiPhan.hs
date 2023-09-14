module Arkham.Investigator.Cards.MinhThiPhan (
  minhThiPhan,
  MinhThiPhan (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

minhThiPhan :: InvestigatorCard MinhThiPhan
minhThiPhan =
  investigator MinhThiPhan Cards.minhThiPhan
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 4, combat = 2, agility = 2}

instance HasAbilities MinhThiPhan where
  getAbilities (MinhThiPhan attrs) =
    [ limitedAbility (PerInvestigatorLimit PerRound 1)
        $ restrictedAbility attrs 1 Self
        $ ReactionAbility (CommittedCard Timing.After (InvestigatorAt YourLocation) AnyCard) Free
    ]

instance HasChaosTokenValue MinhThiPhan where
  getChaosTokenValue iid ElderSign (MinhThiPhan attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

-- TODO: Should we let card selection for ability
instance RunMessage MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.CommittedCard _ card)] _ -> do
      push $ CreateEffect "03002" Nothing (toSource attrs) (CardIdTarget $ toCardId card)
      pure i
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      skills <- selectList AnySkill
      pushWhen (notNull skills)
        $ chooseOne iid
        $ targetLabels1 skills (CreateEffect "03002" Nothing (toSource ElderSign) . toTarget)
      pure i
    _ -> MinhThiPhan <$> runMessage msg attrs
