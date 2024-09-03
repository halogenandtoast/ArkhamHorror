module Arkham.Event.Cards.IveGotAPlan2 (iveGotAPlan2, IveGotAPlan2 (..)) where

import Arkham.Action
import Arkham.Aspect
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType

newtype IveGotAPlan2 = IveGotAPlan2 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan2 :: EventCard IveGotAPlan2
iveGotAPlan2 = event IveGotAPlan2 Cards.iveGotAPlan2

instance HasModifiersFor IveGotAPlan2 where
  getModifiersFor (InvestigatorTarget _) (IveGotAPlan2 attrs) = do
    mInvestigator <- getSkillTestInvestigator
    mAction <- getSkillTestAction
    case (mAction, mInvestigator) of
      (Just Fight, Just iid) -> do
        clueCount <- field InvestigatorClues iid
        pure $ toModifiers attrs [DamageDealt (min clueCount 3), SkillModifier SkillIntellect 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage IveGotAPlan2 where
  runMessage msg e@(IveGotAPlan2 attrs) = case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid attrs (#intellect `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pushAll chooseFight
      pure e
    _ -> IveGotAPlan2 <$> runMessage msg attrs
