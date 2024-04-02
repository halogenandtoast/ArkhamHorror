module Arkham.Event.Cards.IveGotAPlan (iveGotAPlan, IveGotAPlan (..)) where

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

newtype IveGotAPlan = IveGotAPlan EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan :: EventCard IveGotAPlan
iveGotAPlan = event IveGotAPlan Cards.iveGotAPlan

instance HasModifiersFor IveGotAPlan where
  getModifiersFor (InvestigatorTarget _) (IveGotAPlan attrs) = do
    mInvestigator <- getSkillTestInvestigator
    mAction <- getSkillTestAction
    case (mAction, mInvestigator) of
      (Just Fight, Just iid) -> do
        clueCount <- field InvestigatorClues iid
        pure $ toModifiers attrs [DamageDealt (min clueCount 3)]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs) = case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      chooseFight <-
        leftOr <$> aspect iid attrs (#intellect `InsteadOf` #combat) (mkChooseFight iid attrs)
      pushAll chooseFight
      pure e
    _ -> IveGotAPlan <$> runMessage msg attrs
