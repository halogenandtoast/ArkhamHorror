module Arkham.Event.Cards.IveGotAPlan (
  iveGotAPlan,
  IveGotAPlan (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType

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
  runMessage msg e@(IveGotAPlan attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      push $ ChooseFightEnemy iid (EventSource eid) Nothing SkillIntellect mempty False
      pure e
    _ -> IveGotAPlan <$> runMessage msg attrs
