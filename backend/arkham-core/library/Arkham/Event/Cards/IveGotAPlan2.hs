module Arkham.Event.Cards.IveGotAPlan2
  ( iveGotAPlan2
  , IveGotAPlan2(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Action
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Event.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype IveGotAPlan2 = IveGotAPlan2 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan2 :: EventCard IveGotAPlan2
iveGotAPlan2 = event IveGotAPlan2 Cards.iveGotAPlan2

instance HasModifiersFor IveGotAPlan2 where
  getModifiersFor (SkillTestSource iid _ _ (Just Fight)) (InvestigatorTarget _) (IveGotAPlan2 attrs)
    = do
      clueCount <- field InvestigatorClues iid
      pure $ toModifiers
        attrs
        [DamageDealt (min clueCount 3), SkillModifier SkillIntellect 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage IveGotAPlan2 where
  runMessage msg e@(IveGotAPlan2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e <$ push
        (ChooseFightEnemy
          iid
          (EventSource eid)
          Nothing
          SkillIntellect
          mempty
          False
        )
    _ -> IveGotAPlan2 <$> runMessage msg attrs
