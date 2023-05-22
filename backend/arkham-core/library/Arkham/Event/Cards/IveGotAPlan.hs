module Arkham.Event.Cards.IveGotAPlan
  ( iveGotAPlan
  , IveGotAPlan(..)
  ) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType

newtype IveGotAPlan = IveGotAPlan EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan :: EventCard IveGotAPlan
iveGotAPlan = event IveGotAPlan Cards.iveGotAPlan

instance HasModifiersFor IveGotAPlan where
  getModifiersFor (InvestigatorTarget _) (IveGotAPlan attrs) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource iid _ _ (Just Fight)) -> do
        clueCount <- field InvestigatorClues iid
        pure $ toModifiers attrs [DamageDealt (min clueCount 3)]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      pushAll
        [ ChooseFightEnemy
          iid
          (EventSource eid)
          Nothing
          SkillIntellect
          mempty
          False
        ]
      pure e
    _ -> IveGotAPlan <$> runMessage msg attrs
