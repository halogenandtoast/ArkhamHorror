module Arkham.Event.Cards.CleanThemOut (
  cleanThemOut,
  CleanThemOut (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.SkillType

newtype CleanThemOut = CleanThemOut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanThemOut :: EventCard CleanThemOut
cleanThemOut =
  event CleanThemOut Cards.cleanThemOut

instance RunMessage CleanThemOut where
  runMessage msg e@(CleanThemOut attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ TakeResources iid 2 (toSource attrs) False
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure e
    _ -> CleanThemOut <$> runMessage msg attrs
