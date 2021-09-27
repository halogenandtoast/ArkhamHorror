module Arkham.Types.Event.Cards.MonsterSlayer
  ( monsterSlayer
  , MonsterSlayer(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MonsterSlayer = MonsterSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer :: EventCard MonsterSlayer
monsterSlayer = event MonsterSlayer Cards.monsterSlayer

instance EventRunner env => RunMessage env MonsterSlayer where
  runMessage msg e@(MonsterSlayer attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ skillTestModifier
          (toSource attrs)
          (InvestigatorTarget iid)
          (DamageDealt 1)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        , Discard (toTarget attrs)
        ]
    _ -> MonsterSlayer <$> runMessage msg attrs
