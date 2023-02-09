module Arkham.Event.Cards.MonsterSlayer
  ( monsterSlayer
  , MonsterSlayer(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype MonsterSlayer = MonsterSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer :: EventCard MonsterSlayer
monsterSlayer = event MonsterSlayer Cards.monsterSlayer

instance RunMessage MonsterSlayer where
  runMessage msg e@(MonsterSlayer attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ skillTestModifier
          (toSource attrs)
          (InvestigatorTarget iid)
          (DamageDealt 1)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        , discard attrs
        ]
    _ -> MonsterSlayer <$> runMessage msg attrs
