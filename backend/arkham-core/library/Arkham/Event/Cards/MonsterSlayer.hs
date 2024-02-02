module Arkham.Event.Cards.MonsterSlayer (
  monsterSlayer,
  MonsterSlayer (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.SkillType

newtype MonsterSlayer = MonsterSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

monsterSlayer :: EventCard MonsterSlayer
monsterSlayer = event MonsterSlayer Cards.monsterSlayer

instance RunMessage MonsterSlayer where
  runMessage msg e@(MonsterSlayer attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e
        <$ pushAll
          [ skillTestModifier
              (toSource attrs)
              (InvestigatorTarget iid)
              (DamageDealt 1)
          , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
          ]
    _ -> MonsterSlayer <$> runMessage msg attrs
