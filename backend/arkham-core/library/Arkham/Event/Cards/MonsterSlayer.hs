module Arkham.Event.Cards.MonsterSlayer (monsterSlayer, MonsterSlayer (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Fight
import Arkham.Prelude

newtype MonsterSlayer = MonsterSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer :: EventCard MonsterSlayer
monsterSlayer = event MonsterSlayer Cards.monsterSlayer

instance RunMessage MonsterSlayer where
  runMessage msg e@(MonsterSlayer attrs) = case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid attrs
      pushAll
        [ skillTestModifier sid attrs iid (DamageDealt 1)
        , chooseFight
        ]
      pure e
    _ -> MonsterSlayer <$> runMessage msg attrs
