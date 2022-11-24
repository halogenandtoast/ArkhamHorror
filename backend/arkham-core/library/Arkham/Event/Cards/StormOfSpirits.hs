module Arkham.Event.Cards.StormOfSpirits
  ( stormOfSpirits
  , StormOfSpirits(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Runner
import Arkham.Matcher hiding (AttackDamageEffect)
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateEffect
          (toCardCode attrs)
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        , ChooseFightEnemy
          iid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillWillpower
          mempty
          False
        ]
    Successful (Action.Fight, EnemyTarget eid) iid _ target _
      | isTarget attrs target -> do
        let
          toMsg eid' = if eid == eid'
            then EnemyDamage eid' iid (toSource attrs) AttackDamageEffect 2
            else DirectEnemyDamage
              eid'
              iid
              (toSource attrs)
              AttackDamageEffect
              2
        msgs <- selectListMap toMsg $ EnemyAt YourLocation
        e <$ pushAll msgs
    _ -> StormOfSpirits <$> runMessage msg attrs
