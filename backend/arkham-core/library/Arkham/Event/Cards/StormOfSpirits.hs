module Arkham.Event.Cards.StormOfSpirits (
  stormOfSpirits,
  StormOfSpirits (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (AttackDamageEffect)
import Arkham.Message
import Arkham.SkillType

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e
        <$ pushAll
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
            toMsg eid' =
              if eid == eid'
                then EnemyDamage eid' $ delayDamage $ attack attrs 2
                else EnemyDamage eid' $ delayDamage $ directDamage $ attack attrs 2
          msgs <- selectListMap toMsg $ EnemyAt (locationWithInvestigator iid)
          pushAll $ msgs <> [CheckDefeated (toSource attrs)]
          pure e
    _ -> StormOfSpirits <$> runMessage msg attrs
