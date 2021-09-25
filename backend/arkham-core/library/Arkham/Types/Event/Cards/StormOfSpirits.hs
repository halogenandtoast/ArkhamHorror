module Arkham.Types.Event.Cards.StormOfSpirits
  ( stormOfSpirits
  , StormOfSpirits(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.DamageEffect
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher hiding (AttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance EventRunner env => RunMessage env StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateEffect
          (toCardCode attrs)
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        , ChooseFightEnemy
          iid
          (toSource attrs)
          Nothing
          SkillWillpower
          mempty
          False
        ]
    Successful (Action.Fight, EnemyTarget eid) iid _ target
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
