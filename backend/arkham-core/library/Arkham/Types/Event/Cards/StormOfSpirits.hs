module Arkham.Types.Event.Cards.StormOfSpirits
  ( stormOfSpirits
  , StormOfSpirits(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
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
        [ ChooseFightEnemy
            iid
            (toSource attrs)
            Nothing
            SkillWillpower
            mempty
            False
        ]
    Successful (Action.Fight, EnemyTarget eid) iid _ target
      | isTarget attrs target -> do
        enemies <- selectList $ EnemyAt YourLocation
        let
          additionalDamage eid' = if eid == eid'
            then WithAdditionalDamage
            else WithoutAdditionalDamage
        e <$ pushAll
          [ InvestigatorDamageEnemy
              iid
              eid'
              (additionalDamage eid')
              (toSource attrs)
          | eid' <- enemies
          ]
    _ -> StormOfSpirits <$> runMessage msg attrs
