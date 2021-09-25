module Arkham.Types.Event.Cards.StormOfSpirits
  ( stormOfSpirits
  , StormOfSpirits(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.SkillType

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
    _ -> StormOfSpirits <$> runMessage msg attrs
