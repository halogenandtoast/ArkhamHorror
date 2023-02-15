module Arkham.Event.Cards.ATestOfWill2
  ( aTestOfWill2
  , ATestOfWill2(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import qualified Arkham.Event.Cards as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype ATestOfWill2 = ATestOfWill2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill2 :: EventCard ATestOfWill2
aTestOfWill2 = event ATestOfWill2 Cards.aTestOfWill2

instance RunMessage ATestOfWill2 where
  runMessage msg e@(ATestOfWill2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CancelNext (toSource attrs) RevelationMessage
        , beginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        ]
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ Exile (toTarget attrs)
        pure e
    _ -> ATestOfWill2 <$> runMessage msg attrs
