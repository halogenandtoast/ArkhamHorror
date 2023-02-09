module Arkham.Event.Cards.ATestOfWill
  ( aTestOfWill
  , ATestOfWill(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import qualified Arkham.Event.Cards as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype ATestOfWill = ATestOfWill EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill :: EventCard ATestOfWill
aTestOfWill = event ATestOfWill Cards.aTestOfWill

instance RunMessage ATestOfWill where
  runMessage msg e@(ATestOfWill attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        , discard attrs
        ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ CancelNext (toSource attrs) RevelationMessage
        pure e
    _ -> ATestOfWill <$> runMessage msg attrs
