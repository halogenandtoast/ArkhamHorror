module Arkham.Event.Cards.EtherealForm (etherealForm, EtherealForm (..)) where

import Arkham.Classes
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype EtherealForm = EtherealForm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealForm :: EventCard EtherealForm
etherealForm = event EtherealForm Cards.etherealForm

instance RunMessage EtherealForm where
  runMessage msg e@(EtherealForm attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      chooseEvade <- toMessage <$> mkChooseEvade sid iid attrs
      pushAll
        [ skillTestModifier sid attrs iid (AddSkillValue #willpower)
        , chooseEvade
        ]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select $ enemyEngagedWith iid
      pushAll
        $ roundModifiers attrs iid [Ethereal, CannotBeEngaged, CannotAttack, CannotDealDamage]
        : map (DisengageEnemy iid) enemies
      pure e
    _ -> EtherealForm <$> runMessage msg attrs
