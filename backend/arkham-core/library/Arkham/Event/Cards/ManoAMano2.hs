module Arkham.Event.Cards.ManoAMano2
  ( manoAMano2
  , ManoAMano2(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message

newtype ManoAMano2 = ManoAMano2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manoAMano2 :: EventCard ManoAMano2
manoAMano2 =
  event ManoAMano2 Cards.manoAMano2

instance RunMessage ManoAMano2 where
  runMessage msg e@(ManoAMano2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
      pushAll
        [ chooseOrRunOne iid
          [ EnemyDamage enemy iid (toSource attrs) NonAttackDamageEffect 2
          | enemy <- enemies
          ]
        , Discard (toTarget attrs)
        ]
      pure e
    _ -> ManoAMano2 <$> runMessage msg attrs
