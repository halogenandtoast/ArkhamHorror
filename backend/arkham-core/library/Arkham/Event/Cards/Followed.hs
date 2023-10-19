module Arkham.Event.Cards.Followed (
  followed,
  Followed (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher

newtype Followed = Followed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followed :: EventCard Followed
followed = event Followed Cards.followed

instance RunMessage Followed where
  runMessage msg e@(Followed attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <- selectWithField Field.EnemyDamage $ enemyAtLocationWith iid
      investigation <- mkInvestigate iid attrs
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel
            enemy
            [ skillTestModifiers (toSource attrs) iid [SkillModifier #intellect (min 5 dmg), DiscoveredClues 1]
            , toMessage investigation
            ]
          | (enemy, dmg) <- enemies
          ]
      pure e
    _ -> Followed <$> runMessage msg attrs
