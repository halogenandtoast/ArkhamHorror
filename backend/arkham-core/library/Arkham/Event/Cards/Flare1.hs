module Arkham.Event.Cards.Flare1 where

import Arkham.Prelude

import Arkham.Asset.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards (flare1)
import Arkham.Event.Runner
import Arkham.Helpers.Enemy
import Arkham.Matcher

newtype Flare1 = Flare1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

instance RunMessage Flare1 where
  runMessage msg e@(Flare1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigators <- select $ affectsOthers can.manipulate.deck
      fightableEnemies <- getFightableEnemyIds iid attrs
      player <- getPlayer iid

      let doSearch iid' = targetLabel iid' [search iid' e iid' [fromTopOfDeck 9] #ally (defer $ toTarget e)]
      let searchForAlly = [CheckAttackOfOpportunity iid False, chooseOne player $ map doSearch investigators]

      push
        $ chooseOrRunOne player
        $ [ Label "Fight"
            $ [ skillTestModifiers attrs iid [SkillModifier #combat 3, DamageDealt 2]
              , chooseFightEnemy iid e #combat
              , Exile (toTarget e)
              ]
          | notNull fightableEnemies
          ]
        <> [Label "Search for Ally" searchForAlly]
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let choices = [targetLabel (toCardId card) [putCardIntoPlay iid card] | card <- cards]
      targetCount <- getTotalSearchTargets iid choices 1
      player <- getPlayer iid
      pushAll [chooseN player targetCount choices, Exile (toTarget attrs)]
      pure e
    _ -> Flare1 <$> runMessage msg attrs
