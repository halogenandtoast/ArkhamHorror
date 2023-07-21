module Arkham.Event.Cards.Flare1 where

import Arkham.Prelude

import Arkham.Asset.Helpers
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards (flare1)
import Arkham.Event.Runner
import Arkham.Helpers.Enemy
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Window (defaultWindows)

newtype Flare1 = Flare1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

findAllyMessages :: InvestigatorId -> [InvestigatorId] -> Flare1 -> [Message]
findAllyMessages iid investigatorIds e =
  [ CheckAttackOfOpportunity iid False
  , chooseOne
      iid
      [ targetLabel
        iid
        [ Search
            iid'
            (toSource e)
            (InvestigatorTarget iid')
            [fromTopOfDeck 9]
            IsAlly
            (DeferSearchedToTarget $ toTarget e)
        ]
      | iid' <- investigatorIds
      ]
  ]

instance RunMessage Flare1 where
  runMessage msg e@(Flare1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <- getInvestigatorIds
      fightableEnemies <- getFightableEnemyIds iid (toSource e)
      e
        <$ if null fightableEnemies
          then pushAll $ findAllyMessages iid investigatorIds e
          else
            push $
              chooseOne
                iid
                [ Label
                    "Fight"
                    [ skillTestModifiers
                        attrs
                        (InvestigatorTarget iid)
                        [SkillModifier SkillCombat 3, DamageDealt 2]
                    , ChooseFightEnemy iid (toSource e) Nothing SkillCombat mempty False
                    , Exile (toTarget e)
                    ]
                , Label "Search for Ally" $ findAllyMessages iid investigatorIds e
                ]
    SearchFound iid target _ [card]
      | isTarget e target ->
          e
            <$ pushAll
              [PutCardIntoPlay iid card Nothing (defaultWindows iid), Exile target]
    SearchNoneFound _ target | isTarget e target -> e <$ push (Discard (toSource attrs) target)
    _ -> Flare1 <$> runMessage msg attrs
