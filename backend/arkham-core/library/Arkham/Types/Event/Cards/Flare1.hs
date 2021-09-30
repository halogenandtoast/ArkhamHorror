module Arkham.Types.Event.Cards.Flare1 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards (flare1)
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Flare1 = Flare1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity, SourceEntity)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

findAllyMessages :: InvestigatorId -> [InvestigatorId] -> Flare1 -> [Message]
findAllyMessages iid investigatorIds e =
  [ CheckAttackOfOpportunity iid False
  , chooseOne
    iid
    [ Search
        iid'
        (toSource e)
        (InvestigatorTarget iid')
        [fromTopOfDeck 9]
        [Ally]
        (DeferSearchedToTarget $ toTarget e)
    | iid' <- investigatorIds
    ]
  ]

instance EventRunner env => RunMessage env Flare1 where
  runMessage msg e@(Flare1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      investigatorIds <- getInvestigatorIds
      fightableEnemies <- getSetList @FightableEnemyId (iid, toSource e)
      e <$ if null fightableEnemies
        then pushAll $ findAllyMessages iid investigatorIds e
        else push $ chooseOne
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
    SearchFound iid target _ [card] | isTarget e target ->
      e <$ pushAll [PutCardIntoPlay iid card Nothing, Exile target]
    SearchNoneFound _ target | isTarget e target -> e <$ push (Discard target)
    _ -> Flare1 <$> runMessage msg attrs
