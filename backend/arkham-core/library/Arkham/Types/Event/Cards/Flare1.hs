module Arkham.Types.Event.Cards.Flare1 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards (flare1)
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Flare1 = Flare1 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity, SourceEntity)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

instance HasModifiersFor env Flare1 where
  getModifiersFor = noModifiersFor

findAllyMessages :: InvestigatorId -> [InvestigatorId] -> Flare1 -> [Message]
findAllyMessages iid investigatorIds e =
  [ CheckAttackOfOpportunity iid False
  , chooseOne
    iid
    [ SearchTopOfDeck
        iid'
        (toSource e)
        (InvestigatorTarget iid')
        9
        [Ally]
        (ShuffleBackIn $ NotifyTargetOfFound (toTarget e))
    | iid' <- investigatorIds
    ]
  ]

instance HasActions env Flare1 where
  getActions i window (Flare1 attrs) = getActions i window attrs

instance
  ( EventRunner env
  , HasSet FightableEnemyId env (InvestigatorId, Source)
  )
  => RunMessage env Flare1 where
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
            , ChooseFightEnemy iid (toSource e) SkillCombat mempty False
            , Exile (toTarget e)
            ]
          , Label "Search for Ally" $ findAllyMessages iid investigatorIds e
          ]
    SearchTopOfDeckFound iid target card | isTarget e target ->
      e <$ pushAll [PutCardIntoPlay iid card Nothing [], Exile target]
    SearchTopOfDeckNoneFound _ target | isTarget e target ->
      e <$ push (Discard target)
    _ -> Flare1 <$> runMessage msg attrs
