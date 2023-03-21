module Helpers.Message where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Asset.Types
import Arkham.Attack qualified as Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Types
import Arkham.Event.Types
import Arkham.Helpers
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Message
import Arkham.Movement
import Arkham.Name
import Arkham.Placement
import Arkham.SkillTest.Base
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Window (defaultWindows)
import Arkham.Zone

playEvent :: Investigator -> Event -> Message
playEvent i e = Run [InvestigatorPlayEvent (toId i) (toId e) Nothing [] FromHand, FinishedEvent (toId e)]

moveTo :: Investigator -> Location -> Message
moveTo i l = MoveTo $ move (toSource i) (toId i) (toId l)

moveFrom :: Investigator -> Location -> Message
moveFrom i l = MoveFrom (toSource i) (toId i) (toId l)

moveAllTo :: Location -> Message
moveAllTo = MoveAllTo (TestSource mempty) . toId

enemySpawn :: Location -> Enemy -> Message
enemySpawn l e = EnemySpawn Nothing (toId l) (toId e)

loadDeck :: Investigator -> [PlayerCard] -> Message
loadDeck i cs = LoadDeck (toId i) (Deck cs)

addToHand :: IsCard a => Investigator -> a -> Message
addToHand i (toCard -> c) = AddToHand (toId i) c

chooseEndTurn :: Investigator -> Message
chooseEndTurn i = ChooseEndTurn (toId i)

enemyAttack :: Investigator -> Enemy -> Message
enemyAttack i e = EnemyAttack $ Attack.enemyAttack (toId e) (toId i)

fightEnemy :: Investigator -> Enemy -> Message
fightEnemy i e =
  FightEnemy (toId i) (toId e) (toSource i) Nothing SkillCombat False

engageEnemy :: Investigator -> Enemy -> Message
engageEnemy i e = EngageEnemy (toId i) (toId e) False

disengageEnemy :: Investigator -> Enemy -> Message
disengageEnemy i e = DisengageEnemy (toId i) (toId e)

playAsset :: Investigator -> Asset -> Message
playAsset i a = PlaceAsset (toId a) (InPlayArea $ toId i)

placedLocation :: Location -> Message
placedLocation l = PlacedLocation (toName l) (toCardCode l) (toId l)

playCard :: Investigator -> Card -> Message
playCard i c = PlayCard (toId i) c Nothing (defaultWindows $ toId i) True

investigate :: Investigator -> Location -> Message
investigate i l =
  Investigate (toId i) (toId l) (TestSource mempty) Nothing SkillIntellect False

beginSkillTest :: Investigator -> SkillType -> Int -> Message
beginSkillTest i sType n =
  BeginSkillTest $ initSkillTest (toId i) (TestSource mempty) TestTarget sType n

beginActionSkillTest :: Investigator -> Action -> Maybe Target -> SkillType -> Int -> Message
beginActionSkillTest i a mt sType n =
  BeginSkillTest $ (initSkillTest (toId i) (TestSource mempty) target sType n)
    { skillTestAction = Just a }
 where
   target = fromMaybe TestTarget mt

playAssetCard :: PlayerCard -> Investigator -> Message
playAssetCard card (toId -> iid) =
  InitiatePlayCard iid (PlayerCard $ card { pcOwner = Just iid}) Nothing (defaultWindows iid) True
