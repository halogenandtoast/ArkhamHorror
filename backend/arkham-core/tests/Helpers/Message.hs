module Helpers.Message where

import ClassyPrelude

import Arkham.Types.Asset
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Event
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

playEvent :: Investigator -> Event -> Message
playEvent i e = InvestigatorPlayEvent (toId i) (toId e) Nothing

moveTo :: Investigator -> Location -> Message
moveTo i l = MoveTo (toId i) (toId l)

moveFrom :: Investigator -> Location -> Message
moveFrom i l = MoveFrom (toId i) (toId l)

moveAllTo :: Location -> Message
moveAllTo = MoveAllTo . getLocationId

enemySpawn :: Location -> Enemy -> Message
enemySpawn l e = EnemySpawn Nothing (toId l) (toId e)

loadDeck :: Investigator -> [PlayerCard] -> Message
loadDeck i cs = LoadDeck (toId i) cs

addToHand :: Investigator -> Card -> Message
addToHand i card = AddToHand (toId i) card

chooseEndTurn :: Investigator -> Message
chooseEndTurn i = ChooseEndTurn (toId i)

enemyAttack :: Investigator -> Enemy -> Message
enemyAttack i e = EnemyAttack (toId i) (toId e)

fightEnemy :: Investigator -> Enemy -> Message
fightEnemy i e = FightEnemy (toId i) (toId e) TestSource SkillCombat False

engageEnemy :: Investigator -> Enemy -> Message
engageEnemy i e = EngageEnemy (toId i) (toId e) False

disengageEnemy :: Investigator -> Enemy -> Message
disengageEnemy i e = DisengageEnemy (toId i) (toId e)

playAsset :: Investigator -> Asset -> Message
playAsset i a =
  InvestigatorPlayAsset (toId i) (toId a) (slotsOf a) (toList $ getTraits a)

playDynamicCard :: Investigator -> Card -> Message
playDynamicCard i c = PlayDynamicCard (toId i) (getCardId c) 0 Nothing True

drawCards :: Investigator -> Int -> Message
drawCards i n = DrawCards (toId i) n False

investigate :: Investigator -> Location -> Message
investigate i l =
  Investigate (toId i) (getLocationId l) TestSource SkillIntellect False

beginSkillTest :: Investigator -> SkillType -> Int -> Message
beginSkillTest i stype difficulty =
  BeginSkillTest (toId i) TestSource TestTarget Nothing stype difficulty
