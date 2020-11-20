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
playEvent i e =
  InvestigatorPlayEvent (getInvestigatorId i) (getEventId e) Nothing

moveTo :: Investigator -> Location -> Message
moveTo i l = MoveTo (getInvestigatorId i) (getLocationId l)

moveFrom :: Investigator -> Location -> Message
moveFrom i l = MoveFrom (getInvestigatorId i) (getLocationId l)

moveAllTo :: Location -> Message
moveAllTo = MoveAllTo . getLocationId

enemySpawn :: Location -> Enemy -> Message
enemySpawn l e = EnemySpawn Nothing (getLocationId l) (getEnemyId e)

loadDeck :: Investigator -> [PlayerCard] -> Message
loadDeck i cs = LoadDeck (getInvestigatorId i) cs

addToHand :: Investigator -> Card -> Message
addToHand i card = AddToHand (getInvestigatorId i) card

chooseEndTurn :: Investigator -> Message
chooseEndTurn i = ChooseEndTurn (getInvestigatorId i)

enemyAttack :: Investigator -> Enemy -> Message
enemyAttack i e = EnemyAttack (getInvestigatorId i) (getEnemyId e)

fightEnemy :: Investigator -> Enemy -> Message
fightEnemy i e =
  FightEnemy (getInvestigatorId i) (getEnemyId e) TestSource SkillCombat False

engageEnemy :: Investigator -> Enemy -> Message
engageEnemy i e = EngageEnemy (getInvestigatorId i) (getEnemyId e) False

disengageEnemy :: Investigator -> Enemy -> Message
disengageEnemy i e = DisengageEnemy (getInvestigatorId i) (getEnemyId e)

playAsset :: Investigator -> Asset -> Message
playAsset i a = InvestigatorPlayAsset
  (getInvestigatorId i)
  (getAssetId a)
  (slotsOf a)
  (toList $ getTraits a)

playDynamicCard :: Investigator -> Card -> Message
playDynamicCard i c =
  PlayDynamicCard (getInvestigatorId i) (getCardId c) 0 Nothing True

drawCards :: Investigator -> Int -> Message
drawCards i n = DrawCards (getInvestigatorId i) n False

investigate :: Investigator -> Location -> Message
investigate i l = Investigate
  (getInvestigatorId i)
  (getLocationId l)
  TestSource
  SkillIntellect
  False

beginSkillTest :: Investigator -> SkillType -> Int -> Message
beginSkillTest i stype difficulty = BeginSkillTest
  (getInvestigatorId i)
  TestSource
  TestTarget
  Nothing
  stype
  difficulty
