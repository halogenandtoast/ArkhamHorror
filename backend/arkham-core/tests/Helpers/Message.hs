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

playEvent :: Investigator -> Event -> Message
playEvent i e = InvestigatorPlayEvent (getId () i) (getId () e)

moveTo :: Investigator -> Location -> Message
moveTo i l = MoveTo (getId () i) (getId () l)

moveFrom :: Investigator -> Location -> Message
moveFrom i l = MoveFrom (getId () i) (getId () l)

moveAllTo :: Location -> Message
moveAllTo = MoveAllTo . getId ()

enemySpawn :: Location -> Enemy -> Message
enemySpawn l e = EnemySpawn (getId () l) (getId () e)

loadDeck :: Investigator -> [PlayerCard] -> Message
loadDeck i cs = LoadDeck (getId () i) cs

addToHand :: Investigator -> Card -> Message
addToHand i card = AddToHand (getId () i) card

chooseEndTurn :: Investigator -> Message
chooseEndTurn i = ChooseEndTurn (getId () i)

enemyAttack :: Investigator -> Enemy -> Message
enemyAttack i e = EnemyAttack (getId () i) (getId () e)

fightEnemy :: Investigator -> Enemy -> Message
fightEnemy i e = FightEnemy (getId () i) (getId () e) SkillCombat [] [] False

engageEnemy :: Investigator -> Enemy -> Message
engageEnemy i e = EngageEnemy (getId () i) (getId () e) False

disengageEnemy :: Investigator -> Enemy -> Message
disengageEnemy i e = DisengageEnemy (getId () i) (getId () e)

playAsset :: Investigator -> Asset -> Message
playAsset i a = InvestigatorPlayAsset
  (getId () i)
  (getId () a)
  (slotsOf a)
  (toList $ getTraits a)

playDynamicCard :: Investigator -> Card -> Message
playDynamicCard i c = PlayDynamicCard (getId () i) (getCardId c) 0 True

drawCards :: Investigator -> Int -> Message
drawCards i n = DrawCards (getId () i) n False

beginSkillTest :: Investigator -> SkillType -> Int -> Message
beginSkillTest i stype difficulty = BeginSkillTest
  (getId () i)
  TestSource
  Nothing
  stype
  difficulty
  mempty
  mempty
  mempty
  mempty
