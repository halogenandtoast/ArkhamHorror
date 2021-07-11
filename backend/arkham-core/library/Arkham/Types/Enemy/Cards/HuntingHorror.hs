module Arkham.Types.Enemy.Cards.HuntingHorror
  ( huntingHorror
  , HuntingHorror(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import Data.UUID (nil)

newtype HuntingHorror = HuntingHorror EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingHorror :: EnemyCard HuntingHorror
huntingHorror = enemy HuntingHorror Cards.huntingHorror (2, Static 3, 2) (1, 1)

instance HasModifiersFor env HuntingHorror

instance ActionRunner env => HasActions env HuntingHorror where
  getActions i window (HuntingHorror attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env HuntingHorror where
  runMessage msg e@(HuntingHorror attrs@EnemyAttrs {..}) = case msg of
    BeginEnemy -> e <$ push (RequestTokens (toSource attrs) Nothing 1 SetAside)
    RequestedTokens source _ tokens | isSource attrs source -> do
      e <$ when
        (any
          (`elem` map tokenFace tokens)
          [Skull, Cultist, Tablet, ElderThing, AutoFail]
        )
        (push (Ready $ toTarget attrs))
    When (RemoveEnemy eid) | eid == enemyId -> do
      _ <- popMessage
      e <$ pushAll (resolve $ PlaceEnemyInVoid enemyId)
    When (EnemyDefeated eid _ _ _ _ _) | eid == enemyId -> do
      _ <- popMessage
      e <$ pushAll (resolve $ PlaceEnemyInVoid enemyId)
    When (Discard target) | isTarget attrs target -> do
      _ <- popMessage
      e <$ pushAll (resolve $ PlaceEnemyInVoid enemyId)
    When (PlaceEnemyInVoid eid) | eid == enemyId ->
      pure
        . HuntingHorror
        $ attrs
        & (damageL .~ 0)
        & (doomL .~ 0)
        & (cluesL .~ 0)
        & (engagedInvestigatorsL .~ mempty)
        & (locationL .~ LocationId (CardId nil))
    _ -> HuntingHorror <$> runMessage msg attrs
