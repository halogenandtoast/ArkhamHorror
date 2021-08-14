module Arkham.Types.Enemy.Cards.BalefulReveler
  ( balefulReveler
  , BalefulReveler(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Control.Monad.Extra (findM)

newtype BalefulReveler = BalefulReveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balefulReveler :: EnemyCard BalefulReveler
balefulReveler =
  enemy BalefulReveler Cards.balefulReveler (4, PerPlayer 5, 3) (2, 2)

instance HasActions BalefulReveler where
  getActions (BalefulReveler x) =
    [ mkAbility
          x
          1
          (ForcedAbility $ MoveFromHunter Timing.After $ EnemyWithId $ toId x)
        & (abilityLimitL .~ GroupLimit PerRound 1)
    ]

instance EnemyAttrsRunMessage env => RunMessage env BalefulReveler where
  runMessage msg e@(BalefulReveler attrs) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == toId attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      start <- getId @LocationId leadInvestigatorId
      locations <- getCounterClockwiseLocations start

      mSpawnLocation <- findM (fmap null . getSet @InvestigatorId) locations

      case mSpawnLocation of
        Just spawnLocation -> BalefulReveler <$> runMessage
          msg
          (attrs & spawnAtL ?~ LocationWithId spawnLocation)
        Nothing -> error "could not find location for baleful reveler"
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (RequestTokens source (Just iid) 1 SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      tokenFaces <- getModifiedTokenFaces source tokens
      let
        moveMsg =
          [ HunterMove (toId attrs)
          | any
            (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
            tokenFaces
          ]
      e <$ pushAll (chooseOne iid [Continue "Continue"] : moveMsg)
    _ -> BalefulReveler <$> runMessage msg attrs
