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
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Window
import Control.Monad.Extra (findM)

newtype BalefulReveler = BalefulReveler EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balefulReveler :: EnemyCard BalefulReveler
balefulReveler =
  enemy BalefulReveler Cards.balefulReveler (4, PerPlayer 5, 3) (2, 2)

instance HasModifiersFor env BalefulReveler

forcedAbility :: EnemyAttrs -> Ability
forcedAbility attrs = (mkAbility attrs 1 LegacyForcedAbility)
  { abilityLimit = GroupLimit PerRound 1
  }

instance EnemyAttrsHasAbilities env => HasAbilities env BalefulReveler where
  getAbilities _ (Window Timing.After (MoveFromHunter eid)) (BalefulReveler attrs)
    | eid == toId attrs
    = pure [forcedAbility attrs]
  getAbilities i window (BalefulReveler attrs) = getAbilities i window attrs

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
