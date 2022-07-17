module Arkham.Enemy.Cards.BalefulReveler
  ( balefulReveler
  , BalefulReveler(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Timing qualified as Timing
import Arkham.Token
import Control.Monad.Extra ( findM )

newtype BalefulReveler = BalefulReveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- TODO: spawn at is complicated here
balefulReveler :: EnemyCard BalefulReveler
balefulReveler =
  enemy BalefulReveler Cards.balefulReveler (4, PerPlayer 5, 3) (2, 2)

instance HasAbilities BalefulReveler where
  getAbilities (BalefulReveler attrs) = withBaseAbilities
    attrs
    [ mkAbility
          attrs
          1
          (ForcedAbility $ MovedFromHunter Timing.After $ EnemyWithId $ toId
            attrs
          )
        & (abilityLimitL .~ GroupLimit PerRound 1)
    ]

instance RunMessage BalefulReveler where
  runMessage msg e@(BalefulReveler attrs) = case msg of
    InvestigatorDrawEnemy _ eid | eid == toId attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      start <- getJustLocation leadInvestigatorId
      locations <- getCounterClockwiseLocations start

      mSpawnLocation <- findM (selectNone . InvestigatorAt . LocationWithId) locations

      case mSpawnLocation of
        Just spawnLocation -> BalefulReveler <$> runMessage
          msg
          (attrs & spawnAtL ?~ LocationWithId spawnLocation)
        Nothing -> error "could not find location for baleful reveler"
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (RequestTokens source (Just iid) (Reveal 1) SetAside)
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
