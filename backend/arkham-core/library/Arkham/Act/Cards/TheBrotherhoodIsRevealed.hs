module Arkham.Act.Cards.TheBrotherhoodIsRevealed
  ( TheBrotherhoodIsRevealed(..)
  , theBrotherhoodIsRevealed
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types ( Field (EnemyLocation) )
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Target

newtype Metadata = Metadata { mariaDeSilvasLocation :: Maybe LocationId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheBrotherhoodIsRevealed = TheBrotherhoodIsRevealed (ActAttrs `With` Metadata)
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrotherhoodIsRevealed :: ActCard TheBrotherhoodIsRevealed
theBrotherhoodIsRevealed = act
  (3, E)
  (TheBrotherhoodIsRevealed . (`with` Metadata Nothing))
  Cards.theBrotherhoodIsRevealed
  Nothing

instance HasModifiersFor TheBrotherhoodIsRevealed where
  getModifiersFor (EnemyTarget eid) (TheBrotherhoodIsRevealed (a `With` _)) =
    do
      isPrey <- remembered (IchtacasPrey eid)
      atLocationWithoutClues <-
        selectAny $ locationWithEnemy eid <> LocationWithoutClues
      n <- getPlayerCountValue (PerPlayer 1)
      pure $ if isPrey && atLocationWithoutClues
        then toModifiers a [EnemyFight 1, HealthModifier n, EnemyEvade 1]
        else []
  getModifiersFor _ _ = pure []

instance HasAbilities TheBrotherhoodIsRevealed where
  getAbilities (TheBrotherhoodIsRevealed (a `With` _)) =
    [ restrictedAbility
          a
          1
          (Negate $ EnemyCriteria $ EnemyExists $ IsIchtacasPrey)
        $ Objective
        $ ForcedAbility AnyWindow
    ]

instance RunMessage TheBrotherhoodIsRevealed where
  runMessage msg a@(TheBrotherhoodIsRevealed (attrs `With` metadata)) =
    case msg of
      UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
        push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
        pure a
      AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
        leadInvestigatorId <- getLeadInvestigatorId
        deckCount <- getActDecksInPlayCount
        ichtaca <- selectJust $ assetIs Assets.ichtacaTheForgottenGuardian
        lid <- maybe
          (selectJust $ locationIs Locations.blackCave)
          pure
          (mariaDeSilvasLocation metadata)
        iids <- selectList $ NearestToLocation $ LocationWithId lid
        -- TODO: we need to know the prey details
        let
          takeControlMessage = chooseOrRunOne
            leadInvestigatorId
            [ targetLabel iid [TakeControlOfAsset iid ichtaca] | iid <- iids ]
          nextMessage = if deckCount <= 1
            then ScenarioResolution $ Resolution 1
            else RemoveFromGame (ActTarget $ toId attrs)
        pushAll [takeControlMessage, nextMessage]
        pure a
      RemoveEnemy eid -> do
        isPrey <- remembered (IchtacasPrey eid)
        isMariaDeSilva <- eid
          <=~> enemyIs Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn
        if isPrey && isMariaDeSilva
          then do
            location <- fieldMap
              EnemyLocation
              (fromJustNote "missing locatioN")
              eid
            pure . TheBrotherhoodIsRevealed $ attrs `with` Metadata
              (Just location)
          else pure a
      _ ->
        TheBrotherhoodIsRevealed . (`with` metadata) <$> runMessage msg attrs
