module Arkham.Act.Cards.TheBrotherhoodIsRevealed (
  TheBrotherhoodIsRevealed (..),
  theBrotherhoodIsRevealed,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype Metadata = Metadata {mariaDeSilvasLocation :: Maybe LocationId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheBrotherhoodIsRevealed = TheBrotherhoodIsRevealed (ActAttrs `With` Metadata)
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrotherhoodIsRevealed :: ActCard TheBrotherhoodIsRevealed
theBrotherhoodIsRevealed =
  act
    (3, E)
    (TheBrotherhoodIsRevealed . (`with` Metadata Nothing))
    Cards.theBrotherhoodIsRevealed
    Nothing

instance HasModifiersFor TheBrotherhoodIsRevealed where
  getModifiersFor (EnemyTarget eid) (TheBrotherhoodIsRevealed (a `With` _)) = do
    isPrey <- isIchtacasPrey eid
    atLocationWithoutClues <- selectAny $ locationWithEnemy eid <> LocationWithoutClues
    n <- perPlayer 1
    pure
      $ toModifiers a
      $ guard (isPrey && atLocationWithoutClues)
      *> [EnemyFight 1, HealthModifier n, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TheBrotherhoodIsRevealed where
  getAbilities (TheBrotherhoodIsRevealed (a `With` _)) | onSide E a = do
    [ restrictedAbility a 1 (Negate $ enemyExists IsIchtacasPrey)
        $ Objective
        $ ForcedAbility AnyWindow
      ]
  getAbilities _ = []

instance RunMessage TheBrotherhoodIsRevealed where
  runMessage msg a@(TheBrotherhoodIsRevealed (attrs `With` metadata)) =
    case msg of
      UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
        push $ advanceVia #other attrs attrs
        pure a
      AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
        lead <- getLeadPlayer
        deckCount <- getActDecksInPlayCount
        ichtaca <- getSetAsideCard Assets.ichtacaTheForgottenGuardian
        lid <- maybe (selectJust $ locationIs Locations.blackCave) pure (mariaDeSilvasLocation metadata)
        iids <- selectList $ NearestToLocation $ LocationWithId lid
        -- TODO: we need to know the prey details
        let
          takeControlMessage =
            chooseOrRunOne lead [targetLabel iid [TakeControlOfSetAsideAsset iid ichtaca] | iid <- iids]
          nextMessage =
            if deckCount <= 1
              then R1
              else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
        pushAll [takeControlMessage, nextMessage]
        pure a
      RemoveEnemy eid -> do
        isPrey <- isIchtacasPrey eid
        isMariaDeSilva <- eid <=~> enemyIs Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn
        if isPrey && isMariaDeSilva
          then do
            location <- fieldJust EnemyLocation eid
            pure . TheBrotherhoodIsRevealed $ attrs `with` Metadata (Just location)
          else pure a
      _ ->
        TheBrotherhoodIsRevealed . (`with` metadata) <$> runMessage msg attrs
