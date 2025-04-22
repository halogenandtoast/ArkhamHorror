module Arkham.Act.Cards.TheBrotherhoodIsRevealed (theBrotherhoodIsRevealed) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyLastKnownLocation))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype Metadata = Metadata {mariaDeSilvasLocation :: Maybe LocationId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheBrotherhoodIsRevealed = TheBrotherhoodIsRevealed (ActAttrs `With` Metadata)
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrotherhoodIsRevealed :: ActCard TheBrotherhoodIsRevealed
theBrotherhoodIsRevealed =
  act
    (3, E)
    (TheBrotherhoodIsRevealed . (`with` Metadata Nothing))
    Cards.theBrotherhoodIsRevealed
    Nothing

instance HasModifiersFor TheBrotherhoodIsRevealed where
  getModifiersFor (TheBrotherhoodIsRevealed (a `With` _)) = do
    modifySelectMaybe a IsIchtacasPrey \eid -> do
      liftGuardM $ selectAny $ locationWithEnemy eid <> LocationWithoutClues
      n <- lift $ perPlayer 1
      pure [EnemyFight 1, HealthModifier n, EnemyEvade 1]

instance HasAbilities TheBrotherhoodIsRevealed where
  getAbilities (TheBrotherhoodIsRevealed (a `With` _)) =
    [ restricted a 1 (not_ $ exists IsIchtacasPrey) $ Objective $ forced AnyWindow
    | onSide E a
    ]

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
        iids <- select $ NearestToLocation $ LocationWithId lid
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
            location <- join <$> fieldMay EnemyLastKnownLocation eid
            pure
              . TheBrotherhoodIsRevealed
              $ attrs
              `with` Metadata (Just $ fromJustNote "missing location" location)
          else pure a
      _ ->
        TheBrotherhoodIsRevealed . (`with` metadata) <$> runMessage msg attrs
