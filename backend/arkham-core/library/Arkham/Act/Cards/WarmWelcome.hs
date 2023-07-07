module Arkham.Act.Cards.WarmWelcome (
  WarmWelcome (..),
  warmWelcome,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement

newtype WarmWelcome = WarmWelcome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

warmWelcome :: ActCard WarmWelcome
warmWelcome = act (1, A) WarmWelcome Cards.warmWelcome (Just $ GroupClueCost (PerPlayer 3) Anywhere)

spawnNathanWick :: LocationId -> GameT [Message]
spawnNathanWick library = do
  nathanWick <- getSetAsideCard Enemies.nathanWickMasterOfInitiation
  puzzleBox <- getSetAsideCard Assets.puzzleBox
  (nathanWickId, placeNathanWick) <- createEnemyAt nathanWick library Nothing
  puzzleBoxId <- getRandom
  pure
    [ placeNathanWick
    , CreateAssetAt puzzleBoxId puzzleBox (AttachedToEnemy nathanWickId)
    ]

instance RunMessage WarmWelcome where
  runMessage msg a@(WarmWelcome attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      mLibrary <- selectOne $ locationIs Locations.library
      case mLibrary of
        Just library -> do
          spawnMessages <- spawnNathanWick library
          pushAll $ spawnMessages <> [advanceActDeck attrs]
        Nothing -> do
          (library, placeLibrary) <- placeSetAsideLocation Locations.library
          spawnMessages <- spawnNathanWick library
          pushAll $
            placeLibrary
              : spawnMessages <> [advanceActDeck attrs]
      pure a
    _ -> WarmWelcome <$> runMessage msg attrs
