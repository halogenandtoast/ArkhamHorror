module Arkham.Homebrew.CircusExMortis.Assets.RalphDykstra (ralphDykstra) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Classes.HasGame (HasGame)
import Arkham.GameValue (GameValue (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RalphDykstra = RalphDykstra AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ralphDykstra :: AssetCard RalphDykstra
ralphDykstra = asset RalphDykstra Cards.ralphDykstra

instance HasAbilities RalphDykstra where
  getAbilities (RalphDykstra attrs) =
    [ groupLimit PerRound
        $ restricted attrs 1 (thisExists attrs (not_ AssetExhausted) <> OnSameLocation)
        $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)
    ]

{- | "Remove 1 doom from any card in play" — no single matcher spans every
entity type doom can sit on, so this unions the per-type "any doom" matchers.
-}
anyCardWithDoomTargets :: HasGame m => m [Target]
anyCardWithDoomTargets = do
  enemies <- map toTarget <$> select EnemyWithAnyDoom
  agendas <- map toTarget <$> select AgendaWithAnyDoom
  assets <- map toTarget <$> select AssetWithAnyDoom
  events <- map toTarget <$> select EventWithAnyDoom
  investigators <- map toTarget <$> select InvestigatorWithAnyDoom
  locations <- map toTarget <$> select LocationWithAnyDoom
  treacheries <- map toTarget <$> select TreacheryWithAnyDoom
  pure $ enemies <> agendas <> assets <> events <> investigators <> locations <> treacheries

instance RunMessage RalphDykstra where
  runMessage msg a@(RalphDykstra attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> placeClues attrs (LocationTarget loc) n
      doomed <- anyCardWithDoomTargets
      chooseOrRunOneM iid $ targets doomed \tg -> removeDoom attrs tg 1
      pure a
    _ -> RalphDykstra <$> liftRunMessage msg attrs
