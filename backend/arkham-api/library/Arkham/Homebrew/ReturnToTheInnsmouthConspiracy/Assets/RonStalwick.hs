module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.RonStalwick (ronStalwick) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.Hybrid
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as Cards
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.TheVanishingOfElinaHarper.Helpers

newtype RonStalwick = RonStalwick AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ronStalwick :: AssetCard RonStalwick
ronStalwick = ally RonStalwick Cards.ronStalwick (1, 0)

instance HasAbilities RonStalwick where
  getAbilities (RonStalwick a) =
    [ removeFromGameWhenDefeated a 1
    , controlled a 2 NoRestriction actionAbility
    ]

instance RunMessage RonStalwick where
  runMessage msg a@(RonStalwick attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putHybridIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      leads <- take 2 <$> getScenarioDeck LeadsDeck
      focusCards leads do
        chooseOneM iid $ targets leads \lead -> do
          unfocusCards
          -- "remove one of those cards and Ron Stalwick from the game"
          removeCardFromGame lead
          removeFromGame attrs
          n <- perPlayer 1
          gainClues iid (attrs.ability 2) n
          shuffleLeadsDeck
      pure a
    _ -> RonStalwick <$> liftRunMessage msg attrs
