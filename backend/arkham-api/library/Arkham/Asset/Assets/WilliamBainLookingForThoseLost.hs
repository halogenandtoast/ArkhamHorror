module Arkham.Asset.Assets.WilliamBainLookingForThoseLost (williamBainLookingForThoseLost) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue (GameValue (..), perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheMidwinterGala.Helpers

newtype WilliamBainLookingForThoseLost = WilliamBainLookingForThoseLost AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamBainLookingForThoseLost :: AssetCard WilliamBainLookingForThoseLost
williamBainLookingForThoseLost =
  allyWith WilliamBainLookingForThoseLost Cards.williamBainLookingForThoseLost (4, 4) noSlots

instance HasModifiersFor WilliamBainLookingForThoseLost where
  getModifiersFor (WilliamBainLookingForThoseLost a) = do
    for_ a.controller \iid -> do
      getSkillTest >>= traverse_ \st ->
        when (st.investigator == iid) do
          committed <- field InvestigatorCommittedCards iid
          when (notNull committed) do
            modified_ a iid [SkillModifier s 1 | s <- [#willpower, #intellect, #combat, #agility]]

instance HasAbilities WilliamBainLookingForThoseLost where
  getAbilities (WilliamBainLookingForThoseLost a) =
    [ controlled a 1 (DuringSkillTest (YourSkillTest AnySkillTest) <> ScenarioDeckWithCard GuestDeck)
        $ triggered
          (CommittedCards #after You $ LengthIs $ atLeast 1)
          (exhaust a <> CalculatedResourceCost (GameValueCalculation $ PerPlayer 1))
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage WilliamBainLookingForThoseLost where
  runMessage msg a@(WilliamBainLookingForThoseLost attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      push $ SpendResources iid n
      lobby <- selectJust $ locationIs Locations.lobbyTheMidwinterGala
      mCard <- listToMaybe <$> getGuestDeck
      for_ mCard \card -> do
        push $ RemoveCardFromScenarioDeck GuestDeck card
        createAssetAt_ card (AtLocation lobby)
      shuffleGuestDeck
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      pure a
    _ -> WilliamBainLookingForThoseLost <$> liftRunMessage msg attrs
