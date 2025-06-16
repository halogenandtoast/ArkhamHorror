module Arkham.Asset.Assets.WilliamBainLookingForThoseLost (
  williamBainLookingForThoseLost,
  WilliamBainLookingForThoseLost(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Trait

newtype WilliamBainLookingForThoseLost = WilliamBainLookingForThoseLost AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamBainLookingForThoseLost :: AssetCard WilliamBainLookingForThoseLost
williamBainLookingForThoseLost =
  allyWith WilliamBainLookingForThoseLost Cards.williamBainLookingForThoseLost (4, 4) noSlots

instance HasModifiersFor WilliamBainLookingForThoseLost where
  getModifiersFor (WilliamBainLookingForThoseLost a) = for_ a.controller \iid -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Just st | skillTestInvestigator st == iid -> do
        committed <- field InvestigatorCommittedCards iid
        when (notNull committed) do
          modified_ a iid [SkillModifier s 1 | s <- [#willpower, #intellect, #combat, #agility]]
      _ -> pure ()

instance HasAbilities WilliamBainLookingForThoseLost where
  getAbilities (WilliamBainLookingForThoseLost a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ ReactionAbility (CommittedCards #after You $ LengthIs $ AtLeast $ Static 1) (exhaust a)
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage WilliamBainLookingForThoseLost where
  runMessage msg a@(WilliamBainLookingForThoseLost attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      push $ SpendResources iid n
      lobby <- selectJust $ LocationIs Locations.tmgLobby
      mCard <- listToMaybe <$> getGuestDeck
      for_ mCard \card -> do
        pushAll [RemoveCardFromScenarioDeck GuestDeck card, CreateAssetAt_ card (AtLocation lobby)]
      shuffleGuestDeck
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> WilliamBainLookingForThoseLost <$> liftRunMessage msg attrs
