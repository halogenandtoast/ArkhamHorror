module Arkham.Treachery.Cards.Fragmentation (fragmentation) where

import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Deck qualified as Deck
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers (scenarioI18n)
import Arkham.Trait (Trait (Emissary))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Fragmentation = Fragmentation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentation :: TreacheryCard Fragmentation
fragmentation = treachery Fragmentation Cards.fragmentation

placeAssetOnAbyss :: ReverseQueue m => InvestigatorId -> AssetId -> m ()
placeAssetOnAbyss iid aid = do
  card <- field AssetCard aid
  push $ RemoveFromPlay (toSource aid)
  push $ PutCardOnTopOfDeck iid (Deck.ScenarioDeckByKey AbyssDeck) card

nearestEmissaryAttacks :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
nearestEmissaryAttacks source iid = do
  emissaries <- select $ NearestEnemyTo iid (EnemyWithTrait Emissary)
  chooseOrRunOneM iid $ targets emissaries \eid -> initiateEnemyAttack eid source iid

instance RunMessage Fragmentation where
  runMessage msg t@(Fragmentation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> AssetWithHighestPrintedCost AnyAsset
      chooseOneM iid $ scenarioI18n do
        when (notNull assets) $ labeled' "fragmentation.placeAssetOnAbyss" do
          chooseOrRunOneM iid $ targets assets $ placeAssetOnAbyss iid
        labeled' "fragmentation.nearestEmissaryAttacks" $ nearestEmissaryAttacks attrs iid
      pure t
    _ -> Fragmentation <$> liftRunMessage msg attrs
