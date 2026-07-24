module Arkham.Treachery.Cards.Euphoria (euphoria) where

import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Deck qualified as Deck
import Arkham.I18n
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers (scenarioI18n)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Euphoria = Euphoria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

euphoria :: TreacheryCard Euphoria
euphoria = treachery Euphoria Cards.euphoria

placeAssetOnAbyss :: ReverseQueue m => InvestigatorId -> AssetId -> m ()
placeAssetOnAbyss iid aid = do
  card <- field AssetCard aid
  push $ RemoveFromPlay (toSource aid)
  push $ PutCardOnTopOfDeck iid (Deck.ScenarioDeckByKey AbyssDeck) card

euphoriaAsset :: InvestigatorId -> AssetMatcher
euphoriaAsset iid = assetControlledBy iid <> AssetNonStory <> not_ PermanentAsset

chooseAssetOnAbyss :: ReverseQueue m => InvestigatorId -> m ()
chooseAssetOnAbyss iid = do
  assets <- select $ euphoriaAsset iid
  chooseOrRunOneM iid $ targets assets $ placeAssetOnAbyss iid

instance RunMessage Euphoria where
  runMessage msg t@(Euphoria attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      hasAsset <- selectAny $ euphoriaAsset iid
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        when hasAsset $ labeled' "euphoria.placeAssetOnAbyss" $ chooseAssetOnAbyss iid
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      chooseAssetOnAbyss iid
      pure t
    _ -> Euphoria <$> liftRunMessage msg attrs
