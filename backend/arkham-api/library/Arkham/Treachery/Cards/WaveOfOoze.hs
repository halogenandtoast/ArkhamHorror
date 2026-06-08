module Arkham.Treachery.Cards.WaveOfOoze (waveOfOoze) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (AssetDefeated)

newtype WaveOfOoze = WaveOfOoze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waveOfOoze :: TreacheryCard WaveOfOoze
waveOfOoze = treachery WaveOfOoze Cards.waveOfOoze

instance HasAbilities WaveOfOoze where
  getAbilities (WaveOfOoze a) =
    [mkAbility a 1 $ forced $ AssetDefeated #after (BySource $ SourceIs $ toSource a) AnyAsset]

instance RunMessage WaveOfOoze where
  runMessage msg t@(WaveOfOoze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignHorror iid attrs n
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedAsset -> aid) _ -> do
      card <- field AssetCard aid
      devour [card]
      pure t
    _ -> WaveOfOoze <$> liftRunMessage msg attrs
