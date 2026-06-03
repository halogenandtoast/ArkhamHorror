module Arkham.Location.Cards.Barn (barn) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Omega)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype Barn = Barn LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barn :: LocationCard Barn
barn = symbolLabel $ locationWith Barn Cards.barn 3 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor Barn where
  getModifiersFor (Barn a) = modifySelect a (enemyAt a) [DamageDealt 1]

instance HasAbilities Barn where
  getAbilities (Barn a) =
    extendRevealed1 a $ mkAbility a 1 $ triggered_ $ RevealLocation #after Anyone (be a)

instance RunMessage Barn where
  runMessage msg l@(Barn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- getSetAsideCard Assets.ajax
      createAssetAt_ card (AtLocation attrs.id)
      codex iid attrs Omega
      pure l
    _ -> Barn <$> liftRunMessage msg attrs
