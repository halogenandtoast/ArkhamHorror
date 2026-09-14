module Arkham.Homebrew.DarkMatter.Assets.CaptainBurr (captainBurr) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype CaptainBurr = CaptainBurr AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captainBurr :: AssetCard CaptainBurr
captainBurr = ally CaptainBurr Cards.captainBurr (2, 2)

{- | "Captain Burr may be assigned direct damage and direct horror dealt to any
investigator and [[Ally]] asset at your location."

Two halves: the investigators at his location have to be allowed to soak onto
him at all ('CanAssign*ToAsset', which is otherwise only true of assets they
control), and he has to be one of the assets *direct* damage may be assigned to
('CanBeAssignedDirectDamage' — 'handleInvestigatorDirectDamage' narrows the
assignable assets to exactly that modifier).

TODO(homebrew): the "and [[Ally]] asset" half is not covered. Damage dealt
straight to an asset has no redirect seam in the engine ('DealAssetDirectDamage'
places it immediately, and only 'CanAssignDamageToInvestigator' can divert asset
damage, to an investigator).
-}
instance HasModifiersFor CaptainBurr where
  getModifiersFor (CaptainBurr a) = do
    modifySelf a [CanBeAssignedDirectDamage]
    modifySelect
      a
      (InvestigatorAt $ locationWithAsset a.id)
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance RunMessage CaptainBurr where
  runMessage msg a@(CaptainBurr attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> CaptainBurr <$> liftRunMessage msg attrs
