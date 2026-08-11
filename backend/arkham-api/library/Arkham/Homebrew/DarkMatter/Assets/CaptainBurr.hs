module Arkham.Homebrew.DarkMatter.Assets.CaptainBurr (captainBurr) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype CaptainBurr = CaptainBurr AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captainBurr :: AssetCard CaptainBurr
captainBurr = asset CaptainBurr Cards.captainBurr

{- | "Captain Burr may be assigned direct damage and direct horror dealt to any
investigator and [[Ally]] asset at your location."
-}
instance HasModifiersFor CaptainBurr where
  getModifiersFor (CaptainBurr a) =
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
