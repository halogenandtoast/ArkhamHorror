module Arkham.Asset.Assets.K2PS187100FunctionalityDarkMatter (k2PS187100FunctionalityDarkMatter) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.DarkMatter.Helpers (ScanResult (..))
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype K2PS187100FunctionalityDarkMatter = K2PS187100FunctionalityDarkMatter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS187100FunctionalityDarkMatter :: AssetCard K2PS187100FunctionalityDarkMatter
k2PS187100FunctionalityDarkMatter = asset K2PS187100FunctionalityDarkMatter Cards.k2PS187100FunctionalityDarkMatter

instance HasAbilities K2PS187100FunctionalityDarkMatter where
  getAbilities (K2PS187100FunctionalityDarkMatter a) =
    [ playerLimit PerRound
        $ controlled a 1 (DuringPhase #investigation) (FastAbility Free)
    , playerLimit PerRound
        $ restricted a 2 ControlsThis (freeReaction (ScenarioEvent #after Nothing "scan"))
    ]

getScanResult :: [Window] -> ScanResult
getScanResult = \case
  [] -> error "missing scan result"
  ((windowType -> Window.ScenarioEvent "scan" _ v) : _) -> toResult v
  (_ : xs) -> getScanResult xs

instance RunMessage K2PS187100FunctionalityDarkMatter where
  runMessage msg a@(K2PS187100FunctionalityDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction #scan
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let iid' = scannedBy (getScanResult ws)
      chooseOneM iid' do
        (withI18n $ countVar 1 $ labeled' "drawCards") $ drawCards iid' (attrs.ability 2) 1
        (withI18n $ countVar 1 $ labeled' "gainResources") $ gainResources iid' (attrs.ability 2) 1
      pure a
    _ -> K2PS187100FunctionalityDarkMatter <$> liftRunMessage msg attrs
