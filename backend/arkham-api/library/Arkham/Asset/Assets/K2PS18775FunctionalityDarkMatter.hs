module Arkham.Asset.Assets.K2PS18775FunctionalityDarkMatter (k2PS18775FunctionalityDarkMatter) where

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

newtype K2PS18775FunctionalityDarkMatter = K2PS18775FunctionalityDarkMatter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18775FunctionalityDarkMatter :: AssetCard K2PS18775FunctionalityDarkMatter
k2PS18775FunctionalityDarkMatter = asset K2PS18775FunctionalityDarkMatter Cards.k2PS18775FunctionalityDarkMatter

instance HasAbilities K2PS18775FunctionalityDarkMatter where
  getAbilities (K2PS18775FunctionalityDarkMatter a) =
    [ playerLimit PerRound
        $ controlled a 1 (DuringPhase #investigation) (FastAbility Free)
    , -- TODO(homebrew): the "scan" ScenarioEvent window carries success/failure only
      -- in its decoded payload; there is no window-matcher primitive to filter a
      -- ScenarioEvent by its value, so this reaction is offered after every scan and
      -- guarded in the handler (it does nothing on a successful scan). A faithful fix
      -- needs either a distinct unsuccessful-scan window key or a payload predicate on
      -- the ScenarioEvent matcher.
      restricted a 2 ControlsThis $ freeReaction (ScenarioEvent #after Nothing "scan")
    ]

getScanResult :: [Window] -> ScanResult
getScanResult = \case
  [] -> error "missing scan result"
  ((windowType -> Window.ScenarioEvent "scan" _ v) : _) -> toResult v
  (_ : xs) -> getScanResult xs

instance RunMessage K2PS18775FunctionalityDarkMatter where
  runMessage msg a@(K2PS18775FunctionalityDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction #scan
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let r = getScanResult ws
      unless (scanSuccessful r) do
        let iid' = scannedBy r
        chooseOneM iid' do
          (withI18n $ countVar 1 $ labeled' "drawCards") $ drawCards iid' (attrs.ability 2) 1
          (withI18n $ countVar 1 $ labeled' "gainResources") $ gainResources iid' (attrs.ability 2) 1
      pure a
    _ -> K2PS18775FunctionalityDarkMatter <$> liftRunMessage msg attrs
