module Arkham.Homebrew.DarkMatter.Assets.K2PS18775Functionality (k2PS18775Functionality) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), scanEvent)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype K2PS18775Functionality = K2PS18775Functionality AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18775Functionality :: AssetCard K2PS18775Functionality
k2PS18775Functionality = asset K2PS18775Functionality Cards.k2PS18775Functionality

instance HasAbilities K2PS18775Functionality where
  getAbilities (K2PS18775Functionality a) =
    [ playerLimit PerRound
        $ controlled a 1 (DuringPhase #investigation) (FastAbility Free)
    , -- TODO(homebrew): success/failure is only in the decoded payload, not the
      -- window key, so this reaction is offered after every scan and guarded in the
      -- handler (it does nothing on a successful scan). 'scanEventFor' shows the
      -- shape of the fix: fire a distinct key, e.g. @scan[unsuccessful]@, the way
      -- the per-icon windows do.
      restricted a 2 ControlsThis $ freeReaction (CampaignEvent #after Nothing scanEvent)
    ]

getScanResult :: [Window] -> ScanResult
getScanResult = \case
  [] -> error "missing scan result"
  ((windowType -> Window.CampaignEvent k _ v) : _) | k == scanEvent -> toResult v
  (_ : xs) -> getScanResult xs

instance RunMessage K2PS18775Functionality where
  runMessage msg a@(K2PS18775Functionality attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction Scan
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      let r = getScanResult ws
      unless (scanSuccessful r) do
        let iid' = scannedBy r
        chooseOneM iid' do
          (withI18n $ countVar 1 $ labeled' "drawCards") $ drawCards iid' (attrs.ability 2) 1
          (withI18n $ countVar 1 $ labeled' "gainResources") $ gainResources iid' (attrs.ability 2) 1
      pure a
    _ -> K2PS18775Functionality <$> liftRunMessage msg attrs
