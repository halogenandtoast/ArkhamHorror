module Arkham.Homebrew.TheInvisibleMan.Treacheries.SenselessMurder (senselessMurder) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype SenselessMurder = SenselessMurder TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

senselessMurder :: TreacheryCard SenselessMurder
senselessMurder = treachery SenselessMurder Cards.senselessMurder

instance HasModifiersFor SenselessMurder where
  getModifiersFor (SenselessMurder a) = for_ a.attached.location \lid ->
    modifySelect a (investigatorAt lid) [ControlledAssetsCannotReady]

instance HasAbilities SenselessMurder where
  getAbilities (SenselessMurder a) = case a.attached.location of
    Nothing -> []
    Just lid ->
      [ scenarioI18n
          $ withI18nTooltip "senselessMurder.discard"
          $ restricted (proxied lid a) 1 Here doubleActionAbility
      ]

instance RunMessage SenselessMurder where
  runMessage msg t@(SenselessMurder attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> assetControlledBy iid
      chooseOneM iid do
        targets allies $ push . Msg.AssetDefeated (toSource attrs)
        scenarioI18n
          $ scope "senselessMurder"
          $ labeled "attach"
          $ withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SenselessMurder <$> liftRunMessage msg attrs
