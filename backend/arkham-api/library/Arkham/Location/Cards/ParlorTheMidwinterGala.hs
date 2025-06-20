module Arkham.Location.Cards.ParlorTheMidwinterGala (parlorTheMidwinterGala) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Guest))

newtype ParlorTheMidwinterGala = ParlorTheMidwinterGala LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorTheMidwinterGala :: LocationCard ParlorTheMidwinterGala
parlorTheMidwinterGala = location ParlorTheMidwinterGala Cards.parlorTheMidwinterGala 6 (PerPlayer 3)

instance HasModifiersFor ParlorTheMidwinterGala where
  getModifiersFor (ParlorTheMidwinterGala a) = do
    guests <- selectCount $ assetAt a <> AssetWithTrait Guest
    -- TODO: this should not lower the shroud lower than 1, the 5 min is a
    -- guard against this however we might have other cards that could do this
    modifySelf a [ShroudModifier (-(min guests 5))]

instance HasAbilities ParlorTheMidwinterGala where
  getAbilities (ParlorTheMidwinterGala a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> youExist (ControlsAsset $ assetIs Assets.jewelOfSarnath) <> exists ReadyEnemy)
      $ FastAbility Free

instance RunMessage ParlorTheMidwinterGala where
  runMessage msg l@(ParlorTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select ReadyEnemy
      chooseTargetM iid enemies exhaustThis
      pure l
    _ -> ParlorTheMidwinterGala <$> liftRunMessage msg attrs
