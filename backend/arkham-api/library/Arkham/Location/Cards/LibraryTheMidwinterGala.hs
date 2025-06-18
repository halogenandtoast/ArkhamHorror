module Arkham.Location.Cards.LibraryTheMidwinterGala (libraryTheMidwinterGala) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isParley)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype LibraryTheMidwinterGala = LibraryTheMidwinterGala LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryTheMidwinterGala :: LocationCard LibraryTheMidwinterGala
libraryTheMidwinterGala = location LibraryTheMidwinterGala Cards.libraryTheMidwinterGala 4 (PerPlayer 3)

instance HasModifiersFor LibraryTheMidwinterGala where
  getModifiersFor (LibraryTheMidwinterGala a) = do
    getSkillTestInvestigator >>= traverse_ \iid -> do
      maybeModified_ a iid do
        guardM isParley
        pure [AnySkillValue 1]

instance HasAbilities LibraryTheMidwinterGala where
  getAbilities (LibraryTheMidwinterGala a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists (assetIs Assets.jewelOfSarnath <> at_ (be a)))
      $ FastAbility Free

instance RunMessage LibraryTheMidwinterGala where
  runMessage msg l@(LibraryTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      jewelOfSarnath <- selectJust $ assetIs Assets.jewelOfSarnath
      tokens <- field AssetTokens jewelOfSarnath

      chooseOrRunOneM iid $ withI18n do
        countVar 1
          $ nameVar Assets.jewelOfSarnath
          $ labeled' "placeDamageOn"
          $ placeTokens (attrs.ability 1) jewelOfSarnath #damage 1
        for_ (keys tokens) \token -> do
          countVar 1
            $ withVar "token" (String $ tshow token)
            $ nameVar Assets.jewelOfSarnath
            $ labeled' "removeTokensFrom"
            $ removeTokens (attrs.ability 1) jewelOfSarnath token 1

      pure l
    _ -> LibraryTheMidwinterGala <$> liftRunMessage msg attrs
