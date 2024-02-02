module Arkham.Treachery.Cards.CryptChill where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CryptChill = CryptChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cryptChill :: TreacheryCard CryptChill
cryptChill = treachery CryptChill Cards.cryptChill

instance RunMessage CryptChill where
  runMessage msg t@(CryptChill attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower 4
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      push
        $ if hasAssets
          then ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
          else assignDamage iid attrs 2
      pure t
    _ -> CryptChill <$> runMessage msg attrs
