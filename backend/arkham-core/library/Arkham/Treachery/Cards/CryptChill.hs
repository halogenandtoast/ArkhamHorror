module Arkham.Treachery.Cards.CryptChill where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Runner

newtype CryptChill = CryptChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptChill :: TreacheryCard CryptChill
cryptChill = treachery CryptChill Cards.cryptChill

instance RunMessage CryptChill where
  runMessage msg t@(CryptChill attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        hasAssets <- selectAny (DiscardableAsset <> AssetControlledBy You)
        push $ if hasAssets
          then ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
          else InvestigatorAssignDamage iid source DamageAny 2 0
        pure t
    _ -> CryptChill <$> runMessage msg attrs
