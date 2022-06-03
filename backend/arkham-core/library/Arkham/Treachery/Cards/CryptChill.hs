module Arkham.Treachery.Cards.CryptChill where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype CryptChill = CryptChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptChill :: TreacheryCard CryptChill
cryptChill = treachery CryptChill Cards.cryptChill

instance TreacheryRunner env => RunMessage CryptChill where
  runMessage msg t@(CryptChill attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        assetCount <- length <$> select (DiscardableAsset <> AssetControlledBy You)
        if assetCount > 0
          then t <$ push (ChooseAndDiscardAsset iid AnyAsset)
          else t <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> CryptChill <$> runMessage msg attrs
