module Arkham.Types.Treachery.Cards.CryptChill where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CryptChill = CryptChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptChill :: TreacheryCard CryptChill
cryptChill = treachery CryptChill Cards.cryptChill

instance TreacheryRunner env => RunMessage env CryptChill where
  runMessage msg t@(CryptChill attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RevelationSkillTest iid source SkillWillpower 4
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        assetCount <- length <$> select (DiscardableAsset <> AssetOwnedBy You)
        if assetCount > 0
          then t <$ push (ChooseAndDiscardAsset iid AnyAsset)
          else t <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> CryptChill <$> runMessage msg attrs
