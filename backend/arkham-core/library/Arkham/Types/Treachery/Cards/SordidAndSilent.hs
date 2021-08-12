module Arkham.Types.Treachery.Cards.SordidAndSilent
  ( SordidAndSilent(..)
  , sordidAndSilent
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SordidAndSilent = SordidAndSilent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sordidAndSilent :: TreacheryCard SordidAndSilent
sordidAndSilent = treachery SordidAndSilent Cards.sordidAndSilent

instance TreacheryRunner env => RunMessage env SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      t <$ push (AttachTreachery treacheryId $ LocationTarget lid)
    EndRound -> case treacheryAttachedTarget of
      Just (LocationTarget lid) -> do
        iids <- getSetList @InvestigatorId lid
        t <$ pushAll
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
          | iid <- iids
          ]
      _ -> pure t
    AdvanceAgenda _ -> t <$ push (Discard $ toTarget attrs)
    _ -> SordidAndSilent <$> runMessage msg attrs
