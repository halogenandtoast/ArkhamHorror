module Arkham.Types.Treachery.Cards.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Metadata = Metadata { dealtDamageThisTurn :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CurseOfTheRougarou = CurseOfTheRougarou (TreacheryAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarou :: TreacheryCard CurseOfTheRougarou
curseOfTheRougarou = treachery (CurseOfTheRougarou . (`with` Metadata False)) Cards.curseOfTheRougarou

instance HasModifiersFor env CurseOfTheRougarou where
  getModifiersFor = noModifiersFor

instance HasActions env CurseOfTheRougarou where
  getActions iid window (CurseOfTheRougarou (attrs `With` _)) =
    getActions iid window attrs

instance (TreacheryRunner env) => RunMessage env CurseOfTheRougarou where
  runMessage msg t@(CurseOfTheRougarou (attrs@TreacheryAttrs {..} `With` metadata))
    = case msg of
      Revelation iid source | isSource attrs source -> do
        t <$ unshiftMessage
          (AttachTreachery treacheryId $ InvestigatorTarget iid)
      EnemyDamage _ iid _ n | treacheryOnInvestigator iid attrs && n > 0 ->
        CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      InvestigatorAssignDamage _ (InvestigatorSource iid) _ n 0
        | treacheryOnInvestigator iid attrs && n > 0
        -> CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      EndTurn iid | treacheryOnInvestigator iid attrs -> do
        unless
          (dealtDamageThisTurn metadata)
          (unshiftMessage
          $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
          )
        CurseOfTheRougarou . (`with` Metadata False) <$> runMessage msg attrs
      _ -> CurseOfTheRougarou . (`with` metadata) <$> runMessage msg attrs
