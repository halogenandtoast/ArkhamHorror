{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Metadata = Metadata { dealtDamageThisTurn :: Bool }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CurseOfTheRougarou = CurseOfTheRougarou (Attrs `With` Metadata)
  deriving newtype (Show, ToJSON, FromJSON)

curseOfTheRougarou :: TreacheryId -> Maybe InvestigatorId -> CurseOfTheRougarou
curseOfTheRougarou uuid iid =
  CurseOfTheRougarou . (`with` Metadata False) $ weaknessAttrs uuid iid "81029"

instance HasModifiersFor env CurseOfTheRougarou where
  getModifiersFor = noModifiersFor

instance HasActions env CurseOfTheRougarou where
  getActions iid window (CurseOfTheRougarou (attrs `With` _)) =
    getActions iid window attrs

instance (TreacheryRunner env) => RunMessage env CurseOfTheRougarou where
  runMessage msg (CurseOfTheRougarou (attrs@Attrs {..} `With` metadata)) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
        CurseOfTheRougarou . (`with` metadata) <$> runMessage
          msg
          (attrs & attachedInvestigator ?~ iid)
      EnemyDamage _ iid _ n
        | Just iid == treacheryAttachedInvestigator && n > 0
        -> CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      InvestigatorAssignDamage _ (InvestigatorSource iid) n 0
        | Just iid == treacheryAttachedInvestigator && n > 0
        -> CurseOfTheRougarou . (`with` Metadata True) <$> runMessage msg attrs
      EndTurn iid -> do
        unless
          (dealtDamageThisTurn metadata)
          (unshiftMessage $ InvestigatorAssignDamage iid (toSource attrs) 0 1)
        CurseOfTheRougarou . (`with` Metadata False) <$> runMessage msg attrs
      _ -> CurseOfTheRougarou . (`with` metadata) <$> runMessage msg attrs
