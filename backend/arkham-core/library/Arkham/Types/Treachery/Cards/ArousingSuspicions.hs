{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.ArousingSuspicions
  ( ArousingSuspicions(..)
  , arousingSuspicions
  )
where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArousingSuspicions = ArousingSuspicions Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arousingSuspicions :: TreacheryId -> a -> ArousingSuspicions
arousingSuspicions uuid _ = ArousingSuspicions $ baseAttrs uuid "02082"

instance HasModifiersFor env ArousingSuspicions where
  getModifiersFor = noModifiersFor

instance HasActions env ArousingSuspicions where
  getActions i window (ArousingSuspicions attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ArousingSuspicions where
  runMessage msg t@(ArousingSuspicions attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList @EnemyId ([Criminal], lid)
      if null criminals
        then t
          <$ unshiftMessages [SpendResources iid 2, Discard $ toTarget attrs]
        else t <$ unshiftMessages
          ([ PlaceDoom (EnemyTarget eid) 1 | eid <- criminals ]
          <> [Discard $ toTarget attrs]
          )
    _ -> ArousingSuspicions <$> runMessage msg attrs
