{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beyondTheVeil :: TreacheryId -> a -> BeyondTheVeil
beyondTheVeil uuid _ = BeyondTheVeil $ baseAttrs uuid "02084"

instance HasModifiersFor env BeyondTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env BeyondTheVeil where
  getActions i window (BeyondTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode treacheryCardCode)
      t <$ if iid `member` exemptInvestigators
        then unshiftMessage (Discard $ toTarget attrs)
        else unshiftMessage
          (AttachTreachery treacheryId (InvestigatorTarget iid))
    DeckHasNoCards iid | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (toSource attrs) 10 0
        , Discard $ toTarget attrs
        ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
