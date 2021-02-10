module Arkham.Types.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  )
where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryId -> a -> BeyondTheVeil
beyondTheVeil uuid _ = BeyondTheVeil $ baseAttrs uuid "02084"

instance HasModifiersFor env BeyondTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env BeyondTheVeil where
  getActions i window (BeyondTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode treacheryCardCode)
      t <$ if iid `member` exemptInvestigators
        then unshiftMessage (Discard $ toTarget attrs)
        else unshiftMessage
          (AttachTreachery treacheryId (InvestigatorTarget iid))
    DeckHasNoCards iid | treacheryOnInvestigator iid attrs ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 10 0
        , Discard $ toTarget attrs
        ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
