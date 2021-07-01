module Arkham.Types.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryCard BeyondTheVeil
beyondTheVeil = treachery BeyondTheVeil Cards.beyondTheVeil

instance HasModifiersFor env BeyondTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env BeyondTheVeil where
  getActions i window (BeyondTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode $ toCardCode attrs)
      t <$ if iid `member` exemptInvestigators
        then unshiftMessage (Discard $ toTarget attrs)
        else unshiftMessage
          (AttachTreachery treacheryId (InvestigatorTarget iid))
    DeckHasNoCards iid _ | treacheryOnInvestigator iid attrs ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 10 0
        , Discard $ toTarget attrs
        ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
