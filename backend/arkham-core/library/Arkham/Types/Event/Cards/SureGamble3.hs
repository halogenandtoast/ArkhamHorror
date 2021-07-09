module Arkham.Types.Event.Cards.SureGamble3 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Window
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust)

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance HasModifiersFor env SureGamble3 where
  getModifiersFor = noModifiersFor

instance HasActions env SureGamble3 where
  -- getActions iid (InHandWindow ownerId (WhenRevealTokenWithNegativeModifier You tid)) (SureGamble3 attrs)
  --   | ownerId == iid
  --   = pure
  --     [InitiatePlayCard iid (toCardId attrs) (Just $ TokenTarget tid) False]
  getActions i window (SureGamble3 attrs) = getActions i window attrs

instance EventRunner env => RunMessage env SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ windows | eid == eventId -> do
      let
        target = fromJust $ flip
          firstJust
          windows
          \window -> case windowType window of
            AfterRevealToken You TokenWithNegativeModifier ->
              windowTarget window
            _ -> Nothing
      e <$ pushAll
        [ CreateEffect "01088" Nothing (toSource attrs) target
        , Discard (toTarget attrs)
        ]
    _ -> SureGamble3 <$> runMessage msg attrs
