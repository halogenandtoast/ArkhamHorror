module Arkham.Types.Treachery.Cards.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

instance HasModifiersFor env ArcaneBarrier where
  getModifiersFor = noModifiersFor

instance HasActions env ArcaneBarrier where
  getActions i window (ArcaneBarrier attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ unshiftMessage (AttachTreachery (toId attrs) (LocationTarget lid))
    Will (MoveTo iid lid) -> do
      investigatorLocation <- getId iid
      when
          (treacheryOnLocation lid attrs
          || treacheryOnLocation investigatorLocation attrs
          )
        $ do
            moveFromMessage <- fromJustNote "missing move from" <$> popMessage
            moveToMessage <- fromJustNote "missing move to" <$> popMessage
            unshiftMessage
              (CreateEffect
                (CardCode "02102")
                (Just (EffectMessages [moveFromMessage, moveToMessage]))
                (toSource attrs)
                (InvestigatorTarget iid)
              )
      pure t
    _ -> ArcaneBarrier <$> runMessage msg attrs
