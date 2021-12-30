module Arkham.Treachery.Cards.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

-- TODO: Move move to effect to a modifier...
instance TreacheryRunner env => RunMessage env ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ push (AttachTreachery (toId attrs) (LocationTarget lid))
    Will (MoveTo _ iid lid) -> do
      investigatorLocation <- getId iid
      when
          (treacheryOnLocation lid attrs
          || treacheryOnLocation investigatorLocation attrs
          )
        $ do
            moveFromMessage <- fromJustNote "missing move from" <$> popMessage
            moveToMessage <- fromJustNote "missing move to" <$> popMessage
            push
              (CreateEffect
                (CardCode "02102")
                (Just (EffectMessages [moveFromMessage, moveToMessage]))
                (toSource attrs)
                (InvestigatorTarget iid)
              )
      pure t
    _ -> ArcaneBarrier <$> runMessage msg attrs
