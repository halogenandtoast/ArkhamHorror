module Arkham.Treachery.Cards.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Investigator.Attrs
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Cards qualified as Cards

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor m, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

-- TODO: Move move to effect to a modifier...
instance RunMessage ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid
        $ \lid -> push $ AttachTreachery (toId attrs) (LocationTarget lid)
      pure t
    Will (MoveTo _ iid lid) -> do
      mInvestigatorLocation <- field InvestigatorLocation iid
      when
          (treacheryOnLocation lid attrs
          || maybe False (`treacheryOnLocation` attrs) mInvestigatorLocation
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
