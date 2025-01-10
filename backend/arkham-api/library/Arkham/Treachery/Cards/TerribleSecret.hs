module Arkham.Treachery.Cards.TerribleSecret (terribleSecret) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerribleSecret = TerribleSecret TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terribleSecret :: TreacheryCard TerribleSecret
terribleSecret = treachery TerribleSecret Cards.terribleSecret

instance RunMessage TerribleSecret where
  runMessage msg t@(TerribleSecret attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      if null cardsUnderneath
        then shuffleIntoDeck iid attrs
        else focusCards cardsUnderneath do
          chooseUpToNM iid (length cardsUnderneath) "Keep Remaining Cards" do
            for_ cardsUnderneath \pc -> do
              for_ (preview _PlayerCard pc) \c -> targeting c (discardCard iid attrs c)
          unfocusCards
          doStep 1 msg
      pure t
    DoStep 1 (Revelation iid source) | isSource attrs source -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      assignHorror iid attrs (length cardsUnderneath)
      pure t
    _ -> TerribleSecret <$> liftRunMessage msg attrs
