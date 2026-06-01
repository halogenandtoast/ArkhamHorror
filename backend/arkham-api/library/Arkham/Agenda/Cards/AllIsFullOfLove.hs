module Arkham.Agenda.Cards.AllIsFullOfLove (allIsFullOfLove) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher

newtype AllIsFullOfLove = AllIsFullOfLove AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsFullOfLove :: AgendaCard AllIsFullOfLove
allIsFullOfLove = agenda (1, A) AllIsFullOfLove Cards.allIsFullOfLove (Static 6)

instance HasAbilities AllIsFullOfLove where
  getAbilities (AllIsFullOfLove attrs) =
    [mkAbility attrs 1 $ forced $ DrawCard #when You (basic WeaknessCard) AnyDeck]

instance RunMessage AllIsFullOfLove where
  runMessage msg a@(AllIsFullOfLove attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      bertieHadAnEpiphany <- getHasRecord BertieHadAnEpiphany
      push $ if bertieHadAnEpiphany then R2 else R1
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> weakness) _ -> do
      cancelCardEffects attrs weakness
      quietCancelCardDraw weakness
      discardCard iid attrs weakness
      pure a
    _ -> AllIsFullOfLove <$> liftRunMessage msg attrs
