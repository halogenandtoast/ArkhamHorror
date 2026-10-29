module Arkham.Homebrew.AgesUnwound.Treacheries.UnwrittenExistence (unwrittenExistence) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (timeRunsOutI18n)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (recordForInvestigator)
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype UnwrittenExistence = UnwrittenExistence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unwrittenExistence :: TreacheryCard UnwrittenExistence
unwrittenExistence = treachery UnwrittenExistence Cards.unwrittenExistence

instance HasAbilities UnwrittenExistence where
  getAbilities (UnwrittenExistence a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage UnwrittenExistence where
  runMessage msg t@(UnwrittenExistence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      lead <- getLead
      chooseOneM lead $ timeRunsOutI18n $ scope "unwrittenExistence" do
        labeled "putIntoPlay" do
          placeInThreatArea attrs iid
          recordForInvestigator iid ExistenceIsWaning
        labeled "removeFromGame" do
          removeFromGame attrs
          placeDoomOnAgendaAndCheckAdvance 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      top <- fieldMap InvestigatorDeck (take n . (.cards)) iid
      for_ top \pc -> do
        obtainCard (PlayerCard pc)
        exile (PlayerCard pc)
      directDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    _ -> UnwrittenExistence <$> liftRunMessage msg attrs
