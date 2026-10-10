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

{- | /Unwritten Existence/, the reverse of the @:ages-unwound:192@ printing of
/Days That Never Were/.
-}
unwrittenExistence :: TreacheryCard UnwrittenExistence
unwrittenExistence = treachery UnwrittenExistence Cards.unwrittenExistence

{- | "__Forced__ - At the end of your turn: Exile the top 1[per_investigator] cards
of your deck. Take 1 direct damage and 1 direct horror."
-}
instance HasAbilities UnwrittenExistence where
  getAbilities (UnwrittenExistence a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

{- | "__Revelation__ - The lead investigator must decide (choose one):
-- Put this card into play in your threat area.
-- Remove this card from the game. Place 2 doom on the current agenda. This effect
may cause the current agenda to advance.
/Your existence is waning./

The italic line is the card's standing condition, so taking it into a threat area
is what records /\<investigator\>'s existence is waning/ -- the entry Resolution 2
reports and that /Am I... Real?/, /Erasure/ and /Oblivion Beckons/ read. "Your
threat area" is the drawing investigator's even though the lead makes the call;
that is the ordinary reading of "you" on a treachery.
-}
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
      directDamage iid (attrs.ability 1) 1
      directHorror iid (attrs.ability 1) 1
      pure t
    _ -> UnwrittenExistence <$> liftRunMessage msg attrs
