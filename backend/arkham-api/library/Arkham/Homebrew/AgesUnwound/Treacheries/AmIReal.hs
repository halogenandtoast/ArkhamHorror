module Arkham.Homebrew.AgesUnwound.Treacheries.AmIReal (amIReal) where

import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype AmIReal = AmIReal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | __Surge__ is a printed keyword and comes off the card def.
amIReal :: TreacheryCard AmIReal
amIReal = treachery AmIReal Cards.amIReal

{- | "__Surge__. __Revelation__ - If 'your existence is waning', test [willpower]
(6). For each point you fail by, reveal the top card of your deck, drawing each
revealed weakness and exiling each other revealed card. If you fail by 3 or more,
also take 1 direct horror and 1 mental trauma."

An investigator whose existence is not waning has no test to make, so the card
surges and nothing else happens.

Each point reveals a card and then does the one thing that card's type dictates
-- there is no choice to make per point -- so the loop is a flat sequence of
'DoStep' pushes rather than the 'doNextStep' countdown a per-point /choice/ would
need. They resolve in order, which matters: each one must read the deck as the
previous one left it.
-}
instance RunMessage AmIReal where
  runMessage msg t@(AmIReal attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      waning <- iid <=~> investigatorWithRecord ExistenceIsWaning
      when waning do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 6)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      for_ [1 .. n] \_ -> doStep 1 msg
      when (n >= 3) do
        directHorror iid attrs 1
        sufferMentalTrauma iid 1
      pure t
    DoStep 1 (FailedThisSkillTestBy iid (isSource attrs -> True) _) -> do
      top <- fieldMap InvestigatorDeck (take 1 . (.cards)) iid
      for_ top \pc -> do
        let card = PlayerCard pc
        if card `cardMatch` WeaknessCard
          then drawCard iid card
          else do
            obtainCard card
            exile card
      pure t
    _ -> AmIReal <$> liftRunMessage msg attrs
