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

amIReal :: TreacheryCard AmIReal
amIReal = treachery AmIReal Cards.amIReal

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
