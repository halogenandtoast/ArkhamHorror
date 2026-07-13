module Arkham.Homebrew.CircusExMortis.Treacheries.LunarInfluence (lunarInfluence) where

import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LunarInfluence = LunarInfluence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarInfluence :: TreacheryCard LunarInfluence
lunarInfluence = treachery LunarInfluence Cards.lunarInfluence

instance RunMessage LunarInfluence where
  runMessage msg t@(LunarInfluence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      moons <- getSealedMoonTokens iid
      revelationSkillTest sid iid attrs #willpower (Fixed (3 + length moons))
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      randomDiscard iid attrs
      when (n >= 2) $ loseActions iid attrs 1
      pure t
    _ -> LunarInfluence <$> liftRunMessage msg attrs
