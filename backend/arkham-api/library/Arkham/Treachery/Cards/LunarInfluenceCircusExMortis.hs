module Arkham.Treachery.Cards.LunarInfluenceCircusExMortis (lunarInfluenceCircusExMortis) where

import Arkham.Campaigns.CircusExMortis.Helpers
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LunarInfluenceCircusExMortis = LunarInfluenceCircusExMortis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarInfluenceCircusExMortis :: TreacheryCard LunarInfluenceCircusExMortis
lunarInfluenceCircusExMortis = treachery LunarInfluenceCircusExMortis Cards.lunarInfluenceCircusExMortis

instance RunMessage LunarInfluenceCircusExMortis where
  runMessage msg t@(LunarInfluenceCircusExMortis attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      moons <- getSealedMoonTokens iid
      revelationSkillTest sid iid attrs #willpower (Fixed (3 + length moons))
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      randomDiscard iid attrs
      when (n >= 2) $ loseActions iid attrs 1
      pure t
    _ -> LunarInfluenceCircusExMortis <$> liftRunMessage msg attrs
