module Arkham.Event.Cards.Riastrad1 (riastrad1, Riastrad1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag

newtype Riastrad1 = Riastrad1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riastrad1 :: EventCard Riastrad1
riastrad1 = event Riastrad1 Cards.riastrad1

instance RunMessage Riastrad1 where
  runMessage msg e@(Riastrad1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      n <- min 3 <$> getRemainingCurseTokens
      when (n > 0) do
        chooseAmount
          iid
          "Add {curse} tokens to chaos bag"
          "Curse Tokens"
          0
          n
          (ProxyTarget (toTarget sid) (toTarget attrs))
      chooseFightEnemy sid iid attrs
      pure e
    ResolveAmounts
      iid
      (getChoiceAmount "Curse Tokens" -> n)
      (ProxyTarget (SkillTestTarget sid) (isTarget attrs -> True)) | n > 0 -> do
        replicateM_ n $ addChaosToken #curse
        skillTestModifiers sid attrs iid [#combat n, #damage n]
        pure e
    _ -> Riastrad1 <$> liftRunMessage msg attrs
