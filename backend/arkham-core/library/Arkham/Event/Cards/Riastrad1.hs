module Arkham.Event.Cards.Riastrad1 (riastrad1, Riastrad1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Modifier

newtype Riastrad1 = Riastrad1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riastrad1 :: EventCard Riastrad1
riastrad1 = event Riastrad1 Cards.riastrad1

instance RunMessage Riastrad1 where
  runMessage msg e@(Riastrad1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      n <- min 3 <$> getRemainingCurseTokens
      when (n > 0) $ chooseAmount iid "Add {curse} tokens to chaos bag" "Curse Tokens" 0 n attrs
      chooseFightEnemy iid attrs
      pure e
    ResolveAmounts iid (getChoiceAmount "Curse Tokens" -> n) (isTarget attrs -> True) | n > 0 -> do
      replicateM_ n $ addChaosToken #curse
      skillTestModifiers attrs iid [#combat n, #damage n]
      pure e
    _ -> Riastrad1 <$> lift (runMessage msg attrs)
