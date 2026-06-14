module Arkham.Location.Cards.DazzlingSkyline (dazzlingSkyline) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype DazzlingSkyline = DazzlingSkyline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dazzlingSkyline :: LocationCard DazzlingSkyline
dazzlingSkyline = location DazzlingSkyline Cards.dazzlingSkyline 1 (Static 1)

instance HasAbilities DazzlingSkyline where
  getAbilities (DazzlingSkyline a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , restricted a 2 (Here <> youExist can.spend.clues) actionAbility
      ]

instance RunMessage DazzlingSkyline where
  runMessage msg l@(DazzlingSkyline attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- Spend 1-3 clues, capped at what the investigator can actually pay.
      clues <- field InvestigatorClues iid
      chooseAmount iid "Clues" "Clues" 1 (min 3 clues) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      spendClues iid n
      -- TODO: Summit deck has no engine support. For each clue spent, reveal the
      -- bottom 3 cards of the Summit deck and place those cards on the top or
      -- bottom of the Summit deck in any order.
      pure l
    _ -> DazzlingSkyline <$> liftRunMessage msg attrs
