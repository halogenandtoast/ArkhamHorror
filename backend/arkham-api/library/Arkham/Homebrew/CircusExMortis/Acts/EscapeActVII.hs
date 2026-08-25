module Arkham.Homebrew.CircusExMortis.Acts.EscapeActVII (escapeActVII) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.EscapeAct (escapeActAdvance)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations

newtype EscapeActVII = EscapeActVII ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeActVII :: ActCard EscapeActVII
escapeActVII = act (1, A) EscapeActVII Cards.escapeActVII Nothing

instance HasAbilities EscapeActVII where
  getAbilities = actAbilities1 \a -> restricted a 1 NoRestriction $ Objective $ FastAbility Free

instance RunMessage EscapeActVII where
  runMessage msg a@(EscapeActVII attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      escapeActAdvance attrs Locations.campOutskirtsQuietForNow
      pure a
    _ -> EscapeActVII <$> liftRunMessage msg attrs
