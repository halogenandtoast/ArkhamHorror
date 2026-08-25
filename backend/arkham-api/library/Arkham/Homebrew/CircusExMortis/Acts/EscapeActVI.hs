module Arkham.Homebrew.CircusExMortis.Acts.EscapeActVI (escapeActVI) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.EscapeAct (escapeActAdvance)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations

newtype EscapeActVI = EscapeActVI ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeActVI :: ActCard EscapeActVI
escapeActVI = act (1, A) EscapeActVI Cards.escapeActVI Nothing

instance HasAbilities EscapeActVI where
  getAbilities = actAbilities1 \a -> restricted a 1 NoRestriction $ Objective $ FastAbility Free

instance RunMessage EscapeActVI where
  runMessage msg a@(EscapeActVI attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      escapeActAdvance attrs Locations.campOutskirtsGuardedClosely
      pure a
    _ -> EscapeActVI <$> liftRunMessage msg attrs
