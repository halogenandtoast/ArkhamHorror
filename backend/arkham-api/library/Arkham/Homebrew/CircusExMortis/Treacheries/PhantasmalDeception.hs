module Arkham.Homebrew.CircusExMortis.Treacheries.PhantasmalDeception (phantasmalDeception) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, hasSealedMoonToken, moonToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.SkillType
import Arkham.Treachery.Import.Lifted

newtype PhantasmalDeception = PhantasmalDeception TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phantasmalDeception :: TreacheryCard PhantasmalDeception
phantasmalDeception = treachery PhantasmalDeception Cards.phantasmalDeception

instance HasModifiersFor PhantasmalDeception where
  getModifiersFor (PhantasmalDeception a) = do
    sealed <- select hasSealedMoonToken
    unless (null sealed) do
      investigators <- select Anyone
      for_ investigators \iid -> do
        here <- select $ InvestigatorAt (locationWithInvestigator iid) <> hasSealedMoonToken
        n <- sum <$> traverse (fmap length . getSealedMoonTokens) here
        modifiedWhen_ a (n > 0) iid [ActionSkillModifier #investigate sType (-n) | sType <- allSkills]

instance HasAbilities PhantasmalDeception where
  getAbilities (PhantasmalDeception a) =
    [mkAbility a 1 $ forced $ ChaosTokenReleased #after Anyone moonToken]

instance RunMessage PhantasmalDeception where
  runMessage msg t@(PhantasmalDeception attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> PhantasmalDeception <$> liftRunMessage msg attrs
