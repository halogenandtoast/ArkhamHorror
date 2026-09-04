-- | The runner behind a debug-authored custom treachery. See "Arkham.Custom.Enemy".
module Arkham.Custom.Treachery (CustomTreachery (..), customTreachery) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (customAbilities, customModifiers, runCustomAbility, runCustomHandlers)
import Arkham.Treachery.Import.Lifted

newtype CustomTreachery = CustomTreachery TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customTreachery :: CardDef -> TreacheryCard CustomTreachery
customTreachery = treachery CustomTreachery

instance HasModifiersFor CustomTreachery where
  getModifiersFor (CustomTreachery a) = customModifiers a

instance HasAbilities CustomTreachery where
  getAbilities (CustomTreachery a) = customAbilities a

instance RunMessage CustomTreachery where
  runMessage msg x@(CustomTreachery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx -> do
      runCustomAbility attrs iid idx
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomTreachery <$> liftRunMessage msg attrs
