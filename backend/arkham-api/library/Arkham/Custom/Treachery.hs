-- | The runner behind a debug-authored custom treachery. See "Arkham.Custom.Enemy".
module Arkham.Custom.Treachery (CustomTreachery (..), customTreachery) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  isCustomAbility,
  runCustomAbility,
  runCustomHandlers,
  runCustomRevelation,
  pattern ZonedUseThisAbility,
 )
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
    ZonedUseThisAbility iid (isSource attrs -> True) idx ws payment | isCustomAbility attrs idx -> do
      runCustomAbility attrs iid idx ws payment
      pure x
    -- What it does when it is revealed. Not an ability: no one activates it, and
    -- the card may have to place itself before the engine tidies it away.
    Revelation iid (isSource attrs -> True) -> do
      runCustomRevelation attrs iid
      runCustomHandlers attrs msg
      CustomTreachery <$> liftRunMessage msg attrs
    _ -> do
      runCustomHandlers attrs msg
      CustomTreachery <$> liftRunMessage msg attrs
