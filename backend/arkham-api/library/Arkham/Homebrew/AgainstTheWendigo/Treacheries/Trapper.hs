module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Trapper (trapper) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype Trapper = Trapper TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapper :: TreacheryCard Trapper
trapper = treachery Trapper Cards.trapper

instance RunMessage Trapper where
  runMessage msg t@(Trapper attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "Test [intellect] (2). If you are in a Wild location, test [intellect] (4) instead."
      inTheWild <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Wild
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed $ if inTheWild then 4 else 2)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      loseResources iid attrs n
      pure t
    _ -> Trapper <$> liftRunMessage msg attrs
