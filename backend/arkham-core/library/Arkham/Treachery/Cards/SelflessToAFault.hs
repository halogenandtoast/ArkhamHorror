module Arkham.Treachery.Cards.SelflessToAFault (selflessToAFault, SelflessToAFault (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SelflessToAFault = SelflessToAFault TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selflessToAFault :: TreacheryCard SelflessToAFault
selflessToAFault = treachery SelflessToAFault Cards.selflessToAFault

instance HasAbilities SelflessToAFault where
  getAbilities (SelflessToAFault x) =
    [ restrictedAbility x 1 (InThreatAreaOf (You <> InvestigatorWithMetaKey "wasNotSelfless"))
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage SelflessToAFault where
  runMessage msg t@(SelflessToAFault attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    _ -> SelflessToAFault <$> liftRunMessage msg attrs
