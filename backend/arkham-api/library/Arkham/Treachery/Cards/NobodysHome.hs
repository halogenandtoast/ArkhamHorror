module Arkham.Treachery.Cards.NobodysHome (nobodysHome) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NobodysHome = NobodysHome TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nobodysHome :: TreacheryCard NobodysHome
nobodysHome = treachery NobodysHome Cards.nobodysHome

instance HasModifiersFor NobodysHome where
  getModifiersFor (NobodysHome attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [AdditionalCostToInvestigate (ActionCost 1)]
    _ -> pure ()

instance HasAbilities NobodysHome where
  getAbilities (NobodysHome a) = case a.attached.location of
    Just lid -> [restricted a 1 (exists $ be lid <> LocationWithoutClues) $ forced AnyWindow]
    _ -> []

instance RunMessage NobodysHome where
  runMessage msg t@(NobodysHome attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        whenM (fieldNone LocationClues lid) $ gainSurge attrs
        attachTreachery attrs lid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> NobodysHome <$> liftRunMessage msg attrs
