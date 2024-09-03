module Arkham.Treachery.Cards.DisquietingDreams (disquietingDreams, DisquietingDreams (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DisquietingDreams = DisquietingDreams TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disquietingDreams :: TreacheryCard DisquietingDreams
disquietingDreams = treachery DisquietingDreams Cards.disquietingDreams

instance HasAbilities DisquietingDreams where
  getAbilities (DisquietingDreams a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ forced $ TurnEnds #at You
    , restrictedAbility a 2 (InThreatAreaOf You) $ forced $ EncounterDeckRunsOutOfCards
    ]

instance RunMessage DisquietingDreams where
  runMessage msg t@(DisquietingDreams attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 1 (attrs.ability 1) Nothing
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      search iid attrs iid [(FromTopOfDeck 10, DiscardRest)] (basic WeaknessCard) (DrawFound iid 10)
      pure t
    _ -> DisquietingDreams <$> liftRunMessage msg attrs
