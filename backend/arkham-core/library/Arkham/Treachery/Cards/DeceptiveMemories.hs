module Arkham.Treachery.Cards.DeceptiveMemories (deceptiveMemories, DeceptiveMemories (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeceptiveMemories = DeceptiveMemories TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deceptiveMemories :: TreacheryCard DeceptiveMemories
deceptiveMemories = treachery DeceptiveMemories Cards.deceptiveMemories

instance HasAbilities DeceptiveMemories where
  getAbilities (DeceptiveMemories a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ EntersThreatArea #after You (NotCard $ CardWithId $ toCardId a)
    , restrictedAbility a 2 Here actionAbility
    ]

instance RunMessage DeceptiveMemories where
  runMessage msg t@(DeceptiveMemories attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> DeceptiveMemories <$> liftRunMessage msg attrs
