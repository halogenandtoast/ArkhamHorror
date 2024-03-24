module Arkham.Treachery.Cards.DeceptiveMemories (deceptiveMemories, DeceptiveMemories (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
  runMessage msg t@(DeceptiveMemories attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ chooseAndDiscardCard iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid (attrs.ability 2) iid #willpower 3
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid (toSource attrs) attrs
      pure t
    _ -> DeceptiveMemories <$> runMessage msg attrs
