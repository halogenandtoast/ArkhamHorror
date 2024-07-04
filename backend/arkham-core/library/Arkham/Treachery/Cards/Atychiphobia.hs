module Arkham.Treachery.Cards.Atychiphobia (atychiphobia, Atychiphobia (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Atychiphobia = Atychiphobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atychiphobia :: TreacheryCard Atychiphobia
atychiphobia = treachery Atychiphobia Cards.atychiphobia

instance HasAbilities Atychiphobia where
  getAbilities (Atychiphobia a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest #failure
    , restrictedAbility a 2 (InThreatAreaOf $ InvestigatorAt YourLocation)
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage Atychiphobia where
  runMessage msg t@(Atychiphobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Atychiphobia <$> liftRunMessage msg attrs
