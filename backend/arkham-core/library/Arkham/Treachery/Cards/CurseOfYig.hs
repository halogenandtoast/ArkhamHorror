module Arkham.Treachery.Cards.CurseOfYig (curseOfYig, CurseOfYig (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Prelude
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CurseOfYig = CurseOfYig TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfYig :: TreacheryCard CurseOfYig
curseOfYig = treachery CurseOfYig Cards.curseOfYig

instance HasModifiersFor CurseOfYig where
  getModifiersFor (InvestigatorTarget iid) (CurseOfYig attrs) | attrs `on` iid = do
    pure $ toModifiers attrs [SkillModifier #combat (-1), HealthModifier (-1), AddTrait Serpent]
  getModifiersFor _ _ = pure []

instance HasAbilities CurseOfYig where
  getAbilities (CurseOfYig a) = [restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage CurseOfYig where
  runMessage msg t@(CurseOfYig attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getVengeanceInVictoryDisplay
      push $ beginSkillTest iid (attrs.ability 1) iid #willpower (Fixed $ 2 + n)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CurseOfYig <$> runMessage msg attrs
