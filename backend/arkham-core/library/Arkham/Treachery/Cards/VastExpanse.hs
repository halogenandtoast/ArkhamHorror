module Arkham.Treachery.Cards.VastExpanse (
  vastExpanse,
  VastExpanse (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VastExpanse = VastExpanse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

vastExpanse :: TreacheryCard VastExpanse
vastExpanse = treachery VastExpanse Cards.vastExpanse

instance RunMessage VastExpanse where
  runMessage msg t@(VastExpanse attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      push
        $ if extradimensionalCount == 0
          then gainSurge attrs
          else
            beginSkillTest
              iid
              source
              (toTarget iid)
              SkillWillpower
              (min 5 extradimensionalCount)
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source ->
          t
            <$ push (InvestigatorAssignDamage iid source DamageAny 0 n)
    _ -> VastExpanse <$> runMessage msg attrs
