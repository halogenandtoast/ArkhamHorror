module Arkham.Treachery.Cards.SelfCentered (
  selfCentered,
  SelfCentered (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SelfCentered = SelfCentered TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

selfCentered :: TreacheryCard SelfCentered
selfCentered = treachery SelfCentered Cards.selfCentered

instance HasModifiersFor SelfCentered where
  getModifiersFor (InvestigatorTarget iid) (SelfCentered attrs) | treacheryOnInvestigator iid attrs = do
    pure
      $ toModifiers
        attrs
        [CannotCommitToOtherInvestigatorsSkillTests, CannotAffectOtherPlayersWithPlayerEffectsExceptDamage]
  getModifiersFor _ _ = pure []

instance HasAbilities SelfCentered where
  getAbilities (SelfCentered a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage SelfCentered where
  runMessage msg t@(SelfCentered attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> SelfCentered <$> runMessage msg attrs
