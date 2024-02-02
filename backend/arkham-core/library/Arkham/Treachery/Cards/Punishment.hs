module Arkham.Treachery.Cards.Punishment (
  punishment,
  Punishment (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Punishment = Punishment TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

punishment :: TreacheryCard Punishment
punishment = treachery Punishment Cards.punishment

instance HasModifiersFor Punishment where
  getModifiersFor (InvestigatorTarget iid) (Punishment attrs) = do
    mModifiers <- runMaybeT $ do
      source <- MaybeT getSkillTestSource
      investigator <- MaybeT getSkillTestInvestigator
      guard $ isSource attrs source && iid == investigator
      guardM
        . lift
        . selectAny
        $ ExhaustedEnemy
        <> EnemyWithTrait Witch
        <> EnemyAt (locationWithInvestigator iid)
      pure SkillTestAutomaticallySucceeds

    pure $ toModifiers attrs $ maybeToList mModifiers
  getModifiersFor _ _ = pure []

instance HasAbilities Punishment where
  getAbilities (Punishment a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ EnemyDefeated #after Anyone ByAny AnyEnemy
    ]

instance RunMessage Punishment where
  runMessage msg t@(Punishment attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ assignDamage iid attrs 1
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) attrs #willpower 3
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid attrs attrs
      pure t
    _ -> Punishment <$> runMessage msg attrs
