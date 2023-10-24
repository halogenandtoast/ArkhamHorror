module Arkham.Treachery.Cards.DrivenToMadness (
  drivenToMadness,
  DrivenToMadness (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message hiding (EnemyEvaded)
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (EnemyEvaded)

newtype DrivenToMadness = DrivenToMadness TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drivenToMadness :: TreacheryCard DrivenToMadness
drivenToMadness = treachery DrivenToMadness Cards.drivenToMadness

instance HasModifiersFor DrivenToMadness where
  getModifiersFor target (DrivenToMadness attrs) | target `elem` treacheryAttachedTarget attrs = do
    pure $ toModifiers attrs [EnemyFight 1, HealthModifier 1, EnemyEvade 1, RemoveKeyword Keyword.Aloof]
  getModifiersFor (InvestigatorTarget _) (DrivenToMadness attrs) = do
    case treacheryAttachedTarget attrs of
      Just (EnemyTarget eid) -> pure $ toModifiers attrs [CannotParleyWith $ EnemyWithId eid]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities DrivenToMadness where
  getAbilities (DrivenToMadness attrs) = case treacheryAttachedTarget attrs of
    Just (EnemyTarget eid) -> [mkAbility attrs 1 $ ForcedAbility $ EnemyEvaded #after You $ EnemyWithId eid]
    _ -> []

instance RunMessage DrivenToMadness where
  runMessage msg t@(DrivenToMadness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      humanoids <- selectList $ NearestEnemyTo iid $ EnemyWithTrait Humanoid
      if null humanoids
        then push $ gainSurge attrs
        else do
          player <- getPlayer iid
          push $ chooseOrRunOne player $ targetLabels humanoids $ only . attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> DrivenToMadness <$> runMessage msg attrs
