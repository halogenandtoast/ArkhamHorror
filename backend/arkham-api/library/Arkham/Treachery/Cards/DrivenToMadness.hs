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
import Arkham.Placement
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (EnemyEvaded)

newtype DrivenToMadness = DrivenToMadness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drivenToMadness :: TreacheryCard DrivenToMadness
drivenToMadness = treachery DrivenToMadness Cards.drivenToMadness

instance HasModifiersFor DrivenToMadness where
  getModifiersFor (DrivenToMadness attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      enemy <-
        modified_ attrs eid [EnemyFight 1, HealthModifier 1, EnemyEvade 1, RemoveKeyword Keyword.Aloof]
      investigators <- modifySelect attrs Anyone [CannotParleyWith $ EnemyWithId eid]
      pure $ enemy <> investigators
    _ -> pure mempty

instance HasAbilities DrivenToMadness where
  getAbilities (DrivenToMadness attrs) = case treacheryAttachedTarget attrs of
    Just (EnemyTarget eid) -> [mkAbility attrs 1 $ ForcedAbility $ EnemyEvaded #after You $ EnemyWithId eid]
    _ -> []

instance RunMessage DrivenToMadness where
  runMessage msg t@(DrivenToMadness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      humanoids <- select $ NearestEnemyTo iid $ EnemyWithTrait Humanoid
      if null humanoids
        then push $ gainSurge attrs
        else do
          player <- getPlayer iid
          push $ chooseOrRunOne player $ targetLabels humanoids $ only . attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> DrivenToMadness <$> runMessage msg attrs
