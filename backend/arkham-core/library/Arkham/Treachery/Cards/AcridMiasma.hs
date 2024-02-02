module Arkham.Treachery.Cards.AcridMiasma (
  acridMiasma,
  AcridMiasma (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AcridMiasma = AcridMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

acridMiasma :: TreacheryCard AcridMiasma
acridMiasma = treachery AcridMiasma Cards.acridMiasma

instance HasAbilities AcridMiasma where
  getAbilities (AcridMiasma attrs) = case treacheryAttachedTarget attrs of
    Just (LocationTarget lid) -> [mkAbility attrs 1 $ ForcedAbility $ Enters #after You $ LocationWithId lid]
    _ -> []

instance RunMessage AcridMiasma where
  runMessage msg t@(AcridMiasma attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      mLocation <- selectOne $ NearestLocationToYou $ locationWithoutTreachery Cards.acridMiasma
      for_ mLocation $ push . AttachTreachery (toId attrs) . toTarget
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #willpower 2
      pure t
    -- not revelation but puts card into active
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      moveHunters <- selectListMap HunterMove HunterEnemy
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Take 1 damage and 1 horror" [assignDamageAndHorror iid attrs 1 1]
        : [Label "Resolve the hunter keyword on each enemy in play" moveHunters | notNull moveHunters]
      pure t
    _ -> AcridMiasma <$> runMessage msg attrs
