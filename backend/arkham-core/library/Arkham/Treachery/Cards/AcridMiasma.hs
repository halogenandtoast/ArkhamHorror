module Arkham.Treachery.Cards.AcridMiasma (
  acridMiasma,
  AcridMiasma (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AcridMiasma = AcridMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acridMiasma :: TreacheryCard AcridMiasma
acridMiasma = treachery AcridMiasma Cards.acridMiasma

instance HasAbilities AcridMiasma where
  getAbilities (AcridMiasma attrs) = case treacheryAttachedTarget attrs of
    Just (LocationTarget lid) ->
      [ mkAbility attrs 1 $
          ForcedAbility $
            Enters Timing.After You $
              LocationWithId lid
      ]
    _ -> []

instance RunMessage AcridMiasma where
  runMessage msg t@(AcridMiasma attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      mLocation <-
        selectOne $
          NearestLocationToYou $
            locationWithoutTreachery
              Cards.acridMiasma
      for_ mLocation $
        \x -> push $ AttachTreachery (toId attrs) (LocationTarget x)
      pure t
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          t <$ push (RevelationSkillTest iid (toSource attrs) SkillWillpower 2)
    -- not revelation but puts card into active
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          moveHunters <- selectListMap HunterMove HunterEnemy
          let dmgChoice = [InvestigatorAssignDamage iid source DamageAny 1 1]
          push $
            chooseOrRunOne iid $
              Label "Take 1 damage and 1 horror" dmgChoice
                : [ Label
                    "Resolve the hunter keyword on each enemy in play"
                    moveHunters
                  | notNull moveHunters
                  ]
          pure t
    _ -> AcridMiasma <$> runMessage msg attrs
