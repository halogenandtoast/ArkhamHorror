module Arkham.Treachery.Cards.AcridMiasma
  ( acridMiasma
  , AcridMiasma(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype AcridMiasma = AcridMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acridMiasma :: TreacheryCard AcridMiasma
acridMiasma = treachery AcridMiasma Cards.acridMiasma

instance HasAbilities AcridMiasma where
  getAbilities (AcridMiasma attrs) = case treacheryAttachedTarget attrs of
    Just (LocationTarget lid) ->
      [ mkAbility attrs 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId lid
      ]
    _ -> []

instance RunMessage AcridMiasma where
  runMessage msg t@(AcridMiasma attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      targetLocations <-
        selectList
        $ NearestLocationToYou
        $ LocationWithoutTreachery
        $ treacheryIs Cards.acridMiasma
      case targetLocations of
        [] -> pure t
        (x : _) -> t <$ push (AttachTreachery (toId attrs) (LocationTarget x))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (RevelationSkillTest iid (toSource attrs) SkillWillpower 2)
      -- not revelation but puts card into active
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        hunters <- selectList HunterEnemy
        let damageChoice = InvestigatorAssignDamage iid source DamageAny 1 1
        case hunters of
          [] -> push damageChoice
          eids -> push $ chooseOne
            iid
            [ Label "Take 1 damage and 1 horror" [damageChoice]
            , Label
              "Resolve the hunter keyword on each enemy in play"
              [ HunterMove eid | eid <- eids ]
            ]
        pure t
    _ -> AcridMiasma <$> runMessage msg attrs
