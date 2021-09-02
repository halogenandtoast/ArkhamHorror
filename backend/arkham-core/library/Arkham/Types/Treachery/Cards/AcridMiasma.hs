module Arkham.Types.Treachery.Cards.AcridMiasma
  ( acridMiasma
  , AcridMiasma(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AcridMiasma = AcridMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
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

instance
  ( HasSet ClosestLocationId env (InvestigatorId, LocationMatcher)
  , HasSet EnemyId env EnemyMatcher
  , TreacheryRunner env
  )
  => RunMessage env AcridMiasma where
  runMessage msg t@(AcridMiasma attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targetLocations <- map unClosestLocationId <$> getSetList
        (iid, LocationWithoutTreachery $ toCardCode Cards.acridMiasma)
      case targetLocations of
        [] -> t <$ push (Discard $ toTarget attrs)
        (x : _) -> t <$ push (AttachTreachery (toId attrs) (LocationTarget x))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (RevelationSkillTest iid (toSource attrs) SkillWillpower 2)
      -- not revelation but puts card into active
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        hunters <- getSetList HunterEnemy
        let damageChoice = InvestigatorAssignDamage iid source DamageAny 1 1
        case hunters of
          [] -> t <$ push damageChoice
          eids -> t <$ push
            (chooseOne
              iid
              [ Label "Take 1 damage and 1 horror" [damageChoice]
              , Label
                "Resolve the hunter keyword on each enemy in play"
                [ HunterMove eid | eid <- eids ]
              ]
            )
    _ -> AcridMiasma <$> runMessage msg attrs
