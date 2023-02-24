module Arkham.Treachery.Cards.ATearInTime
  ( aTearInTime
  , ATearInTime(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (InvestigatorRemainingActions) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ATearInTime = ATearInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInTime :: TreacheryCard ATearInTime
aTearInTime = treachery ATearInTime Cards.aTearInTime

instance RunMessage ATearInTime where
  runMessage msg t@(ATearInTime attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillWillpower 3
      pure t
    FailedSkillTest iid maction source target@SkillTestInitiatorTarget{} sType n
      | isSource attrs source && n > 0 -> do
        hasRemainingActions <- fieldP InvestigatorRemainingActions (> 0) iid
        pushAll
          $ chooseOrRunOne
              iid
              ([ Label "Lose 1 Action" [LoseActions iid source 1]
               | hasRemainingActions
               ]
              <> [ Label
                     "Take 1 Horror"
                     [InvestigatorAssignDamage iid source DamageAny 0 1]
                 ]
              )
          : [FailedSkillTest iid maction source target sType (n - 1)]
        pure t
    _ -> ATearInTime <$> runMessage msg attrs
