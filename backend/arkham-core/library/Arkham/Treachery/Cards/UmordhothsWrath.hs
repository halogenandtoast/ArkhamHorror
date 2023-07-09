module Arkham.Treachery.Cards.UmordhothsWrath (
  umordhothsWrath,
  UmordhothsWrath (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryCard UmordhothsWrath
umordhothsWrath = treachery UmordhothsWrath Cards.umordhothsWrath

instance RunMessage UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ beginSkillTest iid source iid SkillWillpower 5
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source -> do
          push (HandlePointOfFailure iid (toTarget attrs) n)
          pure t
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      pushAll $
        if hasCards
          then
            [ chooseOne
                iid
                [ Label
                    "Discard a card from your hand"
                    [toMessage $ chooseAndDiscardCard iid attrs]
                , Label
                    "Take 1 damage and 1 horror"
                    [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
                ]
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
          else
            [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
      pure t
    _ -> UmordhothsWrath <$> runMessage msg attrs
