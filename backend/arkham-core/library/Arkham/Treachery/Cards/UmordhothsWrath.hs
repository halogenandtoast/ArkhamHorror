module Arkham.Treachery.Cards.UmordhothsWrath
  ( umordhothsWrath
  , UmordhothsWrath(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryCard UmordhothsWrath
umordhothsWrath = treachery UmordhothsWrath Cards.umordhothsWrath

instance RunMessage UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        5
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t
      <$ push (HandlePointOfFailure iid (toTarget attrs) n)
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      if hasCards
        then t <$ pushAll
          [ chooseOne
            iid
            [ Label "Discard a card from your hand" [RandomDiscard iid (toSource attrs) AnyCard]
            , Label
              "Take 1 damage and 1 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1]
            ]
          , HandlePointOfFailure iid (toTarget attrs) (n - 1)
          ]
        else t <$ pushAll
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
          , HandlePointOfFailure iid (toTarget attrs) (n - 1)
          ]
    _ -> UmordhothsWrath <$> runMessage msg attrs
