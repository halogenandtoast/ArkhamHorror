module Arkham.Treachery.Cards.UmordhothsWrath
  ( umordhothsWrath
  , UmordhothsWrath(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryCard UmordhothsWrath
umordhothsWrath = treachery UmordhothsWrath Cards.umordhothsWrath

instance TreacheryRunner env => RunMessage env UmordhothsWrath where
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
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' > 0
        then t <$ pushAll
          [ chooseOne
            iid
            [ Label "Discard a card from your hand" [RandomDiscard iid]
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
