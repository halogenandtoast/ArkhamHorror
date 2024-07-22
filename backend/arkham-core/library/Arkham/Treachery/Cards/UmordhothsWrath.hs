module Arkham.Treachery.Cards.UmordhothsWrath (umordhothsWrath, UmordhothsWrath (..)) where

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
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
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ HandlePointOfFailure iid (toTarget attrs) n
      pure t
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      player <- getPlayer iid
      pushAll
        $ if hasCards
          then
            [ chooseOne player
                $ [ Label "Discard a card from your hand"
                      $ [toMessage $ chooseAndDiscardCard iid attrs]
                  , Label "Take 1 damage and 1 horror"
                      $ [assignDamageAndHorror iid attrs 1 1]
                  ]
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
          else
            [ assignDamageAndHorror iid attrs 1 1
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
      pure t
    _ -> UmordhothsWrath <$> runMessage msg attrs
