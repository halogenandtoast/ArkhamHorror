module Arkham.Location.Cards.Southside_294 (
  southside_294,
  Southside_294 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (Power))

newtype Southside_294 = Southside_294 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_294 :: LocationCard Southside_294
southside_294 = location Southside_294 Cards.southside_294 1 (Static 0)

instance HasAbilities Southside_294 where
  getAbilities (Southside_294 attrs) =
    let breachCount = countLocationBreaches attrs
     in withRevealedAbilities
          attrs
          [fastAbility attrs 1 Free (Here <> mwhen (breachCount > 0) EncounterDeckIsNotEmpty)]

instance RunMessage Southside_294 where
  runMessage msg l@(Southside_294 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toAbilitySource attrs 1) (Just $ toTarget attrs)
      pure l
    DiscardedTopOfEncounterDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      let powerTreacheries = filter ((`cardMatch` CardWithTrait Power) . toCard) cards
      act <- selectJust AnyAct
      pushAll
        $ [RemoveBreaches (toTarget attrs) 1, PlaceBreaches (toTarget act) 1]
          <> ( guard (notNull powerTreacheries)
                *> [ FocusCards (toCard <$> cards)
                   , chooseOrRunOne
                      iid
                      [targetLabel (toCardId card) [UnfocusCards, InvestigatorDrewEncounterCard iid card] | card <- cards]
                   ]
             )
      pure l
    _ -> Southside_294 <$> runMessage msg attrs
