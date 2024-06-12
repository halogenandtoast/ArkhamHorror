module Arkham.Location.Cards.TemploMayor_175 (temploMayor_175, TemploMayor_175 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype TemploMayor_175 = TemploMayor_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_175 :: LocationCard TemploMayor_175
temploMayor_175 = locationWith TemploMayor_175 Cards.temploMayor_175 2 (PerPlayer 2) (labelL .~ "circle")

instance HasAbilities TemploMayor_175 where
  getAbilities (TemploMayor_175 attrs) =
    extendRevealed attrs
      $ [ mkAbility attrs 1 $ forced $ PutLocationIntoPlay #after Anyone (be attrs)
        , groupLimit PerPhase
            $ restrictedAbility attrs 2 (CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
            $ actionAbilityWithCost (ShuffleDiscardCost 1 WeaknessCard)
        ]

instance RunMessage TemploMayor_175 where
  runMessage msg l@(TemploMayor_175 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ ShuffleDiscardBackIn iid
        , DiscardUntilFirst iid (attrs.ability 1) (Deck.InvestigatorDeck iid) (basic WeaknessCard)
        ]
      pure l
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ push . addToHand iid . PlayerCard
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 2) 2
      pure l
    _ -> TemploMayor_175 <$> runMessage msg attrs
