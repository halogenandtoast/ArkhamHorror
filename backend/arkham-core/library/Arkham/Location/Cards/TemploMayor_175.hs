module Arkham.Location.Cards.TemploMayor_175 (
  temploMayor_175,
  TemploMayor_175 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TemploMayor_175 = TemploMayor_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_175 :: LocationCard TemploMayor_175
temploMayor_175 =
  locationWith
    TemploMayor_175
    Cards.temploMayor_175
    2
    (PerPlayer 2)
    (labelL .~ "circle")

instance HasAbilities TemploMayor_175 where
  getAbilities (TemploMayor_175 attrs) =
    withRevealedAbilities attrs $
      [ mkAbility attrs 1 $
          ForcedAbility $
            PutLocationIntoPlay Timing.After Anyone $
              LocationWithId $
                toId attrs
      , limitedAbility (GroupLimit PerPhase 1)
          $ restrictedAbility
            attrs
            2
            ( CluesOnThis (AtLeast $ Static 1)
                <> CanDiscoverCluesAt (LocationWithId $ toId attrs)
            )
          $ ActionAbility Nothing
          $ ActionCost 1
            <> ShuffleDiscardCost 1 WeaknessCard
      ]

instance RunMessage TemploMayor_175 where
  runMessage msg l@(TemploMayor_175 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ ShuffleDiscardBackIn iid
        , DiscardUntilFirst iid (toSource attrs) (Deck.InvestigatorDeck iid) (BasicCardMatch WeaknessCard)
        ]
      pure l
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ push . addToHand iid . PlayerCard
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 2) 2 Nothing
      pure l
    _ -> TemploMayor_175 <$> runMessage msg attrs
