module Arkham.Location.Cards.Montmartre209 (montmartre209, montmartre209Effect, Montmartre209 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype Montmartre209 = Montmartre209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: LocationCard Montmartre209
montmartre209 = location Montmartre209 Cards.montmartre209 3 (PerPlayer 1)

instance HasAbilities Montmartre209 where
  getAbilities (Montmartre209 attrs) =
    withRevealedAbilities
      attrs
      [limitedAbility (GroupLimit PerRound 1) $ restrictedAbility attrs 1 Here actionAbility]

instance RunMessage Montmartre209 where
  runMessage msg a@(Montmartre209 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ createCardEffect Cards.montmartre209 Nothing (attrs.ability 1) iid
      pure a
    _ -> Montmartre209 <$> runMessage msg attrs

newtype Montmartre209Effect = Montmartre209Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209Effect :: EffectArgs -> Montmartre209Effect
montmartre209Effect = cardEffect Montmartre209Effect Cards.montmartre209

instance HasModifiersFor Montmartre209Effect where
  getModifiersFor (InvestigatorTarget _) (Montmartre209Effect a) =
    pure $ toModifiers a [TopCardOfDeckIsRevealed, CanPlayTopOfDeck AnyCard]
  getModifiersFor _ _ = pure []

instance RunMessage Montmartre209Effect where
  runMessage msg e@(Montmartre209Effect attrs) = case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid) | eid == effectId attrs -> do
      cards <-
        filterM
          ( getIsPlayable
              iid
              source
              (UnpaidCost NoAction)
              [mkWhen (Window.DuringTurn iid)]
          )
          =<< select (TopOfDeckOf UneliminatedInvestigator)
      player <- getPlayer iid
      pushAll
        [ chooseOne player
            $ Label "Play no cards" []
            : [ targetLabel
                (toCardId card)
                [InitiatePlayCard iid card Nothing NoPayment (Window.defaultWindows iid) False]
              | card <- cards
              ]
        , DisableEffect eid
        ]
      pure e
    _ -> Montmartre209Effect <$> runMessage msg attrs
