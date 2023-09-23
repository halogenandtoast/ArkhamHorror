module Arkham.Effect.Effects.Montmartre209 (
  Montmartre209 (..),
  montmartre209,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype Montmartre209 = Montmartre209 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: EffectArgs -> Montmartre209
montmartre209 = Montmartre209 . uncurry4 (baseAttrs "03209")

instance HasModifiersFor Montmartre209 where
  getModifiersFor (InvestigatorTarget _) (Montmartre209 a) =
    pure $ toModifiers a [TopCardOfDeckIsRevealed, CanPlayTopOfDeck AnyCard]
  getModifiersFor _ _ = pure []

instance RunMessage Montmartre209 where
  runMessage msg e@(Montmartre209 attrs) = case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid)
      | eid == effectId attrs -> do
          cards <-
            filterM
              ( getIsPlayable
                  iid
                  source
                  UnpaidCost
                  [mkWindow Timing.When (Window.DuringTurn iid)]
              )
              =<< selectList (TopOfDeckOf UneliminatedInvestigator)
          pushAll
            [ chooseOne iid
                $ Label "Play no cards" []
                : [ TargetLabel
                    (CardIdTarget $ toCardId card)
                    [ InitiatePlayCard
                        iid
                        card
                        Nothing
                        (Window.defaultWindows iid)
                        False
                    ]
                  | card <- cards
                  ]
            , DisableEffect eid
            ]
          pure e
    _ -> Montmartre209 <$> runMessage msg attrs
