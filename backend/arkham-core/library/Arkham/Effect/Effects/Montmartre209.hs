module Arkham.Effect.Effects.Montmartre209
  ( Montmartre209(..)
  , montmartre209
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype Montmartre209 = Montmartre209 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: EffectArgs -> Montmartre209
montmartre209 = Montmartre209 . uncurry4 (baseAttrs "03209")

instance HasModifiersFor env Montmartre209 where
  getModifiersFor _ (InvestigatorTarget _) (Montmartre209 a) =
    pure $ toModifiers a [TopCardOfDeckIsRevealed, CanPlayTopOfDeck AnyCard]
  getModifiersFor _ _ _ = pure []

instance (CanCheckPlayable env, HasQueue env) => RunMessage Montmartre209 where
  runMessage msg e@(Montmartre209 attrs) = case msg of
    CreatedEffect eid _ source (InvestigatorTarget iid) | eid == effectId attrs -> do
      cards <-
        fmap (fmap toCardId)
        . filterM
            (getIsPlayable
              iid
              source
              UnpaidCost
              [Window Timing.When (Window.DuringTurn iid)]
            )
        =<< selectList (TopOfDeckOf Anyone)
      pushAll
        [ chooseOne iid
        $ Label "Play no cards" []
        : [ InitiatePlayCard iid card Nothing False | card <- cards ]
        , DisableEffect eid
        ]
      pure e
    _ -> Montmartre209 <$> runMessage msg attrs
