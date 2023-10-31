module Arkham.Event.Cards.AstralTravel (
  astralTravel,
  AstralTravel (..),
) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (MoveAction)
import Arkham.RequestedChaosTokenStrategy
import Arkham.Trait qualified as Trait
import Arkham.Window qualified as Window

newtype AstralTravel = AstralTravel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astralTravel :: EventCard AstralTravel
astralTravel = event AstralTravel Cards.astralTravel

instance RunMessage AstralTravel where
  runMessage msg e@(AstralTravel attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locations <- selectList $ RevealedLocation <> Unblocked <> NotYourLocation <> canEnterLocation iid
      player <- getPlayer iid
      pushAll
        [ chooseOne player [targetLabel lid [MoveAction iid lid Free False] | lid <- locations]
        , RequestChaosTokens (toSource attrs) Nothing (Reveal 1) SetAside
        ]
      pure e
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      let faces = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when (any ((`elem` faces) . chaosTokenFace) tokens) $ do
        targets <- selectList $ oneOf (AssetWithTrait <$> [Trait.Item, Trait.Ally])
        player <- getPlayer (eventOwner attrs)
        push
          $ If (Window.RevealChaosTokenEventEffect (eventOwner attrs) tokens (toId attrs))
          $ case targets of
            [] -> [assignDamage (eventOwner attrs) source 1]
            xs ->
              [ chooseOne
                  player
                  [ targetLabel x [toDiscardBy (eventController attrs) attrs x]
                  | x <- xs
                  ]
              ]
      pure e
    _ -> AstralTravel <$> runMessage msg attrs
